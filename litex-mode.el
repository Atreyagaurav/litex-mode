;;; litex-mode.el --- Minor mode for converting lisp to LaTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author: Gaurav Atreya <allmanpride@gmail.com>
;; URL: https://github.com/Atreyagaurav/litex-mode
;; Version: 0.1
;; Keywords: calculator, lisp, LaTeX
;; Package-Requires: ((cl-lib "0.5") (emacs "24.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; converts valid mathematical Lisp expressions to LaTeX snippets
;; improved from https://emacs.stackexchange.com/a/70360
;; modified with help from https://gist.github.com/bpanthi977/4b8ece0eeff3bc05bb82275a23cbb56d

;; For detailed help visit github page:  https://github.com/Atreyagaurav/litex-mode

;;; Code:
(eval-when-compile (require 'pcase))
(require 'cl-lib)


;; lisp functions that lave their own latex commands
(defvar litex-latex-functions '(sin cos tan))

;; enclose latex converted to paran
(defvar litex-latex-maybe-enclose? nil)

;; format string to be used by floats, and their limits
(defvar litex-format-float-string "%.3f")
(defvar litex-format-float-upper-limit 1e4)
(defvar litex-format-float-lower-limit 1e-2)

;; used for joing strings in steps of solution
(defvar litex-steps-join-string "= ")
(defvar litex-steps-end-string " ")

;; math inlin environment to use.
(defvar litex-math-inline-start "\\(")
(defvar litex-math-inline-end "\\)")

;; math equation environment to use and strings to join the solutions.
(defvar litex-math-equation-start "\\begin{equation}\n")
(defvar litex-math-equation-end "\n\\end{equation}\n")
(defvar litex-math-steps-equation-join-string "= ")
(defvar litex-math-steps-equation-end-string " ")

;; math eqnarray environment to use and strings to join the solutions.
(defvar litex-math-eqnarray-start "\\begin{eqnarray*}\n")
(defvar litex-math-eqnarray-end "\n\\end{eqnarray*}\n")
(defvar litex-math-steps-eqnarray-join-string " &=& ")
(defvar litex-math-steps-eqnarray-end-string "\\\\\n")


(defun litex-format-float (val)
  "Function that defines how float VAL is formatted in lisp2latex."
  (if (or (< val litex-format-float-lower-limit) (> val litex-format-float-upper-limit))
      (let* ((exponent (floor (log val 10)))
             (front (/ val (expt 10 exponent))))
        (format (concat litex-format-float-string " \\times 10^{%d}") front exponent))
    (format litex-format-float-string val)))


(defun litex-latex-maybe-enclose (form)
  "Encloses FORM in parantheis if LITEX-LATEX-MAYBE-ENCLOSE is true."
  (let* ((litex-latex-maybe-enclose? nil)
         (latex (litex-lisp2latex-all form)))
    (if litex-latex-maybe-enclose?
        (format "(%s)" latex)
      latex)))


(defun litex-lisp2latex-all (form)
  "Convert given Lisp expression FORM to latex equivalent string."
  (pcase form
    ;; basic operators
    (`(+ . ,args)
     (setf litex-latex-maybe-enclose t)
     (mapconcat #'litex-lisp2latex-all args " + "))

    (`(* . ,args)
     (setf latex-maybe-enclose t)
     (with-output-to-string
       (cl-loop for (me next . rest) on args do
		(if (numberp next)
                    (princ (format "%s \\times " (litex-latex-maybe-enclose me)))
		  (princ (format "%s " (litex-latex-maybe-enclose me)))))))

    (`(/ ,a1 . ,args)
     (if args
         (format "\\frac{%s}{%s}" (litex-lisp2latex-all a1)
                 (litex-lisp2latex-all (cons '* args)))
       (format "\\frac1{%s}" (litex-lisp2latex-all a1))))

    (`(- ,a1 . ,args)
     (if args
         (format "%s - %s" (litex-lisp2latex-all a1)
                 (mapconcat #'litex-lisp2latex-all args " - "))
       (format "- %s" (litex-lisp2latex-all a1))))

    (`(expt ,base ,power)
     (if (listp base)
         (format "(%s)^{%s}" (litex-lisp2latex-all base) (litex-lisp2latex-all power))
       (format "%s^{%s}"  (litex-lisp2latex-all base) (litex-lisp2latex-all power))))

    ;; assignment operator
    (`(setq . ,args)
     (with-output-to-string
       (cl-loop for (a b . rest) on args by #'cddr do
		(princ (format "%s = %s" (litex-lisp2latex-all a) (litex-lisp2latex-all b)))
		(when rest (princ "; ")))))

    ;; other operators
    (`(1+ ,arg) (concat "1 + " (litex-lisp2latex-all arg) ))
    (`(sqrt ,arg) (format "\\sqrt{%s}" (litex-lisp2latex-all arg)))


    ;; Function definition:
    ;; TODO: something like func(a,b,c): a+b+c


    ;; named functions
    (`(,func . ,args)
     (let* ((known? (cl-find func litex-latex-functions))
            (enclose? (or (not known?)
                          (> (length args) 1)
                          (listp (cl-first args))))
            (format-string (concat (if known? "\\%s" "\\mathrm{%s}")
                                   (if enclose?  "(%s)" " %s"))))
       (format format-string func (mapconcat #'litex-lisp2latex-all args ","))))

    (_
     (cond ((floatp form)
            (litex-format-float form))
           (t (prin1-to-string form))))))



(defun litex-contains-variables (expression)
  "Check if given EXPRESSION has variables."
  (if (functionp expression)
      nil
    (if (symbolp expression)
	t
      (if (consp expression)
	  (cl-some #'litex-contains-variables expression)
	nil))))


(defun litex-substitute-values (expression)
  "Gives a string from EXPRESSION substituting the values."
  (if (functionp expression)
      (format "%s" expression)
    (if (symbolp expression)
	(format "%s" (eval expression))
      (if (consp expression)
	  (format "(%s)"
		  (mapconcat #'litex-substitute-values expression " "))
	(prin1-to-string expression)))))


(defun litex-solve-single-step (form)
  "Solves a single step of calculation in FORM."
  (cond ((listp form)
         (if (cl-every #'numberp (cl-rest form))
             (eval form)
           (cons (cl-first form) (mapcar #'litex-solve-single-step (cl-rest form)))))
	((functionp form)
	 form)

        ((symbolp form)
         (symbol-value form))

        (t form)))


(defun litex-solve-all-steps (form)
  "Solves all the steps of calculations in FORM expression and retuns a list of steps."
  (let
      ((solution (list form))) ;given expression
    (if
	(litex-contains-variables form)
	(setq solution
	      (append solution
		      (list
		       (setq form
			     (read
			      (litex-substitute-values form)))))))

    (while (consp form)
      (setq solution
	    (append solution
		    (list (setq form
				(litex-solve-single-step form))))))
    solution))


(defun litex-sexp-to-solved-string (expression format-func)
  "Return solved string of EXPRESSION using FORMAT-FUNC to format all steps."
  (pcase expression
    (`(setq ,var ,exp)
     (concat
      (format (concat "%s" litex-steps-join-string) var)
      (mapconcat format-func (litex-solve-all-steps exp)
		 (concat litex-steps-end-string litex-steps-join-string))))
    (_ (mapconcat format-func (litex-solve-all-steps expression)
		  (concat litex-steps-end-string litex-steps-join-string)))))


(defun litex-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun litex-eval-and-insert ()
  "Insert the evaulation result of the preceding sexp."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (insert (concat (current-kill 0)
		      litex-steps-join-string
		      (format "%s"
			      (eval (read (current-kill 0))))))
    (error (message "Invalid expression"))))


(defun litex-exp-to-latex (beg end)
  "Convert exponentials in the given region to latex notation.
Argument BEG begining position of region.
Argument END end position of region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward
	    "\\([0-9.+-]+\\)e\\([0-9.+-]+\\)" beg)
      (replace-match "\\1 \\\\times 10^{\\2}"))))


(defun litex-exp-in-latex-math (beg end)
  "Insert the selected expression inside latex inline math environment.
Argument BEG begining position of region.
Argument END end position of region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (insert litex-math-inline-start)
    (goto-char (+ (point) (- end beg)))
    (insert litex-math-inline-end)))


(defun litex-sexp-to-latex-exp ()
  "Convert valid sexp to latex expressions."
  (interactive)
  (backward-kill-sexp)
  (insert (litex-lisp2latex-all (read (current-kill 0)))))


(defun litex-sexp-replace-variables ()
  "Replace the variable values in the last sexp at point."
  (interactive)
  (backward-kill-sexp)
  (insert (litex-substitute-values (read (current-kill 0)))))


(defun litex-sexp-solve-all-steps ()
  "Solve last sexp at point in steps and insert those steps."
  (interactive)
  (backward-kill-sexp)
  (let ((expression (read (current-kill 0))))
    (insert
     (litex-sexp-to-solved-string expression #'prin1-to-string))))


(defun litex-sexp-solve-all-steps-equation ()
  "Solve last sexp in steps and insert it in LaTeX equation environment."
  (interactive)
  (backward-kill-sexp)
  (let ((expression (read (current-kill 0)))
	(litex-steps-join-string litex-math-steps-equation-join-string)
	(litex-steps-end-string litex-math-steps-equation-end-string))
    (insert litex-math-equation-start)
    (insert
     (litex-sexp-to-solved-string expression #'litex-lisp2latex-all))
    (insert litex-math-equation-end)))


(defun litex-sexp-solve-all-steps-eqnarray ()
  "Solve last sexp in steps and insert it in LaTeX eqnarray environment."
  (interactive)
  (backward-kill-sexp)
  (let ((expression (read (current-kill 0)))
	(litex-steps-join-string litex-math-steps-eqnarray-join-string)
	(litex-steps-end-string litex-math-steps-eqnarray-end-string))
    (insert litex-math-eqnarray-start)
    (insert
     (litex-sexp-to-solved-string expression #'litex-lisp2latex-all))
    (insert litex-math-eqnarray-end)))


(defun litex-format-region-last (beg end)
  "Format selected region as per format of last call to `litex-format-region`,BEG and END are region bounds."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'sexp)))
		   (list (cl-first bnd) (cl-rest bnd)))))
  (let ((text (buffer-substring-no-properties beg end)))
    ;; maybe I should make it eval if given expression
    (if (string-match-p "%[0-9.]*[dfex]" litex-format-float-string)
	(setq text (eval (read text))))
    (kill-region beg end)
    (insert (format litex-format-float-string text))))


(defun litex-format-region (beg end)
  "Format selected region as per input format, BEG and END are region bounds."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'symbol)))
		   (list (cl-first bnd) (cl-rest bnd)))))
  (let ((fmt  (read-string "Enter format string:"
			   litex-format-float-string)))
    (setq litex-format-float-string fmt)
    (litex-format-region-last beg end)))


(defun litex-increment-number (step)
  "Increase the number value by STEP."
  (interactive "P")
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (word (buffer-substring-no-properties
		(car bounds)
		(cdr bounds)))
	 (step (or step 1)))
    (when bounds
      (let ((ma (string-match
		 "\\(.*[^0-9]\\)\\([0-9]+\\)\\([/_.,-\"']\\)?"
		 word)))
	(when ma
	  (delete-region (car bounds) (cdr bounds))
	  (insert (concat
		   (match-string 1 word)
		   (number-to-string (+ step (string-to-number
					      (match-string 2 word))))
		   (match-string 3 word))))))))



(defun litex-insert-or-replace-x (beg end)
  "If a region (BEG to END) is selected, replace * by \times otherwise insert \times instead of ×."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (goto-char end)
  (if (= beg end)
      (insert "\\times")
    (when (re-search-backward "*" beg)
      (replace-match "\\\\times"))))


(setq litex-key-map (make-sparse-keymap))
(define-key litex-key-map (kbd "F") 'litex-format-region)
(define-key litex-key-map (kbd "f") 'litex-format-region-last)
(define-key litex-key-map (kbd "E") 'litex-eval-and-replace)
(define-key litex-key-map (kbd "e") 'litex-eval-and-insert)
(define-key litex-key-map (kbd "s") 'litex-sexp-to-latex-exp)
(define-key litex-key-map (kbd "S") 'litex-sexp-solve-all-steps)
(define-key litex-key-map (kbd "+") 'litex-increment-number)
(define-key litex-key-map (kbd "l") 'litex-exp-to-latex)
(define-key litex-key-map (kbd "m") 'litex-exp-in-latex-math)
(define-key litex-key-map (kbd "A") 'litex-sexp-solve-all-steps-equation)
(define-key litex-key-map (kbd "a") 'litex-sexp-solve-all-steps-eqnarray)


(local-set-key "×" 'litex-insert-or-replace-x)


(define-minor-mode litex-mode
  "Minor mode for Calculations on lisp, and formatting on LaTeX."
  :lighter " LiTeX")


(provide 'litex-mode)

;;; litex-mode.el ends here
