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


(defvar litex-latex-functions '(sin cos tan)
  "Lisp functions that have their own latex commands")
(defvar litex-make-hyphenated-to-subscript t
  "Whether to make the hyphenated variables subscript or not")
(defvar litex-latex-maybe-enclose? nil
  "Enclose latex converted to paran if needed")

(defvar litex-format-float-string "%.3f"
  "Format string to be used by floats")
(defvar litex-format-float-upper-limit 1e4
  "Upper limit of what number is formatted as float")
(defvar litex-format-float-lower-limit 1e-2
  "Lower limit of what number is formatted as float")

(defvar litex-steps-join-string "= "
  "String used for joining strings in steps of a solution")
(defvar litex-steps-end-string " "
  "String used at the end of each strings in steps of a solution")

(defvar litex-math-inline-start "\\("
  "Opening syntax for math inline environment")
(defvar litex-math-inline-end "\\)"
  "Closing syntax for math inline environment")

(defvar litex-math-equation-start "\\begin{equation}\n"
  "Opening syntax for math equation environment")
(defvar litex-math-equation-end "\n\\end{equation}\n"
  "Closing syntax for math equation environment")
(defvar litex-math-steps-equation-join-string "= "
  "Value of `litex-steps-join-string' to be used in equation environment")
(defvar litex-math-steps-equation-end-string " "
  "Value of `litex-steps-end-string' to be used in equation environment")

(defvar litex-math-eqnarray-start "\\begin{eqnarray*}\n"
  "Opening syntax for math eqnarray environment")
(defvar litex-math-eqnarray-end "\n\\end{eqnarray*}\n"
  "Closing syntax for math eqnarray environment")
(defvar litex-math-steps-eqnarray-join-string " &=& "
  "Value of `litex-steps-join-string' to be used in eqnarray environment")
(defvar litex-math-steps-eqnarray-end-string "\\\\\n"
  "Value of `litex-steps-end-string' to be used in eqnarray environment")


(defun litex-format-float (val)
  "Function that defines how float VAL is formatted in lisp2latex."
  (if (or (< val litex-format-float-lower-limit) (> val litex-format-float-upper-limit))
      (let* ((exponent (floor (log val 10)))
             (front (/ val (expt 10 exponent))))
        (format (concat litex-format-float-string " \\times 10^{%d}") front exponent))
    (format litex-format-float-string val)))


(defun litex-format-variable (var)
  "Format variable VAR for LaTeX"
  (let ((var-str (prin1-to-string var)))
    (if litex-make-hyphenated-to-subscript
	(while (string-match "\\([^-]+\\)[-]\\(.*\\)" var-str)
	  (setq var-str (format "%s_{%s}"
				(match-string 1 var-str)
				(match-string 2 var-str)))))
    var-str))


(defun litex-latex-maybe-enclose (form)
  "Encloses FORM in parantheis if LITEX-LATEX-MAYBE-ENCLOSE is true."
  (let* ((litex-latex-maybe-enclose? nil)
         (latex (litex-lisp2latex-all form)))
    (if litex-latex-maybe-enclose?
        (format "(%s)" latex)
      latex)))


;; formatting functions to be called by litex-lisp2latex-all
;; each one corresponds to the function at the end with args as arguments.
(defun litex-format-args-+ (args)
  "Formatting function for + operator"
  (let ((litex-latex-maybe-enclose? t))
       (mapconcat #'litex-lisp2latex-all args " + ")))


(defun litex-format-args-- (args)
  "Formatting function for - operator"
  (let ((arg1 (car args))
	(arg-rest (cdr args)))
  (if arg-rest
         (format "%s - %s" (litex-lisp2latex-all arg1)
                 (mapconcat #'litex-lisp2latex-all arg-rest " - "))
    (format "-%s" (litex-lisp2latex-all arg1)))))


(defun litex-format-args-* (args)
  "Formatting function for * operator"
  (let ((litex-latex-maybe-enclose? t))
   (with-output-to-string
       (cl-loop for (me next . rest) on args do
		(if (numberp next)
                    (princ (format "%s \\times " (litex-latex-maybe-enclose me)))
		  (princ (format "%s" (litex-latex-maybe-enclose me))))))))

(defun litex-format-args-/ (args)
  "Formatting function for / operator"
  (let ((arg1 (car args))
	(arg-rest (cdr args)))
  (if arg-rest
      (format "\\frac{%s}{%s}"
	      (litex-lisp2latex-all arg1)
              (litex-lisp2latex-all (cons '* arg-rest)))
    (format "\\frac1{%s}" (litex-lisp2latex-all arg1)))))


(defun litex-format-args-1+ (args)
  "Formatting function for 1+ operator"
  (concat (litex-lisp2latex-all (car args)) " + 1"))


(defun litex-format-args-expt (args)
  "Formatting function for expt function"
  (let ((base (car args))
	(power (cadr args)))
   (if (listp base)
         (format "(%s)^{%s}" (litex-lisp2latex-all base) (litex-lisp2latex-all power))
     (format "%s^{%s}"  (litex-lisp2latex-all base) (litex-lisp2latex-all power)))))


(defun litex-format-args-sqrt (args)
  "Formatting function for sqrt function"
  (format "\\sqrt{%s}" (litex-lisp2latex-all (car args))))


(defun litex-format-args-setq (args)
  "Formatting function for setq function"
  (with-output-to-string
    (cl-loop for (a b . rest) on args by #'cddr do
	     (princ (format "%s = %s" (litex-lisp2latex-all a) (litex-lisp2latex-all b)))
	     (when rest (princ "; ")))))


(setf (symbol-function 'litex-format-args-local-setq)
      #'litex-format-args-setq)


(defun litex-format-args-defun (args)
  "Formatting function for defun function"
  (let ((func-name (car args))
	   (fargs (cadr args))
	   (expr (caddr args)))
       (format "%s(%s):%s"
	       func-name
	       (mapconcat #'prin1-to-string fargs ",")
	       (litex-lisp2latex-all expr))))


(defun litex-format-args-default (func args)
  "Default Formatting function, Call corresponding formatting function if available for FUNC passing ARGS as argument, else make a general format."
  (let ((func-symbol (intern (format "litex-format-args-%s" func))))
    (if (functionp func-symbol)
	(apply func-symbol (list args))
      (let* ((known? (cl-find func litex-latex-functions))
            (enclose? (or (not known?)
                          (> (length args) 1)
                          (listp (cl-first args))))
            (format-string (concat (if known? "\\%s" "\\mathrm{%s}")
                                   (if enclose?  "(%s)" " %s"))))
	(format format-string func (mapconcat #'litex-lisp2latex-all args ","))))))


(defun litex-lisp2latex-all (form)
  "Convert given Lisp expression FORM to latex equivalent string."
  (pcase form
    
    ;; functions
    (`(,func . ,args) (litex-format-args-default func args))

    ;; simple variables
    (_
     (cond ((floatp form) (litex-format-float form))
	   ;; FIX: doesn't work now
	   ((symbolp form) (litex-format-variable form))
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
  (condition-case err
   (if (functionp expression)
      (format "%s" expression)
    (if (symbolp expression)
	(format "%s" (eval expression))
      (if (consp expression)
	  (format "(%s)"
		  (mapconcat #'litex-substitute-values expression " "))
	(prin1-to-string expression))))
   ;; this will catch error for undefined variables.
   (void-variable (prin1-to-string expression))))


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


(defun litex-solve-all-steps-equation ()
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


(defun litex-solve-all-steps-eqnarray ()
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

(define-minor-mode litex-mode
  "Minor mode for Calculations on lisp, and formatting on LaTeX."
  :lighter " LiTeX"
  :keymap (make-sparse-keymap))


;; you can choose to apply this keymap to some other key.
(defvar litex-key-map (make-sparse-keymap))
(define-key litex-key-map (kbd "F") 'litex-format-region)
(define-key litex-key-map (kbd "f") 'litex-format-region-last)
(define-key litex-key-map (kbd "E") 'litex-eval-and-replace)
(define-key litex-key-map (kbd "e") 'litex-eval-and-insert)
(define-key litex-key-map (kbd "s") 'litex-sexp-to-latex-exp)
(define-key litex-key-map (kbd "S") 'litex-sexp-solve-all-steps)
(define-key litex-key-map (kbd "r") 'litex-sexp-replace-variables)
(define-key litex-key-map (kbd "+") 'litex-increment-number)
(define-key litex-key-map (kbd "l") 'litex-exp-to-latex)
(define-key litex-key-map (kbd "m") 'litex-exp-in-latex-math)
(define-key litex-key-map (kbd "A") 'litex-solve-all-steps-equation)
(define-key litex-key-map (kbd "a") 'litex-solve-all-steps-eqnarray)

;; can be used directly
(define-key litex-mode-map (kbd "×") 'litex-insert-or-replace-x)


(provide 'litex-mode)

;;; litex-mode.el ends here
