;;; litex-mode.el --- Minor mode for elisp based calculation.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author: Gaurav Atreya <allmanpride@gmail.com>
;; Version: 0.1
;; Keywords: calculator, lisp, LaTeX

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

;; converts valid mathematical lisp expressions to LaTeX snippets
;; improved from https://emacs.stackexchange.com/a/70360
;; modified with help from https://gist.github.com/bpanthi977/4b8ece0eeff3bc05bb82275a23cbb56d

;;; Code:

(require 'cl)

(defvar litex-latex-functions '(sin cos tan))
(defvar litex-latex-maybe-enclose? nil)


(setq litex-format-string "%.2f")

(defun litex-latex-maybe-enclose (form)
  (let* ((litex-latex-maybe-enclose? nil)
         (latex (litex-lisp2latex-all form)))
    (if litex-latex-maybe-enclose?
        (format "(%s)" latex)
      latex)))


(defun litex-lisp2latex-all (form)
  "Converts given lisp expression to latex equivalent"
  (pcase form
    ;; basic operators
    (`(+ . ,args)
     (setf litex-latex-maybe-enclose t)
     (mapconcat #'litex-lisp2latex-all args " + "))

    (`(* . ,args)
     (setf latex-maybe-enclose t)
     (with-output-to-string
       (loop for (me next . rest) on args do
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
       (loop for (a b . rest) on args by #'cddr do
             (princ (format "%s = %s" (litex-lisp2latex-all a) (litex-lisp2latex-all b)))
             (when rest (princ "; ")))))

    ;; other operators
    (`(1+ ,arg) (concat "1 + " (litex-lisp2latex-all arg) ))
    (`(sqrt ,arg) (format "\\sqrt{%s}" (litex-lisp2latex-all arg)))


    ;; named functions
    (`(,func . ,args)
     (let* ((known? (find func litex-latex-functions))
            (enclose? (or (not known?)
                          (> (length args) 1)
                          (listp (first args))))
            (format-string (concat (if known? "\\%s" "\\mathrm{%s}")
                                   (if enclose?  "(%s)" " %s"))))
       (format format-string func (mapconcat #'litex-lisp2latex-all args ","))))

    (_
     (cond ((floatp form)
            (if (or (< form 1e-2) (> form 1e4))
                (let* ((exponent (floor (log form 10)))
                      (front (/ form (expt 10 exponent))))
                  (format "%.2f \\times 10^{%d}" front exponent))
              (format "%.3f" form)))
           (t (prin1-to-string form))))))



(defun litex-contains-variables (expression)
  "gives a string from expression substituting the values."
  (if (functionp expression)
      nil
  (if (symbolp expression)
	t
      (if (consp expression)
	  (cl-some #'litex-contains-variables expression)
      nil))))


(defun litex-substitute-values (expression)
  "gives a string from expression substituting the values."
  (if (functionp expression)
      (format "%s" expression)
    (if (symbolp expression)
	(format "%s" (eval expression))
      (if (consp expression)
	  (format "(%s)"
		  (mapconcat #'litex-substitute-values expression " "))
	(prin1-to-string expression)))))


(defun litex-solve-single-step (form)
  (cond ((listp form)
         (if (every #'numberp (rest form))
             (eval form)
           (cons (first form) (mapcar #'litex-solve-single-step (rest form)))))
	((functionp form)
	 form)

        ((symbolp form)
         (symbol-value form))

        (t form)))

;; (defun solve-single-step (expression)
;;   (let
;;       ((form-str (prin1-to-string expression)))
;;     (read
;;      (replace-regexp-in-string "\\(([^(^)]+)\\)"
;; 			       (lambda (s) (format "%s" (eval (read s))))
;; 			       form-str)))
;;   )


(defun litex-solve-all-steps (form)
  (let
      ((solution (list (prin1-to-string form)))) ;given expression
    (if
	(litex-contains-variables form)
      (setq solution
	    (append solution
		    (list
		     (prin1-to-string
				  (setq form
					(read
					 (litex-substitute-values form))))))))

  (while (consp form)
    (setq solution
	  (append solution
		  (list (prin1-to-string
			 (setq form
			       (litex-solve-single-step form)))))))
  solution))


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
  "Inserts the evaulation result of the preceding sexp."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (insert (concat (current-kill 0)
		     " = "
		     (format "%s"
			     (eval (read (current-kill 0))))))
    (error (message "Invalid expression"))))


(defun litex-exp-to-latex (beg end)
  "Converts exponentials to latex notation."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward
     "\\([0-9.+-]+\\)e\\([0-9.+-]+\\)" beg)
      (replace-match "\\1 \\\\times 10^{\\2}")
      )))


(defun litex-exp-in-latex-math (beg end)
  "Inserts the selected expression inside latex inline math environment."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (insert "\\(")
    (goto-char (+ (point) (- end beg)))
    (insert "\\)")
    ))


(defun litex-sexp-to-latex-exp ()
  "Converts valid sexp to latex expressions."
  (interactive)
  (backward-kill-sexp)
  (insert (litex-lisp2latex-all (read (current-kill 0))))
  )


(defun litex-sexp-replace-variables ()
  (interactive)
  (backward-kill-sexp)
  (insert (litex-substitute-values (read (current-kill 0))))
  )


(defun litex-sexp-solve-all-steps ()
  (interactive)
  (backward-kill-sexp)
  (let ((expression (read (current-kill 0))))
    (pcase expression
      (`(setq ,var ,exp)
       (insert (concat
		(format "%s = " var)
		(s-join " = " (litex-solve-all-steps exp)))))
      (_ (s-join " = " (litex-solve-all-steps expression)))
      )))



(defun litex-format-region-last (beg end)
      (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'sexp)))
		   (list (cl-first bnd) (cl-rest bnd)))))
      (let ((text (buffer-substring-no-properties beg end)))
	;; maybe I should make it eval if given expression
      (if (string-match-p "%[0-9.]*[dfex]" litex-format-string)
	    (setq text (eval (read text))))
      (kill-region beg end)
      (insert (format litex-format-string text))))


(defun litex-format-region (beg end)
    (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'symbol)))
		   (list (cl-first bnd) (cl-rest bnd)))))
    (let ((fmt  (read-string "Enter format string:"
			     litex-format-string)))
      (setq litex-format-string fmt)
      (litex-format-region-last beg end)))


(defun litex-increment-number (step)
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
		   (match-string 3 word))
		  ))))))



(defun insert-or-replace-x (beg end)
  "If a region is selected, replaces * by \times otherwise inserts \times
instead of ×. Useful in LaTeX or to convert mathmatical expression
to human redable one."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (goto-char end)
  (if (= beg end)
      (insert "\\times")
      (when (re-search-backward "*" beg)
	(replace-match "\\\\times")
	)))


(local-set-key "×" 'insert-or-replace-x)


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



(define-minor-mode litex-mode
  "Minor mode for Calculations on lisp, and formatting on LaTeX."
  :lighter " LiTeX"
  ;; :keymap `((,(kbd "C-e") . litex-key-map))
  )


(provide 'litex-mode)

;;; litex-mode.el ends here
