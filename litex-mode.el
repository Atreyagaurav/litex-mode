;;; litex-mode.el --- Minor mode for converting lisp to LaTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author: Gaurav Atreya <allmanpride@gmail.com>
;; URL: https://github.com/Atreyagaurav/litex-mode
;; Version: 0.1
;; Keywords: calculator, lisp, LaTeX
;; Package-Requires: ((emacs "24.4") (units-mode "0.1.1"))

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
;; improved from https://emacs.stackexchange.com/a/70360 modified with
;; help from
;; https://gist.github.com/bpanthi977/4b8ece0eeff3bc05bb82275a23cbb56d

;; For detailed help visit github page:
;; https://github.com/Atreyagaurav/litex-mode

;;; Code:
(eval-when-compile (require 'pcase))
(eval-when-compile (require 'subr-x))
(require 'cl-lib)
(require 'ob-lisp)
(require 'units-mode)

;; list from:
;; https://www.overleaf.com/learn/latex/Operators#Reference_guide
;; not all of these functions might have lisp equivalent, check it.
(defvar litex-latex-functions
  '(cos csc exp ker limsup min sinh arcsin cosh deg gcd lg ln Pr sup
	arctan cot det hom lim log sec tan arg coth dim liminf max
	sin tanh)
  "Lisp functions that have their own latex commands.")
(defvar litex-make-unicode-to-latex nil
  "Whether to convert unicode to LaTeX equivalent (eg.  α -> \alpha).
 These work better in math mode.")
(defvar litex-make-name-to-latex-glyph nil
  "Convert variables with the same name as a glyph to a LaTeX glyph.
(eg.  alpha -> \alpha).")
(defvar litex-make-hyphenated-to-subscript t
  "Whether to make the hyphenated variables subscript or not.")
(defvar litex-latex-always-enclose? nil
  "Always enclose latex converted with paran.")
(defvar litex-keep-sexp-in-buffer nil
  "Keep the last sexp on point if true, else replace it.")

(defvar litex-format-float-string "%.3f"
  "Format string to be used by floats.")
(defvar litex-format-float-upper-limit 1e4
  "Upper limit of what number is formatted as float.")
(defvar litex-format-float-lower-limit 1e-2
  "Lower limit of what number is formatted as float.")
(defvar litex-format-float-trim-decimal nil
  "Trim zeros after decimal if all decimals are zeros.")

(defvar litex-steps-join-string "= "
  "String used for joining strings in steps of a solution.")
(defvar litex-steps-end-string " "
  "String used at the end of each strings in steps of a solution.")

(defvar litex-math-inline-start "\\("
  "Opening syntax for math inline environment.")
(defvar litex-math-inline-end "\\)"
  "Closing syntax for math inline environment.")
(defvar litex-math-brackets-start "\\left("
  "Opening syntax for math brackets.")
(defvar litex-math-brackets-end "\\right)"
  "Closing syntax for math brackets.")

(defvar litex-math-equation-start "\\begin{equation}\n"
  "Opening syntax for math equation environment.")
(defvar litex-math-equation-end "\n\\end{equation}\n"
  "Closing syntax for math equation environment.")
(defvar litex-math-steps-equation-join-string "= "
  "Value of `litex-steps-join-string' used in equation environment.")
(defvar litex-math-steps-equation-end-string " "
  "Value of `litex-steps-end-string' used in equation environment.")

(defvar litex-math-eqnarray-start "\\begin{eqnarray*}\n"
  "Opening syntax for math eqnarray environment.")
(defvar litex-math-eqnarray-end "\n\\end{eqnarray*}\n"
  "Closing syntax for math eqnarray environment.")
(defvar litex-math-steps-eqnarray-join-string " &=& "
  "Value of `litex-steps-join-string' used in eqnarray environment.")
(defvar litex-math-steps-eqnarray-end-string "\\\\\n"
  "Value of `litex-steps-end-string' used in eqnarray environment.")

(defvar litex-math-align-start "\\begin{align*}\n"
  "Opening syntax for math align environment.")
(defvar litex-math-align-end "\n\\end{align*}\n"
  "Closing syntax for math align environment.")
(defvar litex-math-steps-align-join-string "& = "
  "Value of `litex-steps-join-string' used in align environment.")
(defvar litex-math-steps-align-end-string "\\\\\n"
  "Value of `litex-steps-end-string' used in align environment.")

(defvar litex-use-slime-for-eval nil
  "Whether to use slime process for evalulation or not.
 You need to start slime yourself.")


(defvar litex-greek-unicode-latex-alist
  '(("α" . "alpha")
    ("β" . "beta")
    ("γ" . "gamma")
    ("δ" . "delta")
    ("ε" . "epsilon")
    ("ϵ" . "varepsilon")
    ("ζ" . "zeta")
    ("η" . "eta")
    ("θ" . "theta")
    ("ϑ" . "vartheta")
    ("ι" . "iota")
    ("κ" . "kappa")
    ("λ" . "lambda")
    ("μ" . "mu")
    ("ν" . "nu")
    ("ξ" . "xi")
    ("π" . "pi")
    ("ρ" . "rho")
    ("ϱ" . "varrho")
    ("σ" . "sigma")
    ("τ" . "tau")
    ("υ" . "upsilon")
    ("φ" . "phi")
    ("φ" . "varphi")
    ("χ" . "chi")
    ("ψ" . "psi")
    ("ω" . "omega")

    ("Γ" . "Gamma")
    ("Δ" . "Delta")
    ("Ζ" . "Zeta")
    ("Θ" . "Theta")
    ("Λ" . "Lambda")
    ("Ξ" . "Xi")
    ("Π" . "Pi")
    ("Ρ" . "Rho")
    ("Σ" . "Sigma")
    ("Υ" . "Upsilon")
    ("Φ" . "Phi")
    ("Ψ" . "Psi")
    ("Ω" . "Omega"))
  "Alist of greek unicode symbols and their LaTeX counterparts.")


(defun litex-varible-is-ratio (var)
  "Check if the slime output of ratio type was mistaken as a symbol.
Argument VAR: variable which could be from slime output."
  (if (string-match-p "[0-9]+/[0-9]+" (prin1-to-string var)) t nil))

(defun litex-process-slime-output (output)
  "Format the OUTPUT from slime to litex form."
  (pcase (type-of output)
    ('symbol (let ((out-str (prin1-to-string output)))
	       (if (litex-varible-is-ratio output)
		 (let ((parts (split-string out-str "/"))
		       (litex-format-float-trim-decimal t))
		   (/ (* 1.0 (read (car parts))) (read (cadr parts))))
	     (if (string-match "\\([0-9]+\\)\\\\?\\([0-9.]+\\)d\\([+-]?[0-9]+\\)" out-str)
		 (read (format "%s%se%s" 
			       (match-string 1 out-str)
		       (match-string 2 out-str)
		       (match-string 3 out-str)))
	       output))))
    (_ output)))

(defun litex-eval (expr)
  "Eval funcion used by LiTeX, evaluate the EXPR in elisp or slime."
  (if litex-use-slime-for-eval
      (litex-process-slime-output
       (org-babel-execute:lisp (prin1-to-string expr) '()))
    (eval expr)))

;; Formatting functions
(defun litex-format-float-not-within-limits (val)
  "Check is VAL is within float limits."
  (let ((sign (if (< val 0) -1 1)))
    (or (< (* sign val) litex-format-float-lower-limit)
	  (> (* sign val) litex-format-float-upper-limit))))


(defun litex-format-float (val &optional nolim)
  "Function that defines how float VAL is formatted in lisp2latex."
  (let ((sign (if (< val 0) -1 1)))
  (if (and (not nolim) (litex-format-float-not-within-limits val))
      (let* ((exponent (floor (log (* sign val) 10)))
             (front (/ val (expt 10 exponent))))
        (concat (litex-format-float front t)
		" \\times 10^{"
		(number-to-string exponent) "}"))
    (let ((formatted (format litex-format-float-string val)))
      (if litex-format-float-trim-decimal
	  (string-trim-right formatted "[.]0+")
	formatted)))))


(defun litex-read-sexp-maybe-kill ()
  "Read the sexp before point.
Kill it if `litex-keep-sexp-in-buffer' is nil."
  (interactive)
  (let ((expr (sexp-at-point)))
    (if (not litex-keep-sexp-in-buffer)
	(backward-kill-sexp)
      ;; should I do it here or check if it's a comment and only do
      ;; that later. or copy the things after the sexp till EOL.
      (or (end-of-line) (insert "\n")))
    expr))


(defun litex-format-greek-characters (string)
  "Format STRING to Greek LaTeX notation.
Replace greek unicode or character name to latex notation."
  (let ((var-str string)
	(var-assoc nil))
    (when litex-make-name-to-latex-glyph
      (when (rassoc var-str litex-greek-unicode-latex-alist)
	(setq var-str (concat "{\\" var-str "}"))))
    (when litex-make-unicode-to-latex
      (setq var-assoc (cdr (assoc var-str litex-greek-unicode-latex-alist)))
      (when var-assoc
	(setq var-str
	      (concat "{\\" var-assoc "}"))))
    var-str))


(defun litex-format-variable (var)
  "Format variable VAR for LaTeX."
  (let ((var-strs (reverse
		   (mapcar
		    (lambda (s) (mapconcat #'litex-format-greek-characters
				      (split-string s "*") ""))
		    (split-string (prin1-to-string var t) "-")))))

    (let ((var-final (car var-strs)))
      (if (> (length var-strs) 1)
	  (cl-loop for (cvar . rest) on (cl-rest var-strs) do
		   (setq var-final (format "%s_{%s}" cvar var-final))))
      var-final)))


;; this function needs some serious thoughts
(defun litex-latex-maybe-enclose (form &optional parent-func)
  "Encloses FORM which is argument of PARENT-FUNC in parantheis.
If `litex-latex-always-enclose?' is true or if it determines it's necessary."
  (let* ((latex (litex-lisp2latex-all form)))
    (if (or litex-latex-always-enclose?
	    (and
	     (consp form)
	     (litex-latex-enclose-check-args (cdr form))
	     (and parent-func
		  (litex-latex-enclose-check-function parent-func))))
        (format " %s %s %s "
		litex-math-brackets-start
		latex
		litex-math-brackets-end)
      latex)))


(defun litex-latex-enclose-check-args (args)
  "Check if we need to use parantheis based on ARGS."
  (and (listp args)
       (or (> (length args) 1)
	   (listp (car args)))))


(defun litex-latex-enclose-check-function (func)
  "Check if we need to use parantheis for args based on FUNC.

Return true if that function may need its argument to be in brackets
 if they are multiple arguments."
  (if (member func '(+ 1+ 1- expt / defun setq))
      nil
    (if (member func '(- *))
	t
      nil)))


;; formatting functions to be called by `litex-lisp2latex-all', each
;; one corresponds to the function at the end with args as arguments.
(defun litex-format-args-+ (args)
  "Formatting function for + operator called with ARGS."
    (mapconcat #'litex-latex-maybe-enclose args " + "))


(defun litex-format-args-- (args)
  "Formatting function for - operator called with ARGS."
  (let ((arg1 (car args))
	(arg-rest (cdr args)))
    (if arg-rest
        (format "%s - %s" (litex-latex-maybe-enclose arg1)
                (mapconcat (lambda (a)
			     (litex-latex-maybe-enclose a '-))
			   arg-rest " - "))
      (format "-%s" (litex-latex-maybe-enclose arg1 '-)))))


(defun litex-format-args-* (args)
  "Formatting function for * operator called with ARGS."
  (with-output-to-string
    (cl-loop for (me next . rest) on args do
	     (princ (format "%s"
			    (litex-latex-maybe-enclose me '*)))
	     (if (and next
		      (or (and (symbolp me)
			       (> (length (prin1-to-string me)) 1))
			  (and (symbolp next)
			       (> (length (prin1-to-string next)) 1))
			  (numberp next)))
                 (princ " \\times ")))))

(defun litex-format-args-/ (args)
  "Formatting function for / operator called with ARGS."
  (let ((arg1 (car args))
	(arg-rest (cdr args)))
    (if arg-rest
	(format "\\frac{%s}{%s}"
		(litex-latex-maybe-enclose arg1 '/)
		(litex-latex-maybe-enclose (cons '* arg-rest) '/))
      (format "\\frac1{%s}" (litex-latex-maybe-enclose arg1 '/)))))


(defun litex-format-args-1+ (args)
  "Formatting function for 1+ called with ARGS operator."
  (concat (litex-latex-maybe-enclose (car args) '1+) " + 1"))


(defun litex-format-args-1- (args)
  "Formatting function for 1- called with ARGS operator."
  (concat (litex-latex-maybe-enclose (car args) '1-) " - 1"))


(defun litex-format-args-expt (args)
  "Formatting function for expt function called with ARGS."
  (let ((base (car args))
	(power (cadr args)))
    (if (or (listp base)
	    ;; for cases where the base will be using × 10^{} format.
	    (and (numberp base)
		 (litex-format-float-not-within-limits base)))
	(format "%s %s %s^{%s}"
		litex-math-brackets-start
		(litex-latex-maybe-enclose base 'expt)
		litex-math-brackets-end
		(litex-latex-maybe-enclose power 'expt))
      (format "%s^{%s}"
	      (litex-latex-maybe-enclose base 'expt)
	      (litex-latex-maybe-enclose power 'expt)))))


(defun litex-format-args-sqrt (args)
  "Formatting function for sqrt function called with ARGS."
  (format "\\sqrt{%s}" (litex-latex-maybe-enclose (car args) 'sqrt)))


(defun litex-format-args-setq (args)
  "Formatting function for setq function called with ARGS."
  (with-output-to-string
    (cl-loop for (a b . rest) on args by #'cddr do
	     (princ (format "%s = %s"
			    (litex-latex-maybe-enclose a 'setq)
			    (litex-latex-maybe-enclose b 'setq)))
	     (when rest (princ "; ")))))


(setf (symbol-function 'litex-format-args-local-setq)
      #'litex-format-args-setq)


(defun litex-format-args-defun (args)
  "Formatting function for defun called with ARGS function."
  (let ((func-name (car args))
	(fargs (cadr args))
	(expr (caddr args)))
    (format "\\text{%s}(%s) = %s"
	    (litex-format-variable func-name)
	    (mapconcat #'prin1-to-string fargs ",")
	    (litex-latex-maybe-enclose expr 'defun))))

(defun litex-format-args-units-convert-simple (args)
    (let ((expr (car args))
	(from-unit (cadr args)))
      (if (litex-is-final-form expr)
	  (format "\\unit[%s]{%s}"
		  (litex-latex-maybe-enclose expr 'units-convert-simple)
		  from-unit)
	(litex-latex-maybe-enclose expr))))

(defun litex-format-args-units-ignore (args)
    (let ((expr (car args))
	  (unit (cadr args)))
      (if (litex-is-final-form expr)
	(format "\\unit[%s]{%s}"
	      (litex-latex-maybe-enclose expr 'units-ignore)
	      unit)
	(litex-latex-maybe-enclose expr))))


(defun litex-format-args-default (func args)
  "Default Formatting function for Lisp expressions.

Call corresponding called with ARGS formatting function if
available for FUNC passing ARGS as argument, else make a general
format."
  (let ((func-symbol (intern (format "litex-format-args-%s" func))))
    (if (functionp func-symbol)
	(apply func-symbol (list args))
      (let* ((known? (cl-find func litex-latex-functions))
             (enclose? (or (not known?)
                           (litex-latex-enclose-check-args args)))
             (format-string
	      (concat (if known? "\\%s" "\\text{%s}")
                      (if enclose?
			  (concat litex-math-brackets-start
				  "%s"
				  litex-math-brackets-end)
			" %s"))))
	(format format-string func
		(mapconcat #'litex-latex-maybe-enclose args ","))))))


(defun litex-lisp2latex-all (form)
  "Convert given Lisp expression FORM to latex equivalent string."
  (pcase form

    ;; functions
    (`(,func . ,args) (litex-format-args-default func args))

    ;; simple variables
    (_
     (cond ((floatp form) (litex-format-float form))
	   ((or (symbolp form) (stringp form))
	    (litex-format-variable form))
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
  (condition-case nil
   (if (functionp expression)
      (format "%s" expression)
    (if (symbolp expression)
	(format "%s" (litex-eval expression))
      (if (consp expression)
	  (format "(%s)"
		  (mapconcat #'litex-substitute-values
			     expression " "))
	(prin1-to-string expression))))
   ;; this will catch error for undefined variables.
   (void-variable (prin1-to-string expression))))


(defun litex-units-single-step (form)
  (pcase form
    (`(units-convert ,_ ,unit) (list 'units-ignore (litex-eval form) unit))
    (`(units-convert-simple ,_ ,_ ,unit)
     (list 'units-ignore
	   (litex-eval form) unit))
    (`(units-reduce ,_) (litex-eval form))
    (`(units-ignore ,exp ,_) (litex-solve-single-step exp))
    (_ (error "Unknown units function."))))

(defun litex-units-is-final-form (form)
  (pcase form
    (`(units-convert ,_ ,_) nil)
    (`(units-convert-simple ,_ ,_ ,_) nil)
    (`(units-reduce ,_) nil)
    (`(units-ignore ,exp ,_) (if (litex-is-final-form exp) t nil))
    (_ nil)))


(defun litex-is-final-form (form)
  (or (numberp form)
      (and (symbolp form) (litex-varible-is-ratio form))
      (stringp form)))


(defun litex-solve-single-step (form)
  "Solves a single step of calculation in FORM."
  (cond ((listp form)
         (if (cl-every #'litex-is-final-form (cl-rest form))
             (if (string-match-p "^units-"
				 (symbol-name (car form)))
		 (if (litex-units-is-final-form form)
		     (litex-eval form)
		   (litex-units-single-step form))
	       (litex-eval form))
           (cons (car form)
		 (mapcar #'litex-solve-single-step (cl-rest form)))))
	((functionp form)
	 form)

        ((symbolp form)
         (symbol-value form))

        (t form)))


(defun litex-solve-all-steps (form)
  "Solves all the steps of calculations in FORM expression.
Retuns a list of steps."
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

    (while (and (consp form)
	       (not (litex-units-is-final-form form)))
      (setq solution
	    (append solution
		    (list (setq form
				(litex-solve-single-step form))))))
    solution))


(defun litex-sexp-to-solved-string (expression format-func)
  "Return solution of EXPRESSION using FORMAT-FUNC to format steps."
  (pcase expression
    (`(setq ,var ,exp)
     (concat
      (format "%s%s"
	      (litex-format-variable var)
	      litex-steps-join-string)
      (mapconcat format-func
		 (litex-solve-all-steps exp)
		 (concat litex-steps-end-string
			 litex-steps-join-string))))
    (_ (mapconcat format-func
		  (litex-solve-all-steps expression)
		  (concat litex-steps-end-string
			  litex-steps-join-string)))))


(defun litex-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (litex-eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun litex-eval-and-insert ()
  "Insert the evaulation result of the preceding sexp."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (insert (current-kill 0)
	      litex-steps-join-string
	      (format "%s"
		      (litex-eval (read (current-kill 0)))))
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
  (insert (litex-lisp2latex-all (litex-read-sexp-maybe-kill))))


(defun litex-sexp-replace-variables ()
  "Replace the variable values in the last sexp at point."
  (interactive)
  (insert (litex-substitute-values (litex-read-sexp-maybe-kill))))


(defun litex-sexp-solve-all-steps ()
  "Solve last sexp at point in steps and insert those steps."
  (interactive)
  (let ((expression (litex-read-sexp-maybe-kill)))
    (insert
     (litex-sexp-to-solved-string expression #'prin1-to-string))))


(defun litex-sexp-solve-single-step ()
  "Solve last sexp at point for one step and insert it."
  (interactive)
  (let ((expression (litex-read-sexp-maybe-kill)))
    (insert
     (prin1-to-string (litex-solve-single-step expression)))))


(defun litex-solve-all-steps-equation ()
  "Solve last sexp in steps and insert it in LaTeX equation environment."
  (interactive)
  (let ((expression (litex-read-sexp-maybe-kill))
	(litex-steps-join-string litex-math-steps-equation-join-string)
	(litex-steps-end-string litex-math-steps-equation-end-string))
    (insert litex-math-equation-start
	    (litex-sexp-to-solved-string expression #'litex-lisp2latex-all)
	    litex-math-equation-end)))


(defun litex-solve-all-steps-eqnarray ()
  "Solve last sexp in steps and insert it in LaTeX eqnarray environment."
  (interactive)
  (let ((expression (litex-read-sexp-maybe-kill))
	(litex-steps-join-string litex-math-steps-eqnarray-join-string)
	(litex-steps-end-string litex-math-steps-eqnarray-end-string))
    (insert litex-math-eqnarray-start
	    (litex-sexp-to-solved-string expression #'litex-lisp2latex-all)
	    litex-math-eqnarray-end)))

(defun litex-solve-all-steps-align ()
  "Solve last sexp in steps and insert it in LaTeX align environment."
  (interactive)
  (let ((expression (litex-read-sexp-maybe-kill))
	(litex-steps-join-string litex-math-steps-align-join-string)
	(litex-steps-end-string litex-math-steps-align-end-string))
    (insert litex-math-align-start
	    (litex-sexp-to-solved-string expression #'litex-lisp2latex-all)
	    litex-math-align-end)))

(defun litex-solve-region-all-steps-align (beg end)
  "Solve last sexp in steps and insert it in LaTeX eqnarray environment."
  (interactive "r")
  (let ((litex-steps-join-string litex-math-steps-align-join-string)
	(litex-steps-end-string litex-math-steps-align-end-string)
	(expressions (cdr (read
			   (concat "(prog "
				   (buffer-substring-no-properties beg end)
				   ")")))))
    (delete-region beg end)
    (insert litex-math-align-start
	    (string-join
	     (cl-loop for exp in expressions
		      collect (litex-sexp-to-solved-string
			       exp #'litex-lisp2latex-all)
		      do (litex-eval exp))
	     litex-steps-end-string)
	    litex-math-align-end)))



(defun litex-format-region-last (beg end)
  "Format region like last `litex-format-region` call (without query).
BEG and END are region bounds."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'sexp)))
		   (list (cl-first bnd) (cl-rest bnd)))))
  (let ((text (buffer-substring-no-properties beg end)))
    ;; maybe I should make it litex-eval if given expression
    (if (string-match-p "%[0-9.]*[dfex]" litex-format-float-string)
	(setq text (litex-eval (read text))))
    (kill-region beg end)
    (insert (format litex-format-float-string text))))


(defun litex-format-region (beg end)
  "Format selected region as per input format, BEG and END are region bounds."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bnd (bounds-of-thing-at-point 'symbol)))
		   (list (cl-first bnd) (cl-rest bnd)))))
  (let ((fmt  (read-string "Enter format string: "
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
	  (insert
	   (match-string 1 word)
	   (number-to-string (+ step (string-to-number
				      (match-string 2 word))))
	   (match-string 3 word)))))))



(defun litex-insert-or-replace-x (beg end)
  "If a region (BEG to END) is selected, replace * by \times
 otherwise insert \times instead of ×."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (goto-char end)
  (if (= beg end)
      (insert "\\times")
    (when (re-search-backward "*" beg)
      (replace-match "\\\\times"))))


;; I'm not making a litex-mode-map because I don't want it to come by
;; default, user can choose to apply this keymap to some other key as
;; prefix key, like C-e in readme, or just map individual functions.
(defvar litex-key-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "F") 'litex-format-region)
    (define-key keymap (kbd "f") 'litex-format-region-last)
    (define-key keymap (kbd "E") 'litex-eval-and-replace)
    (define-key keymap (kbd "e") 'litex-eval-and-insert)
    (define-key keymap (kbd "s") 'litex-sexp-to-latex-exp)
    (define-key keymap (kbd "S") 'litex-sexp-solve-all-steps)
    (define-key keymap (kbd "r") 'litex-sexp-replace-variables)
    (define-key keymap (kbd "+") 'litex-increment-number)
    (define-key keymap (kbd "l") 'litex-exp-to-latex)
    (define-key keymap (kbd "m") 'litex-exp-in-latex-math)
    (define-key keymap (kbd "A") 'litex-solve-all-steps-equation)
    (define-key keymap (kbd "a") 'litex-solve-all-steps-align)
    (define-key keymap (kbd "C-a") 'litex-solve-all-steps-eqnarray)
    (define-key keymap (kbd "C-r") 'litex-solve-region-all-steps-align)
    keymap))

(define-minor-mode litex-mode
  "Minor mode for Calculations on lisp, and formatting on LaTeX."
  :lighter " LiTeX")


(provide 'litex-mode)

;;; litex-mode.el ends here
