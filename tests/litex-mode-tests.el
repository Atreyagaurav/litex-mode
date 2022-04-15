;; Run all tests using (ert t)

;; format tests
(ert-deftest litex-format-float-test ()
  (let ((litex-format-float-string "%.3f")
	(litex-format-float-upper-limit 1e4)
	(litex-format-float-lower-limit 1e-2))
    (should (string= (litex-format-float 2.0) "2.000"))
    (should (string= (litex-format-float 2.1234) "2.123"))
    (should (string= (litex-format-float 2.1236) "2.124")))
  (let ((litex-format-float-string "%.2f"))
    (should (string= (litex-format-float 2.0) "2.00")))
  (let ((litex-format-float-string "%d"))
    (should (string= (litex-format-float 2.0) "2")))
  (let ((litex-format-float-string "%.2e"))
    (should (string= (litex-format-float 2.0) "2.00e+00"))))


(ert-deftest litex-format-float-test-2 ()
  (let ((litex-format-float-string "%.3f")
	(litex-format-float-upper-limit 1e4)
	(litex-format-float-lower-limit 1e-2))
    (should (string= (litex-format-float 2.0e5)
		     "2.000 \\times 10^{5}"))
    (should (string= (litex-format-float 2.1234e-3)
		     "2.123 \\times 10^{-3}"))
    (should (string= (litex-format-float 2.1236e-6)
		     "2.124 \\times 10^{-6}"))))

(ert-deftest litex-format-variable-test ()
  (let ((litex-make-unicode-to-latex t)
	(litex-make-name-to-latex-glyph t))
    (should (string= (litex-format-variable 'α) "{\\alpha}"))
    (should (string= (litex-format-variable 'α-β) "{\\alpha}_{{\\beta}}"))
    (should (string= (litex-format-variable 'α-Δ/t-2)
		     "{\\alpha}_{{\\Delta}t}_{2}"))
    (should (string= (litex-format-variable '\\alpha) "\\alpha"))
    (should (string= (litex-format-variable '\\alpha-\\beta) "\\alpha_{\\beta}"))
    (should (string= (litex-format-variable '\\alpha-\\Delta\ t-2)
		     "\\alpha_{\\Delta t}_{2}"))
    (should (string= (litex-format-variable 'alpha) "{\\alpha}"))
    (should (string= (litex-format-variable 'Delta/alpha)
		     "{\\Delta}{\\alpha}"))
    (should (string= (litex-format-variable 'alpha/beta)
		     "{\\alpha}{\\beta}")))
  )


;; individual operator tests
(ert-deftest litex-format-args-+-test ()
    (should (string= (litex-format-args-+ '(1 2)) "1 + 2"))
    (should (string= (litex-format-args-+ '(x 2)) "x + 2"))
    (should (string= (litex-format-args-+ '(1 y)) "1 + y"))
    (should (string= (litex-format-args-+ '(x y)) "x + y")))


(ert-deftest litex-format-args---test ()
    (should (string= (litex-format-args-- '(1 2)) "1 - 2"))
    (should (string= (litex-format-args-- '(x 2)) "x - 2"))
    (should (string= (litex-format-args-- '(1 y)) "1 - y"))
    (should (string= (litex-format-args-- '(x y)) "x - y"))
    (should (string= (litex-format-args-- '(3)) "-3"))
    (should (string= (litex-format-args-- '(x)) "-x")))


(ert-deftest litex-format-args-*-test ()
    (should (string= (litex-format-args-* '(1 2)) "1 \\times 2"))
    (should (string= (litex-format-args-* '(x 2)) "x \\times 2"))
    (should (string= (litex-format-args-* '(2 y)) "2y"))
    (should (string= (litex-format-args-* '(x y)) "xy"))
    (should (string= (litex-format-args-* '(2 yz)) "2 \\times yz"))
    (should (string= (litex-format-args-* '(xy y)) "xy \\times y")))


(ert-deftest litex-format-args-/-test ()
    (should (string= (litex-format-args-/ '(1 2)) "\\frac{1}{2}"))
    (should (string= (litex-format-args-/ '(x 2)) "\\frac{x}{2}"))
    (should (string= (litex-format-args-/ '(1 y)) "\\frac{1}{y}"))
    (should (string= (litex-format-args-/ '(1 2 y))
		     "\\frac{1}{2y}"))
    (should (string= (litex-format-args-/ '(1 2 y 3))
		     "\\frac{1}{2y \\times 3}"))
    (should (string= (litex-format-args-/ '(x y)) "\\frac{x}{y}"))
    (should (string= (litex-format-args-/ '(2)) "\\frac1{2}"))
    (should (string= (litex-format-args-/ '(x)) "\\frac1{x}")))


(ert-deftest litex-format-args-1+-test ()
    (should (string= (litex-format-args-1+ '(1)) "1 + 1"))
    (should (string= (litex-format-args-1+ '(x)) "x + 1")))


(ert-deftest litex-format-args-expt-test ()
    (should (string= (litex-format-args-expt '(1 2)) "1^{2}"))
    (should (string= (litex-format-args-expt '(x 2)) "x^{2}"))
    (should (string= (litex-format-args-expt
		      '((+ 1 2) 2)) "\\left( 1 + 2 \\right)^{2}"))
    (should (string= (litex-format-args-expt '((* 2 x) 2))
		     "\\left( 2x \\right)^{2}")))


(ert-deftest litex-lisp2latex-all-test ()
  (should (string= (litex-lisp2latex-all
		    '(setq x 2)) "x = 2"))
  (should (string= (litex-lisp2latex-all
		    '(setq x (- y 2))) "x = y - 2"))
  (should (string= (litex-lisp2latex-all
		    '(setq x (expt y 2))) "x = y^{2}"))
  (should (string= (litex-lisp2latex-all
		    '(setq z (expt (1+ x) y)))
		   "z = \\left( x + 1 \\right)^{y}"))
  (should (string= (litex-lisp2latex-all
		    '(setq z (expt (* 2 x) y)))
		   "z = \\left( 2x \\right)^{y}"))
  (should (string= (litex-lisp2latex-all
		    '(setq z (expt (1+ x) (* 2 y))))
		   "z = \\left( x + 1 \\right)^{2y}"))
  (should (string= (litex-lisp2latex-all '(1- (* 2 x))) "2x - 1"))
  (should (string= (litex-lisp2latex-all
		    '(setq x (- t 6 (* 5 60 (/ x)))))
		   "x = t - 6 -  \\left( 5 \\times 60\\frac1{x} \\right) "))
  ;; this one is failing
  (should (string= (litex-lisp2latex-all
		    '(setq x (- t 6 (* 5 60 (/ x 2)))))
		   "x = t - 6 -  \\left( 5 \\times 60 \\left( \\frac{x}{2} \\right)  \\right) "))
  (should (string= (litex-lisp2latex-all
		    '(fun x 2 (/ 5 (+ 6 7) x)))
		   "\\text{fun}\\left(x,2,\\frac{5}{ \\left( 6 + 7 \\right) x}\\right)")))
