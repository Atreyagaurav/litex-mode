
(ert-deftest litex-format-float-test ()
  (let ((litex-format-float-string "%.3f"))
    (should (string= (litex-format-float 2.0) "2.000"))
    (should (string= (litex-format-float 2.1234) "2.123"))
    (should (string= (litex-format-float 2.1236) "2.124")))
  (let ((litex-format-float-string "%.2f"))
    (should (string= (litex-format-float 2.0) "2.00")))
  (let ((litex-format-float-string "%d"))
    (should (string= (litex-format-float 2.0) "2")))
  (let ((litex-format-float-string "%.2e"))
    (should (string= (litex-format-float 2.0) "2.00e+00"))))


(ert-deftest litex-format-float-test ()
  (let ((litex-format-float-string "%.3f"))
    (should (string= (litex-format-float 2.0) "2.000"))
    (should (string= (litex-format-float 2.1234) "2.123"))
    (should (string= (litex-format-float 2.1236) "2.124")))
  (let ((litex-format-float-string "%.2f"))
    (should (string= (litex-format-float 2.0) "2.00"))))


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
    (should (string= (litex-format-args-* '(x y)) "xy")))


(ert-deftest litex-format-args-/-test ()
    (should (string= (litex-format-args-/ '(1 2)) "\\frac{1}{2}"))
    (should (string= (litex-format-args-/ '(x 2)) "\\frac{x}{2}"))
    (should (string= (litex-format-args-/ '(1 y)) "\\frac{1}{y}"))
    (should (string= (litex-format-args-/ '(x y)) "\\frac{x}{y}"))
    (should (string= (litex-format-args-/ '(2)) "\\frac1{2}"))
    (should (string= (litex-format-args-/ '(x)) "\\frac1{x}")))


(ert-deftest litex-format-args-1+-test ()
    (should (string= (litex-format-args-1+ '(1)) "1 + 1"))
    (should (string= (litex-format-args-1+ '(x)) "x + 1")))


(ert-deftest litex-format-args-expt-test ()
    (should (string= (litex-format-args-expt '(1 2)) "1^{2}"))
    (should (string= (litex-format-args-expt '(x 2)) "x^{2}"))
    (should (string= (litex-format-args-expt '((+ 1 2) 2)) "(1 + 2)^{2}"))
    (should (string= (litex-format-args-expt '((* 2 x) 2)) "(2x)^{2}")))


