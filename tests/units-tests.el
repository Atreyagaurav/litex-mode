;; Run all tests using (ert t)

;; conversion tests
(ert-deftest units-convert-test ()
  (should (= (units-convert 1 "ft" "m") 0.3048))
  (should (= (units-convert 1 "m" "ft") 3.2808399))
  (should (= (units-convert 1 "kg" "g") 1e3))
  (should (= (units-convert 1 "hour" "seconds") (* 60 60)))
  (should (= (units-convert 1 "day" "hour") 24)))
