(ns kicker.web-test
  (:require [kicker.web :refer :all]
            [clojure.test :refer :all]))

(deftest test-matches
  (testing "matches a given uri on a given route"
    (is (matches? "/" "/"))
    (is (matches? "/a" "/a"))
    (is (not (matches? "/a" "/b")))
    (is (matches? "/dothing/:param" "/dothing/withthat"))
    (is (not (matches? "/do_something_else/:param" "/dothing/withthat")))
    )
  )
