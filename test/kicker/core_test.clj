(ns kicker.core-test
  (:require [clojure.test :refer :all]
            [kicker.core :refer :all]))

(deftest events-matching-test
  (testing "Single event returned when pattern is empty."
    (let [event-captor (create-event-captor) bus (create-bus [event-captor])]
      ((:event bus) "event")
      (is (= ["event"] ((:events-matching event-captor) ""))))))
