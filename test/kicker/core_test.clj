(ns kicker.core-test
  (:require [clojure.test :refer :all]
            [kicker.core :refer :all]))

(deftest events-matching-test
  (testing "All events returned when pattern is :all."
    (let [event-captor (create-event-captor) bus (create-bus [event-captor])]
      ((:event bus) "red")
      ((:event bus) "blue")
      (is (= ["red" "blue"] ((:events-matching event-captor) :all)))))
  (testing "Matching events returned when pattern is given."
    (let [event-captor (create-event-captor) bus (create-bus [event-captor])]
      ((:event bus) "red")
      ((:event bus) "blue")
      (is (= ["blue"] ((:events-matching event-captor) "blue"))))))
