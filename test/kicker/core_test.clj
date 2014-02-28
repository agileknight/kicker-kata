(ns kicker.core-test
  (:require [clojure.test :refer :all]
            [kicker.core :refer :all]))

(deftest events-matching-test
  (testing "All events returned when pattern is :all."
    (let [event-captor (create-event-captor)
          bus (create-bus [event-captor])]
      ((:event bus) "red" bus)
      ((:event bus) "blue" bus)
      (is (= ["red" "blue"] ((:events-matching event-captor) :all)))))
  (testing "Matching events returned when pattern is given."
    (let [event-captor (create-event-captor)
          bus (create-bus [event-captor])]
      ((:event bus) "red" bus)
      ((:event bus) "blue" bus)
      (is (= ["blue"] ((:events-matching event-captor) "blue"))))))

(deftest integration-test
  (testing "First goal triggers score."
    (let [event-captor (create-event-captor)
          kicker (create-kicker)
          bus (create-bus [event-captor kicker])]
      ((:event bus) "goal:black" bus)
      (is (= ["score:{'black':1, 'white':0}"] ((:events-matching event-captor) "score"))))))
