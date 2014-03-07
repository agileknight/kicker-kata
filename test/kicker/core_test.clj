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

(deftest increase-score-test
  (testing "Increases score by 1."
    (is (= {:black 1 :white 0} (increase-score {:black 0 :white 0} :black)))
    (is (= {:black 2 :white 0} (increase-score {:black 1 :white 0} :black)))
    (is (= {:black 0 :white 3} (increase-score {:black 0 :white 2} :white)))))

(deftest score-counter-test
  (testing "Initially has zero score."
    (let [counter (make-score-counter)]
      (is (= {:black 0 :white 0} ((:score counter))))))
  (testing "One black goal"
    (let [counter (make-score-counter)]
      ((:goal counter) :black)
      (is (= {:black 1 :white 0} ((:score counter)))))))

(deftest statistics-test
  (testing "Goal triggers score."
    (let [stats (make-statistics)]
      (is (= [{:type "score" :content {:black 1 :white 0}}] ((:goal stats) :black)))
      (is (= [{:type "score" :content {:black 2 :white 0}}] ((:goal stats) :black))))))

(deftest goal-event-parsing-test
  (testing "parse goal events correctly"
    (is (= :black (parse-goal-event "goal:black")))
    (is (= :white (parse-goal-event "goal:white")))))

(deftest event-printing-test
  (testing "print score event correctly"
    (is (= "score:{'black':1, 'white':3}" (print-event {:type "score" :content {:black 1 :white 3}})))))

(deftest integration-test
  (testing "First goal triggers score."
    (let [event-captor (create-event-captor)
          kicker (create-kicker)
          bus (create-bus [event-captor kicker])]
      ((:event bus) "goal:black" bus)
      (is (= ["score:{'black':1, 'white':0}"] ((:events-matching event-captor) "score")))))
  '(testing "Kicker counts score for team."
    (let [event-captor (create-event-captor)
          kicker (create-kicker)
          bus (create-bus [event-captor kicker])]
      ((:event bus) "goal:black" bus)
      ((:event bus) "goal:black" bus)
      (is (= "score:{'black':2, 'white':0}" (last ((:events-matching event-captor) "score"))))))
  '(testing "Next"
    (is (= 1 1))))
