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
      (is (= {:black 1 :white 0} ((:score counter))))))
  (testing "Reset resets score"
    (let [counter (make-score-counter)]
      ((:goal counter) :black)
      ((:reset counter))
      (is (= {:black 0 :white 0} ((:score counter)))))))

(deftest player-board-test
  (testing "Registers a player on the board"
    (let [board (make-player-board)]
      (is (= {"John" 0} ((:register board) "John")))))
  (testing "Player wins a game"
    (let [board (make-player-board)]
      ((:register board) "John")
      (is (= {"John" 1} ((:win board) "John")))))
  )

(defn make-callback-mock
  []
  (let [was-called (atom false)]
    {:call (fn [] (reset! was-called true))
     :was-called? (fn [] @was-called)}))

(deftest game-listener-test
  (testing "Does nothing when game does not end"
    (let [callback (make-callback-mock)
          game-listener (make-game-listener (:call callback))]
      ((:score-changed game-listener) {:black 5 :white 5})
      (is (false? ((:was-called? callback))))))
  (testing "Resets score counter when game ends"
    (let [callback (make-callback-mock)
          game-listener (make-game-listener (:call callback))]
      ((:score-changed game-listener) {:black 6 :white 4})
      (is (true? ((:was-called? callback)))))))

(defn goals
  [stats teams]
  (->> (map #((:goal stats) %) teams)
       (reduce conj)
       flatten))

(defn wins-game
  [stats team]
  (goals stats (take 6 (repeat team))))

(deftest acceptance-test
  (testing "Goal triggers score."
    (let [stats (make-statistics)]
      (is (= [{:type "score" :content {:black 1 :white 0}}] ((:goal stats) :black)))
      (is (= [{:type "score" :content {:black 2 :white 0}}] ((:goal stats) :black)))))
  (testing "Game ends when one team gets 6 goals"
    (let [stats (make-statistics)]
      (is (= {:type "score" :content {:black 1 :white 0}} (last (goals stats (take 7 (repeat :black))))))))
  '(testing "Prints player stats after team wins"
    (let [stats (make-statistics)]
      ((:register stats) :black :offense "Joe")
      ((:register stats) :black :defense "Jack")
      ((:register stats) :white :offnse "Daniel")
      ((:register stats) :white :defense "Dennis")
      (is (= {:type "player-ranking" :content {"Joe" 1 "Jack" 1 "Daniel" 0 "Dennis" 0}}
             (last (wins-game stats :black)))))))

(deftest goal-event-parsing-test
  (testing "parse goal events correctly"
    (is (= :black (parse-goal-event "goal:black")))
    (is (= :white (parse-goal-event "goal:white")))))

(deftest event-printing-test
  (testing "print score event correctly"
    (is (= "score:{'black':1, 'white':3}" (print-event {:type "score" :content {:black 1 :white 3}})))))

(deftest system-test
  (testing "First goal triggers score."
    (let [event-captor (create-event-captor)
          kicker (create-kicker)
          bus (create-bus [event-captor kicker])]
      ((:event bus) "goal:black" bus)
      (is (= ["score:{'black':1, 'white':0}"] ((:events-matching event-captor) "score")))))
  (testing "Kicker counts score for team."
    (let [event-captor (create-event-captor)
          kicker (create-kicker)
          bus (create-bus [event-captor kicker])]
      ((:event bus) "goal:black" bus)
      ((:event bus) "goal:black" bus)
      (is (= "score:{'black':2, 'white':0}" (last ((:events-matching event-captor) "score")))))))
