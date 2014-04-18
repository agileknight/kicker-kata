(ns kicker.core-test-acceptance
  (:require [kicker.core :refer :all]
            [kicker.core-test :refer :all]
            [clojure.test :refer :all]))

(defn make-capturer
  []
  (let [events (atom [])]
    (reify
      EventListener
      (fire [this event] (swap! events conj event))
      EventReader
      (latest-events [this] (let [result @events]
                              (reset! events [])
                              result)))))

(def ^:dynamic *score-event-capturer*)

(defn setup-score-event-capturer
  [f]
  (binding [*score-event-capturer* (make-capturer)]
    (f)))

(def ^:dynamic *stats*)

(defn setup-statistics-instance
  [f]
  (binding [*stats* (make-statistics *score-event-capturer*)]))

(use-fixtures :each setup-score-event-capturer setup-statistics-instance)

(deftest acceptance-test
  (testing "Goal triggers score."
    (goal *stats* :black)
    (goal *stats* :black)
    (is (= [{:black 1 :white 0}
            {:black 2 :white 0}] (latest-events *score-event-capturer*))))
  '(testing "Game ends when one team gets 6 goals"
    (let [stats (make-statistics *score-event-capturer*)]
      (is (= {:type "score" :content {:black 1 :white 0}} (last (goals stats (take 7 (repeat :black))))))))
  '(testing "Prints player stats after team wins"
     (let [stats (make-statistics)]
       (register stats :black :offense "Joe")
       (register stats :black :defense "Jack")
       (register stats :white :offnse "Daniel")
       (register stats :white :defense "Dennis")
       (is (= {:type "player-ranking" :content {"Joe" 1 "Jack" 1 "Daniel" 0 "Dennis" 0}}
              (last (wins-game stats :black)))))))
