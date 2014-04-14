(ns kicker.core-test-acceptance
  (:require [kicker.core :refer :all]
            [kicker.core-test :refer :all]
            [clojure.test :refer :all]))

(defn make-capturer
  []
  (let [events (atom [])]
    (reify EventReader
      (latest-events [this] (let [result @events] (reset! events []))))))

(def ^:dynamic *score-event-capturer*)

(defn setup-score-event-capturer
  []
  (binding [*score-event-capturer* make-capturer]))

(use-fixtures :each setup-score-event-capturer)

(deftest acceptance-test
  (testing "Goal triggers score."
    (let [stats (make-statistics *score-event-capturer*)]
      (goal stats :black)
      (goal stats :black)
      (is (= [{:type "score" :content {:black 1 :white 0}}
              {:type "score" :content {:black 2 :white 0}}] (latest-events *score-event-capturer*)))))
  (testing "Game ends when one team gets 6 goals"
    (let [stats (make-statistics)]
      (is (= {:type "score" :content {:black 1 :white 0}} (last (goals stats (take 7 (repeat :black))))))))
  '(testing "Prints player stats after team wins"
    (let [stats (make-statistics)]
      (register stats :black :offense "Joe")
      (register stats :black :defense "Jack")
      (register stats :white :offnse "Daniel")
      (register stats :white :defense "Dennis")
      (is (= {:type "player-ranking" :content {"Joe" 1 "Jack" 1 "Daniel" 0 "Dennis" 0}}
             (last (wins-game stats :black)))))))
