(ns kicker.core-test
  (:require [clojure.test :refer :all]
            [kicker.core :refer :all]))

(defn goals
  [stats teams]
  (doall (map #(goal stats %) teams)))

(defn wins-game
  [stats team]
  (goals stats (take 6 (repeat team))))

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
  (binding [*stats* (make-statistics *score-event-capturer*)]
    (f)))

(defn setup
  [f]
  ((compose-fixtures setup-score-event-capturer setup-statistics-instance) f))

(defmacro with-fixtures
  [fixtures doc & body]
  `(testing ~doc (~fixtures (fn [] (do ~@body)))))

(deftest acceptance-test
  (with-fixtures setup "Goal triggers score."
    (goal *stats* :black)
    (goal *stats* :black)
    (is (= [{:black 1 :white 0}
            {:black 2 :white 0}] (latest-events *score-event-capturer*))))
  (with-fixtures setup "New game starts when one team gets 6 goals"
    (goals *stats* (take 7 (repeat :black)))
    (is (= {:black 1 :white 0} (last (latest-events *score-event-capturer*)))))
  '(testing "Prints player stats after team wins"
     (let [stats (make-statistics)]
       (register stats :black :offense "Joe")
       (register stats :black :defense "Jack")
       (register stats :white :offnse "Daniel")
       (register stats :white :defense "Dennis")
       (is (= {:type "player-ranking" :content {"Joe" 1 "Jack" 1 "Daniel" 0 "Dennis" 0}}
              (last (wins-game stats :black)))))))
