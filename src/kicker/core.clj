(ns kicker.core
  (:gen-class))

(defn start-matches?
  [input [& patterns]]
  (true? (some #(.startsWith input %) patterns)))

(defn create-event-captor
  []
  (let [captured (atom [])]
    {:event (fn [event bus] (swap! captured conj event))
     :events-matching (fn [pattern] (filter #(if (= :all pattern) true (start-matches? % [pattern])) @captured))
     :wants? (fn [event] true)}))

(defn create-bus
  [[& listeners]]
  {:event (fn [event bus] (doseq [listener listeners]
                       (if ((:wants? listener) event)
                         ((:event listener) event bus))))})

(defn increase-score
  [score team]
  (assoc score team (+ 1 (get score team))))

(defn make-score-counter
  []
  (let [score (atom {:black 0 :white 0})]
    {:score (fn [] @score)
     :goal (fn [team] (swap! score increase-score team))}))

(defn make-statistics
  []
  (let [score-counter (make-score-counter)]
    {:goal (fn [team] (do ((:goal score-counter) team) [{:type "score" :content ((:score score-counter))}]))}))

(defn parse-goal-event
  [event]
  (if (.contains event "black") :black :white))

(defn print-score
  [score]
  (format "score:{'black':%d, 'white':%d}" (:black score) (:white score)))

(def event-printers
  {"score" print-score})

(defn print-event
  [event]
  ((get event-printers (:type event)) (:content event)))

(defn create-kicker
  []
  {:event (fn [event bus] ((:event bus) "score:{'black':1, 'white':0}" bus))
   :wants? (fn [event] (start-matches? event ["goal"]))})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
