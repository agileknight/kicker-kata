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
  [score-map team]
  (increase-in-map score-map team))

(defn increase-in-map
  [map key]
  (assoc map key (+ 1 (get map key))))

(defn make-player-board
  []
  (let [board (atom {})]
    {:register (fn [name] (swap! board assoc name 0))
     :win (fn [name] (swap! board increase-in-map name))
     }
    ))

(defn make-score-counter
  []
  (let [score (atom {:black 0 :white 0})]
    {:score (fn [] @score)
     :goal (fn [team] (swap! score increase-in-map team))
     :reset (fn [] (reset! score {:black 0 :white 0}))}))

(defn make-game-listener
  [callback]
  {:score-changed (fn [new-score] (if (or (>= (:black new-score) 6) (>= (:white new-score) 6))
                                   (callback)))})
(defprotocol KickerEventListener
  "Listens to kicker events and returns the resulting events."
  (goal [this team])
  (register [this team position player]))

(deftype Statistics-Impl [score-counter game-listener]
  KickerEventListener
  (goal [this team] (let [new-score ((:goal score-counter) team)]
                 ((:score-changed game-listener) new-score)
                 [{:type "score" :content new-score}])))

(defn make-statistics
  []
  (let [score-counter (make-score-counter)
        game-listener (make-game-listener (fn [] ((:reset score-counter))))]
    (Statistics-Impl. score-counter game-listener)))

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

(defn is-goal-event?
  [event]
  (start-matches? event ["goal"]))

(defn handle-goal
  [stats event bus]
  (doseq [out-event ((:goal stats) (parse-goal-event event))]
    ((:event bus) (print-event out-event) bus)))

(defn create-kicker
  []
  (let [statistics (make-statistics)]
    {:event (fn [event bus] (cond (is-goal-event? event) (handle-goal statistics event bus) ))
    :wants? (fn [event] (is-goal-event? event))}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
