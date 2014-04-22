(ns kicker.core
  (:gen-class))

(defprotocol EventReader
  "Read latest events."
  (latest-events [this]))

(defprotocol EventListener
  "Listens to events."
  (fire [this event]))

(defn increase-in-map
  [map key]
  (assoc map key (+ 1 (get map key))))

(defn increase-score
  [score-map team]
  (increase-in-map score-map team))

(defn make-player-board
  []
  (let [board (atom {})]
    {:register (fn [name] (swap! board assoc name 0))
     :win (fn [name] (swap! board increase-in-map name))
     }
    ))

(defprotocol RegistrationListener
  "Listens to registration events."
  (register [this team position player-name]))

(defprotocol TeamInfo
  "Current team composition."
  (current-teams [this]))

(defn make-team-info
  []
  (let [team-state (atom {})]
    (reify RegistrationListener
      (register [this team position player-name] (swap! team-state assoc-in [team position] player-name))
      TeamInfo
    (current-teams [this] @team-state))))

(defn make-score-counter
  [score-event-listener]
  (let [score (atom {:black 0 :white 0})]
    {:score (fn [] @score)
     :goal (fn [team] (do (swap! score increase-in-map team)
                         (fire score-event-listener @score)
                         @score))
     :reset (fn [] (reset! score {:black 0 :white 0}))}))

(defn make-game-listener
  [callback]
  {:score-changed (fn [new-score] (if (or (>= (:black new-score) 6) (>= (:white new-score) 6))
                                   (callback)))})
(defprotocol KickerEventListener
  "Listens to kicker events and returns the resulting events."
  (goal [this team]))

(defn make-statistics
  [score-event-listener
   player-stats-event-listener]
  (let [score-counter (make-score-counter score-event-listener)
        game-listener (make-game-listener (fn [] ((:reset score-counter))))]
    (reify
      KickerEventListener
      (goal [this team] (let [new-score ((:goal score-counter) team)]
                          ((:score-changed game-listener) new-score)
                          [{:type "score" :content new-score}]))
      RegistrationListener
      (register [this team position player-name] :todo))))
