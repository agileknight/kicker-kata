(ns kicker.core)

(defprotocol EventReader
  "Read latest events."
  (latest-events [this]))

(defprotocol EventListener
  "Listens to events."
  (fire [this event]))

(defprotocol StatisticsModule
  "API of the statistics module."
  (register [this team position player-name])
  (goal [this team]))

(defn increase-in-map
  [map key]
  (assoc map key (+ 1 (get map key))))

(defn increase-score
  [score-map team]
  (increase-in-map score-map team))

(defn make-player-board
  []
  (let [board (atom {})]
    {:register (fn [team position player-name] (swap! board assoc name 0))

     :end-of-game (fn [winner-team] (do (swap! board increase-in-map (:offense winner-team))
                                       (swap! board increase-in-map (:defense winner-team))))}))

(defn make-team-info
  []
  (let [team-state (atom {})]
    ({:register (fn [team position player-name] (swap! team-state assoc-in [team position] player-name))
      :current-teams (fn [] @team-state)})))

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

(defn make-statistics
  [score-event-listener
   player-stats-event-listener]
  (let [score-counter (make-score-counter score-event-listener)

        game-listener (make-game-listener (fn [] ((:reset score-counter))))]
    (reify
      StatisticsModule
      (goal [this team] (let [new-score ((:goal score-counter) team)]
                          ((:score-changed game-listener) new-score)))
      (register [this team position player-name] :todo))))
