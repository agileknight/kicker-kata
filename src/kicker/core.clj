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

(defmacro call
  [map key & args]
  `((~key ~map) ~@args))

(defn increase-in-map
  [map key]
  (assoc map key (+ 1 (get map key))))

(defn make-player-board
  []
  (let [board (atom {})]
    {:register (fn [name] (swap! board assoc name 0))

     :end-of-game (fn [winner-team]
                    (do (swap! board increase-in-map (:offense winner-team))
                        (swap! board increase-in-map (:defense winner-team))))
     :current-high-score (fn [] @board)}))

(defn make-team-info
  []
  (let [team-state (atom {})]
    {:register (fn [team position player-name]
                 (swap! team-state assoc-in [team position] player-name))
     :current-teams (fn [] @team-state)}))

(defn make-score-counter
  []
  (let [score (atom {:black 0 :white 0})]
    {:goal (fn [team] (swap! score increase-in-map team))
     :reset (fn [] (reset! score {:black 0 :white 0}))
     :current-score (fn [] @score)}))

(defn make-game-listener
  [end-of-game-callback]
  {:score-changed (fn [new-score]
                    (cond (>= (:black new-score) 6) (end-of-game-callback :black)
                          (>= (:white new-score) 6) (end-of-game-callback :white)))})

(defn make-statistics
  [score-event-listener
   player-stats-event-listener]
  (let [score-counter (make-score-counter)
        team-info (make-team-info)
        player-board (make-player-board)
        game-listener (make-game-listener
                       (fn [winning-team]
                         (do (call :reset score-counter)
                             (call :end-of-game player-board
                                   (winning-team (call :current-teams team-info)))
                             (fire player-stats-event-listener
                                   (call :current-high-score player-board)))))]
    (reify
      StatisticsModule
      (goal [this team]
        (do (call :goal score-counter team)
            (fire score-event-listener (call :current-score score-counter))
            (call :score-changed game-listener (call :current-score score-counter))))
      (register [this team position player-name]
        (do (call :register player-board player-name)
            (call :register team-info team position player-name))))))

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
