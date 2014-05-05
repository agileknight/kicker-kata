(ns kicker.web (:require [ kicker.core :refer :all]))

(defonce score-cap (make-capturer))
(defonce stats-cap (make-capturer))
(defonce stats (make-statistics score-cap stats-cap))

(defn handler [request]
  (let []
    (register stats :black :offense "Joe")
    (register stats :black :defense "Jack")
    (register stats :white :offense "Daniel")
    (register stats :white :defense "Dennis")
    (goal stats :black)
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (str  (latest-events score-cap) (latest-events stats-cap))}))
