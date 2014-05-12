(ns kicker.web (:require [ kicker.core :refer :all]))

(defonce score-cap (make-capturer))
(defonce stats-cap (make-capturer))
(defonce stats (make-statistics score-cap stats-cap))

(defn dispatch
  [url]
  (str url))

(defn handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (dispatch (:uri request))})

(defn matches-part?
  [a b]
  (cond (= a b) true
        (= \: (first a)) true))

(defn arr-matches?
  [[route-first & route-rest] [uri-first & uri-rest]]
  (if (matches-part? route-first uri-first)
    (if (and (not (nil? route-rest))
             (not (nil? uri-rest)))
      (arr-matches? route-rest uri-rest)
      true)
    false))

(defn matches? [route uri] (let [ route-array (clojure.string/split route #"/")
                                 uri-array (clojure.string/split uri #"/")]
                             (arr-matches? route-array uri-array)))
