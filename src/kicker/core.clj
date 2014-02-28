(ns kicker.core
  (:gen-class))

(defn create-event-captor
  []
  (let [captured (atom [])]
    {:event (fn [event] (swap! captured conj event))
     :events-matching (fn [pattern] (filter #(if (= :all pattern) true (.startsWith % pattern)) @captured))
     :wants? (fn [event] true)}))

(defn create-bus
  [[& listeners]]
  {:event (fn [event] (doseq [listener listeners]
                       (if ((:wants? listener) event)
                         ((:event listener) event))))})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
