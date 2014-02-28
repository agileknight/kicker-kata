(ns kicker.core
  (:gen-class))

(defn create-event-captor
  []
  (let [captured (atom [])]
    {:event (fn [event] (swap! captured conj event))
     :events-matching (fn [pattern] @captured)}))

(defn create-bus
  [[& listeners]]
  {:event (fn [event] (doseq [listener listeners]
                              ((:event listener) event)))})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
