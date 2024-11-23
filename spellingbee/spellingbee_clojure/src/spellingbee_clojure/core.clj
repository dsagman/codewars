(ns spellingbee-clojure.core
  (:gen-class))

(defn -read-dictionary
  "Reads the dictionary file and returns a list of words."
  []
  (let [dictionary-file "/usr/share/dict/words"]
    (with-open [rdr (clojure.java.io/reader dictionary-file)]
      (doall (line-seq rdr)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
  (println (-read-dictionary))
