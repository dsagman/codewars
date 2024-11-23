(require '[clojure.java.io :as io])
(ns spellingbee-clojure.core
  (:gen-class))

(defn read-dictionary
  "Reads the dictionary file and returns a list of words."
  []
  (let [dictionary-file "/usr/share/dict/words"]
    (with-open [rdr (io/reader dictionary-file)]
      (doall (line-seq rdr)))))


(defn filter-dictionary
  "Filters the dictionary for words that are more than 4 characters long."
  [dictionary]
  (filter #(> (count %) 4) dictionary))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let center-letter "a"
  let outer-letters "todrymi"
  (println "Hello, World!"))
  (let [dictionary (read-dictionary)] 
    (println (count (filter-dictionary dictionary)))))

