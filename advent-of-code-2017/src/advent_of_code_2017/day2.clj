(ns advent-of-code-2017.day2
  (:require [clojure.string :as str]))

(defn findRowChecksum
  "Takes in a row of strings, parses them as ints
  and finds the difference between the max and min
  of the ints in that row."
  [row]
  (let [parsedRow (map #(Integer/parseInt %) row)
        maximum (apply max parsedRow)
        minimum (apply min parsedRow)]
  (- maximum minimum)))


(defn day-2-part-1
  "blah blah"
  [body]
  (let [strBody (slurp body)
        splitBody (str/split strBody #"\n")
        splitRows  (map #(str/split % #"\t") splitBody)
        rowChecksums (map findRowChecksum splitRows)]
    {:status 200 :body (str (reduce + rowChecksums))}))
