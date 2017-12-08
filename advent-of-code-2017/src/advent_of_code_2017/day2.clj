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

(defn findRowDivsum
  [row]
  (let [parsedRow (map #(Integer/parseInt %) row)
        rawResult (for [x parsedRow y parsedRow]
                    (if (and  (not (= x y)) (= (mod x y ) 0))
                      (/ x y)))]
    (first (filter #(not (nil? %)) rawResult))))


(defn day-2-part-1
  "Finds the sum of the differences between the 
  maximum and minimum in each row"
  [body]
  (let [strBody (slurp body)
        splitBody (str/split strBody #"\n")
        splitRows  (map #(str/split % #"\t") splitBody)
        rowChecksums (map findRowChecksum splitRows)]
    {:status 200 :body (str (reduce + rowChecksums))}))

(defn day-2-part-2
  [body]
  (let [strBody (slurp body)
        splitBody (str/split strBody #"\n")
        splitRows (map #(str/split % #"\t") splitBody)
        rowDivsums (map findRowDivsum splitRows)]
    (print splitBody)
    {:status 200 :body (str (reduce + rowDivsums))}))
