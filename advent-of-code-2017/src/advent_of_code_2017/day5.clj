(ns advent-of-code-2017.day5
  (:require [clojure.string :as str]))

(defn process-move
  [instruction-map]
  (let [instructions (:instructions instruction-map)
        current-pos (:pos instruction-map)
        current-value (Integer/parseInt (get instructions current-pos))
        updated-instructions (assoc instructions current-pos (str (inc current-value)))
        next-pos (+ current-pos current-value)]
    {:instructions updated-instructions :pos next-pos}))

(defn update-value-for-part-2
  [value]
  (if (> value 2)
    (dec value)
    (inc value)))

(defn process-move-part-2
  [instruction-map]
  (let [instructions (:instructions instruction-map)
        current-pos (:pos instruction-map)
        current-value (Integer/parseInt (get instructions current-pos))
        updated-instructions (assoc instructions current-pos (str (update-value-for-part-2 current-value)))
        next-pos (+ current-pos current-value)]
    {:instructions updated-instructions :pos next-pos}))

(defn process-instructions
  [instructions]
  (loop [instruction-map {:instructions instructions :pos 0}
         total-moves 0]
    (if (>= (:pos instruction-map) (count (:instructions instruction-map)))
      total-moves
      (recur (process-move instruction-map) (inc total-moves)))))

(defn process-instructions-part-2
  [instructions]
  (loop [instruction-map {:instructions instructions :pos 0}
         total-moves 0]
    (if (>= (:pos instruction-map) (count (:instructions instruction-map)))
      total-moves
      (recur (process-move-part-2 instruction-map) (inc total-moves)))))

(defn day-5-part-1
 [body]
 (let [strBody (slurp body)
       instructions (str/split strBody #"\n")
       total-moves (process-instructions instructions)]
   {:status 200 :body (str "The total number of moves is " total-moves )}))


(defn day-5-part-2
 [body]
 (let [strBody (slurp body)
       instructions (str/split strBody #"\n")
       total-moves (process-instructions-part-2 instructions)]
   {:status 200 :body (str "The total number of moves is " total-moves )}))
