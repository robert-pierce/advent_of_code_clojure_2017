(ns advent-of-code-2017.day6
  (:require [clojure.string :as str]))


(defn get-curr-index
  [n size off-set]
  (let [curr-index-raw (+ n off-set)]
    (if (>= curr-index-raw size)
      (mod curr-index-raw size)
      curr-index-raw)))

(defn update-memory-bank
  [memory-banks n off-set]
  (let [curr-index (get-curr-index n (count memory-banks) off-set)
        curr-index-val (get memory-banks curr-index)]
    (assoc memory-banks curr-index (inc curr-index-val))))

(defn redistribute-max-bank
  [old-memory-banks max-bank-index max-bank-val]
  (loop [memory-banks (assoc old-memory-banks max-bank-index 0)
         n 1]
    (if (> n max-bank-val)
      memory-banks
      (recur (update-memory-bank memory-banks n max-bank-index) (inc n)))))

(defn get-max-bank-index
  "Returns the index of the memory bank with the most blocks.
  If there is  a tie then we take lowest indexed bank"
  [memory-banks]
  (first (keep-indexed 
          #(if (= (apply max memory-banks) %2) %1)  
        memory-banks)))

(defn update-memory-banks
  [old-memory-banks]
  (let [max-bank-index (get-max-bank-index old-memory-banks)]
    (redistribute-max-bank old-memory-banks max-bank-index (get old-memory-banks max-bank-index))))

(defn process-move
  [analysis-map]
  (let [old-memory-banks (:memory-banks analysis-map)
        old-previous-states (:previous-states analysis-map)
        updated-previous-states (assoc old-previous-states (hash old-memory-banks) old-memory-banks) 
        updated-memory-banks (update-memory-banks old-memory-banks)]
    {:memory-banks updated-memory-banks :previous-states updated-previous-states}))

(defn process-move-part-2
  [analysis-map current-move]
  (let [old-memory-banks (:memory-banks analysis-map)
        old-previous-states (:previous-states analysis-map)
        updated-previous-states (assoc old-previous-states 
                                  (hash old-memory-banks) {:old-state old-memory-banks :current-move current-move }) 
        updated-memory-banks (update-memory-banks old-memory-banks)]
    {:memory-banks updated-memory-banks :previous-states updated-previous-states}))

(defn process-debug-analysis
  [memory-banks]
  (loop [analysis-map {:memory-banks memory-banks :previous-states {} }
         total-moves 0]
    (if (get (:previous-states analysis-map) (hash (:memory-banks analysis-map)))
      total-moves
      (recur (process-move analysis-map) (inc total-moves)))))

(defn process-debug-analysis-part-2
  [memory-banks]
  (loop [analysis-map {:memory-banks memory-banks :previous-states {} }
         total-moves 0]
    (if-let [previous-occurance (get (:previous-states analysis-map) (hash (:memory-banks analysis-map)))]
      (- total-moves (:current-move previous-occurance))
      (recur (process-move-part-2 analysis-map total-moves) (inc total-moves)))))

(defn day-6-part-1
 [body]
 (let [strBody (slurp body)
       memory-banks-str (str/split strBody #"\t")
       memory-banks (vec (map #(Integer/parseInt %) memory-banks-str))
       total-moves (process-debug-analysis memory-banks)]
   {:status 200 :body (str "The total moves are " total-moves)}))

(defn day-6-part-2
 [body]
 (let [strBody (slurp body)
       memory-banks-str (str/split strBody #"\t")
       memory-banks (vec (map #(Integer/parseInt %) memory-banks-str))
       loop-size (process-debug-analysis-part-2 memory-banks)]
   {:status 200 :body (str "The loop size between matching states is " loop-size)}))

