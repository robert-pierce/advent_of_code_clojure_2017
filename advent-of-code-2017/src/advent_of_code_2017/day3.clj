(ns advent-of-code-2017.day3
  (:require [clojure.string :as str]))

(declare move-right)
(declare move-left)
(declare move-up)
(declare move-down)


(defn move-down
  [current-point spiral-numbers numbers-left]
  (let [current-x (:x current-point)
        current-y (:y current-point)
        moved-down {:x current-x :y (dec current-y)}
        right-moved-down {:x (inc (:x moved-down)) :y (:y moved-down)}
        new-spiral-numbers (assoc spiral-numbers (hash moved-down) moved-down)]
    (if (= numbers-left 1)
      moved-down
      (if (get spiral-numbers (hash right-moved-down))
        (move-down moved-down new-spiral-numbers (dec numbers-left))
        (move-right moved-down new-spiral-numbers (dec numbers-left))))))

(defn move-left
  [current-point spiral-numbers numbers-left]
  (let [current-x (:x current-point)
        current-y (:y current-point)
        moved-left {:x (dec current-x) :y current-y}
        down-moved-left {:x (:x moved-left) :y (dec (:y moved-left))}
        new-spiral-numbers (assoc spiral-numbers (hash moved-left) moved-left)]
    (if (= numbers-left 1)
      moved-left
      (if (get spiral-numbers (hash down-moved-left))
        (move-left moved-left new-spiral-numbers (dec numbers-left))
        (move-down moved-left new-spiral-numbers (dec numbers-left))))))

(defn move-up
  [current-point spiral-numbers numbers-left]
  (let [current-x (:x current-point)
        current-y (:y current-point)
        moved-up {:x current-x :y (inc current-y)}
        left-moved-up {:x (dec (:x moved-up)) :y (:y moved-up)}
        new-spiral-numbers (assoc spiral-numbers (hash moved-up) moved-up)]
    (if (= numbers-left 1)
      moved-up
      (if (get spiral-numbers (hash left-moved-up))
        (move-up moved-up new-spiral-numbers (dec numbers-left))
        (move-left moved-up new-spiral-numbers (dec numbers-left))))))

(defn move-right
  [current-point spiral-numbers numbers-left]
  (let [current-x (:x current-point)
        current-y (:y current-point)
        moved-right {:x (inc current-x) :y current-y}
        above-moved-right {:x (:x moved-right) :y (inc (:y moved-right))}
        new-spiral-numbers (assoc spiral-numbers (hash moved-right) moved-right)]
    (if (= numbers-left 1)
      moved-right
      (if (get spiral-numbers (hash above-moved-right))
        (move-right moved-right new-spiral-numbers (dec numbers-left))
        (move-up moved-right new-spiral-numbers (dec numbers-left))))))

(defn get-spiral-number-coordinate
  ([stopping-number]  
   (let [new-point {:x 0 :y 0}
         spiral-coordinate 
         (if (= stopping-number 1)
           new-point (move-right new-point (hash-map (hash new-point) new-point) 
                                 (dec stopping-number)))]
     (if (= stopping-number 1)
       new-point
       (move-right new-point (hash-map (hash new-point) new-point) (dec stopping-number))))))

(defn day-3-part-1
  [input]
  {:status 200 :body (str "derk a derk") })
