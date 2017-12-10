(ns advent-of-code-2017.day3
  (:require [clojure.string :as str]))

(declare move-right)
(declare move-left)
(declare move-up)
(declare move-down)

(def move-data (atom {:current-point {:x 0 :y 0} :moves-to-make 1 :moves-left 1 :direction "right"}))

(def spiral-sums (atom {(hash {:x 0 :y 0}) {:x 0 :y 0 :value 1}}))

(defn abs
  "returns the absolute value of n"
  [n]
  (if (< n 0) (-' n) n))

(defn calc-distance-from-origin
  [current-point] 
  (+ (abs (:x current-point)) (abs (:y current-point))))

(defn move-down
  [move-data]
  (let [current-point (:current-point move-data)
        current-x (:x current-point)
        current-y (:y current-point)
        moved-down {:x current-x :y (dec current-y)}
        moves-to-make (:moves-to-make move-data)
        moves-left (dec (:moves-left move-data))]
    (if (= moves-left 0)
      {:current-point moved-down :moves-to-make (inc moves-to-make)
       :moves-left (inc moves-to-make) :direction "right"}
      {:current-point moved-down :moves-to-make moves-to-make :moves-left moves-left :direction "down"})))

(defn move-left
  [move-data]
  (let [current-point (:current-point move-data)
        current-x (:x current-point)
        current-y (:y current-point)
        moved-left {:x (dec current-x) :y current-y}
        moves-to-make (:moves-to-make move-data)
        moves-left (dec (:moves-left move-data))]
    (if (= moves-left 0)
      {:current-point moved-left :moves-to-make moves-to-make :moves-left moves-to-make :direction "down"}
      {:current-point moved-left :moves-to-make moves-to-make :moves-left moves-left :direction "left"})))

(defn move-up
  [move-data]
  (let [current-point (:current-point move-data)
        current-x (:x current-point)
        current-y (:y current-point)
        moved-up {:x current-x :y (inc current-y)}
        moves-to-make (:moves-to-make move-data)
        moves-left (dec (:moves-left move-data))]
    (if (= moves-left 0)
      {:current-point moved-up :moves-to-make (inc moves-to-make) 
       :moves-left (inc moves-to-make) :direction "left"}
      {:current-point moved-up :moves-to-make moves-to-make :moves-left moves-left :direction "up"})))

(defn move-right
  [move-data]
  (let [current-point (:current-point move-data)
        current-x (:x current-point)
        current-y (:y current-point)
        moved-right {:x (inc current-x) :y current-y}
        moves-to-make (:moves-to-make move-data)
        moves-left (dec (:moves-left move-data))]
    (if  (= moves-left 0)
      {:current-point moved-right :moves-to-make moves-to-make :moves-left moves-to-make :direction "up"}
      {:current-point moved-right :moves-to-make moves-to-make :moves-left moves-left :direction "right"})))


(defn get-spiral-number-coordinate-loop
  [number]
  (loop [x number]
    (when (> x 0)
      (let [direction (:direction @move-data)]
        (if (= direction "right")
          (swap! move-data merge (move-right @move-data)))
        (if (= direction "up")
          (swap! move-data merge (move-up @move-data)))
        (if (= direction "left")
          (swap! move-data merge (move-left @move-data)))
        (if (= direction "down")
          (swap! move-data merge (move-down @move-data))))
      (recur (dec x)))))

(defn get-existing-neighbors
  [current-point]
  (let [x-coordinate (:x current-point)
        y-coordinate (:y current-point)
        point-west {:x (dec x-coordinate) :y y-coodinate}
        point-north-west {:x (dec x-coordinate) :y (inc y-coordinate)}
        point-north {:x x-coordinate :y (inc y-coordinate)}
        point-north-east {:x (inc x-coordinate) :y (inc y-coordinate)}
        existing-neighbors []])
  (if-let [point (get @spiral-sums (hash point-west))]
    (conj existing-neighbors (:value point)))
  (if-let [point (get @spiral-sums (hash point-north-west))]
    (conj existing-neighbors (:value point)))
  (if-let [point (get @spiral-sums (hash point-north))]
    (conj existing-neighbors (:value point)))
  (if-let [point (get @spiral-sums (hash point-north-east))]
    (conj existing-neighbors (:value point)))
  existing-neighbors)


(defn get-next-spiral-sum
  [move-data]
  (let [current-point (:current-point move-data)
        x-coordinate (:x current-point)
        y-coordiante (:y current-point)
        existing-neighbors (get-existing-neighbors current-point)
        sum (reduce + existing-neighbors)]
    (swap! spiral-sums assoc (hash current-point) {:x x-coordinate :y y-coordinate :value sum})
    sum))

(defn get-spiral-sum-loop
  [target]
  (loop [sum 0]
    (while (< sum target)
      (let [direction (:direction @move-data)]
        (if (= direction "right")
          (swap! move-data merge (move-right @move-data)))
        (if (= direction "up")
          (swap! move-data merge (move-up @move-data)))
        (if (= direction "left")
          (swap! move-data merge (move-left @move-data)))
        (if (= direction "down")
          (swap! move-data merge (move-down @move-data))))
      (recur (get-next-spiral-sum @move-data))))


(defn get-spiral-sum
  [target]
  (swap! move-data merge 
         {current-point {:x 0 :y 0} :moves-to-make 1 :moves-left 1 :direction "right"})
  (swap! spiral-sums assoc (hash :x 0 :y 0) {:x 0 :y 0 :value 1})
  (get-spiral-sum-loop target))


(defn get-spiral-number-coordinate
  [number]
  (swap! move-data merge 
         {:current-point {:x 0 :y 0} :moves-to-make 1 :moves-left 1 :direction "right"})
  (get-spiral-number-coordinate-loop (dec number))
  (:current-point @move-data))

 (defn day-3-part-1
  [input]
  (let [input-str (str input)
        input-int (Integer/parseInt input-str)
        distance (calc-distance-from-origin (get-spiral-number-coordinate input-int))]
    {:status 200 :body (str distance) }))


(defn day-3-part-2
  [input]
  (let [input-str (str input)
        input-int (Integer/parseInt input-str)
        distance (calc-distance-from-origin (get-spiral-number-coordinate input-int))]
    {:status 200 :body (str distance) }))
