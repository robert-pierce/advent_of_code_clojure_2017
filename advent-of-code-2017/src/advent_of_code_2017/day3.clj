(ns advent-of-code-2017.day3
  (:require [clojure.string :as str]))

(declare move-right)
(declare move-left)
(declare move-up)
(declare move-down)


(defn move-down
  [current-point numbers-left move-data]
  (let [current-x (:x current-point)
        current-y (:y current-point)
        moved-down {:x current-x :y (dec current-y)}
        moves-to-make (:moves-to-make move-data)
        moves-left (dec (:moves-left move-data))]
    (if (= numbers-left 1)
      moved-down
      (if (= moves-left 0)
        (move-right moved-down (dec numbers-left)
                    {:moves-to-make (inc moves-to-make)
                     :moves-left (inc moves-to-make)})
        (move-down moved-down (dec numbers-left)
                   {:moves-to-make moves-to-make
                    :moves-left  moves-left})))))

(defn move-left
  [current-point numbers-left move-data]
  (let [current-x (:x current-point)
        current-y (:y current-point)
        moved-left {:x (dec current-x) :y current-y}
        moves-to-make (:moves-to-make move-data)
        moves-left (dec (:moves-left move-data))]
    (if (= numbers-left 1)
      moved-left
      (if (= moves-left 0)
        (move-down moved-left (dec numbers-left)
                   {:moves-to-make moves-to-make
                    :moves-left moves-to-make})
        (move-left moved-left (dec numbers-left) 
                   {:moves-to-make moves-to-make
                    :moves-left  moves-left})))))

(defn move-up
  [current-point numbers-left move-data]
  (let [current-x (:x current-point)
        current-y (:y current-point)
        moved-up {:x current-x :y (inc current-y)}
        moves-to-make (:moves-to-make move-data)
        moves-left (dec (:moves-left move-data))]
    (if (= numbers-left 1)
      moved-up
      (if (= moves-left 0)
        (move-left moved-up (dec numbers-left)
                   {:moves-to-make (inc moves-to-make)
                    :moves-left (inc moves-to-make)})
        (move-up moved-up (dec numbers-left) 
                 {:moves-to-make moves-to-make
                  :moves-left  moves-left})))))

(defn move-right
  [current-point numbers-left move-data]
  (let [current-x (:x current-point)
        current-y (:y current-point)
        moved-right {:x (inc current-x) :y current-y}
        moves-to-make (:moves-to-make move-data)
        moves-left (dec (:moves-left move-data))]
    (if (= numbers-left 1)
      moved-right
      (if  (= moves-left 0)
        (move-up moved-right (dec numbers-left) 
                 {:moves-to-make moves-to-make 
                  :moves-left moves-to-make})
        (move-right moved-right (dec numbers-left) 
                    {:moves-to-make moves-to-make 
                     :moves-left  moves-left})))))

(defn move-down-2
  [move-data]
  (let [current-point (:current-point move-data)
        current-x (:x current-point)
        current-y (:y current-point)
        moved-down {:x current-x :y (dec current-y)}
        moves-to-make (:moves-to-make move-data)
        moves-left (dec (:moves-left move-data))]
    (if (= moves-left 0)
      {:current-pont moved-down :moves-to-make (inc moves-to-make)
       :moves-left (inc moves-to-make) :direction "right"}
      {:current-point moved-down :moves-to-make moves-to-make :moves-left moves-left :direction "down"})))


(defn move-left-2
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

(defn move-up-2
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


(defn move-right-2
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


(def move-data (atom {:current-point {:x 0 :y 0} :moves-to-make 1 :moves-left 1 :direction "right"}))

(defn get-spiral-number-coordinate
  ([stopping-number]  
   (let [new-point {:x 0 :y 0}
         move-data {:current-point {:x 0 :y 0} :moves-to-make 1 :moves-left 1 :direction "right"}]
     (if (= stopping-number 1)
       new-point
       (move-right new-point (dec stopping-number) move-data)))))

(defn get-spiral-number-coordinate-loop
  [number]
  (loop [x number]
    (when (> x 0)
      (let [direction (:direction @move-data)]
        (if (= direction "right")
          (swap! move-data merge (move-right-2 @move-data)))
        (if (= direction "up")
          (swap! move-data merge (move-up-2 @move-data)))
        (if (= direction "left")
          (swap! move-data merge (move-left-2 @move-data)))
        (if (= direction "down")
          (swap! move-data merge (move-down-2 @move-data)))))
    (print x)
    (recur (dec x))))


(defn get-spiral-number-coordinate-2
  [number]
  (swap! move-data merge 
         {:current-point {:x 0 :y 0} :moves-to-make 1 :moves-left 1 :direction "right"})
  (get-spiral-number-coordinate-loop number)
  @move-data)



(defn day-3-part-1
  [input]
  {:status 200 :body (str "derk a derk") })
