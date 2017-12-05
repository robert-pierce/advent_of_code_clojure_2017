(ns advent-of-code-2017.day1
  (:require [clojure.string :as str]))



(defn day1-adder
 [jumps input-seq]
 (reduce + (map (fn [a b] (if (= a b) a 0)) input-seq (drop jumps (cycle input-seq)))))

(defn day-1-part-1
  "takes in a sequence of numbers and returns sum of
  the digits that match the next digit in the list"
  [input]
  (let [input-str (str input)
        input-seq (map read-string (str/split input-str #""))
        answer (day1-adder 1 input-seq)]
    {:status 200 :body (str "The answer is " answer)}))


(defn day-1-part-2
  "takes in a sequence of numbers and returns sum of
  the digits that match the next digit in the list"
  [input]
  (let [input-str (str input)
        input-seq (map read-string (str/split input-str #""))
        answer (day1-adder (/ (count input-seq) 2) input-seq)]
    {:status 200 :body (str "The answer is " answer)}))
