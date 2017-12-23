(ns advent-of-code-2017.day7
  (:require [clojure.string :as str]))


(defn day-7-part-1
 [body]
 (let [strBody (slurp body)
       ]
   {:status 200 :body (str "The strBody is " strBody)}))
