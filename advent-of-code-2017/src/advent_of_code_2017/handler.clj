(ns advent-of-code-2017.handler
  (:require [clojure.string :as str]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]))


(defn day1
  "takes in a sequence of numbers and returns sum of
  the digits that match the next digit in the list"
  [input]
  (let [input-str (str input)
        input-seq (map read-string (str/split input-str #""))
        answer (reduce + 
                (map (fn [a b] (if (= a b) a 0))
                     input-seq
                     (drop 1 (cycle input-seq))))]
    {:status 200 :body (str "The answer is " answer)}))


(defroutes app-routes
  (GET "/pulse_check" [] "I'm alive!")
  (GET "/day1/:input" [input] (day1 input))
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes site-defaults))
