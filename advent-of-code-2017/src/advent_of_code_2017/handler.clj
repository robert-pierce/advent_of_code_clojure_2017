(ns advent-of-code-2017.handler
  (:require [advent-of-code-2017.day1 :as day1]
            [advent-of-code-2017.day2 :as day2]
            [advent-of-code-2017.day3 :as day3]
            [clojure.string :as str]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]))



(defroutes app-routes
  (GET "/pulse_check" [] "I'm alive!")
  (GET "/day1/part1/:input" [input] (day1/day-1-part-1 input))
  (GET "/day1/part2/:input"  [input] (day1/day-1-part-2 input))
  (POST "/day2/part-1" request (day2/day-2-part-1 (:body request)))
  (POST "/day2/part-2" request (day2/day-2-part-2 (:body request)))
  (GET "/day3/part-1/:input" [input] (day3/day-3-part-1 input))
  (GET "/day3/part-2/:input" [input] (day3/day-3-part-2 input) )
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes api-defaults))
