(ns advent-of-code-2017.handler
  (:require [advent-of-code-2017.day1 :as day1]
            [clojure.string :as str]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]))



(defroutes app-routes
  (GET "/pulse_check" [] "I'm alive!")
  (GET "/day1/part1/:input" [input] (day1/day-1-part-1 input))
  (GET "/day1/part2/:input"  [input] (day1/day-1-part-2 input))
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes site-defaults))
