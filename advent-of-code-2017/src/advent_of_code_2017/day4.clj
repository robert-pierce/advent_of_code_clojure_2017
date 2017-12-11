(ns advent-of-code-2017.day4
  (:require [clojure.string :as str]))

(defn validate-row
  [row]
  (let [frequencies (frequencies row)
        duplicates (remove (comp #{1} val) frequencies)]
    (if (empty? duplicates)
      true
      false)))

(defn validate-row-v2
  [row]
  (let [split-words (map #(str/split % #"") row)
        letter-frequencies (map (comp sort frequencies) split-words)
        anagrams (frequencies letter-frequencies)
        duplicate-anagrams (remove (comp #{1} val) anagrams)]
    (println anagrams duplicate-anagrams)
    (if (empty? duplicate-anagrams)
      true
      false)))


(defn day-4-part-1
  [body]
  (let [strBody (slurp body)
        rows (str/split strBody #"\n")
        split-rows (map #(str/split % #" ") rows)
        validated-rows (map validate-row split-rows)
        valid-rows (filter true? validated-rows)]
    {:status 200 :body (str "The number of valid passphrases is " (count valid-rows))}))

(defn day-4-part-2
  [body]
  (let [strBody (slurp body)
        rows (str/split strBody #"\n")
        split-rows (map #(str/split % #" ") rows)
        validated-rows (map validate-row-v2 split-rows)
        valid-rows (filter true? validated-rows)]
    {:status 200 :body (str "The number of valid passphrases is " (count valid-rows))}))
