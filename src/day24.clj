(ns day24
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day24_input.txt")))

(def input (map advent-util/parse-int (string/split-lines input-string)))

(defn subsets [coll]
  (apply concat
         (for [n (range (inc (count coll)))]
           (advent-util/combinations coll n))))

(defn solve [weights num-groups]
  (let [total-weight (reduce + 0 weights)
        target-weight (quot total-weight num-groups)]
    (assert (zero? (mod total-weight target-weight)))
    (let [choices (subsets (sort > weights))
          interesting-choices (filter #(= (reduce + 0 %) target-weight) choices)
          shortest-choices (first (partition-by count interesting-choices))
          entanglements (map #(apply * %) shortest-choices)]
      (apply min entanglements))))

(defn part1 []
  (solve input 3))

(defn part2 []
  (solve input 4))

