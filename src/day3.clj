(ns day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-string (slurp (io/resource "day3_input.txt")))

(defn next-state [[x y] dir]
  (condp = dir
    \> [(inc x) y]
    \< [(dec x) y]
    \^ [x (inc y)]
    \v [x (dec y)]))


(defn part1 []
  (let [history (reductions next-state [0 0] input-string)]
    (count (set history))))

(defn part2 []
  (let [santa-history (reductions next-state [0 0] (take-nth 2 input-string))
        robo-history (reductions next-state [0 0] (take-nth 2 (drop 1 input-string)))
        whole-history (concat santa-history robo-history)]
    (count (set whole-history))))