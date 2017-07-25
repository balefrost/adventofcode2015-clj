(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-string (slurp (io/resource "day1_input.txt")))

(defn paren-to-int [ch]
  (condp = ch
    \( 1
    \) -1))

(defn part1 []
  (->> input-string
    (map paren-to-int)
    (reduce + 0)))

(defn part2 []
  (let [reductions (->> input-string
                        (map paren-to-int)
                        (reductions + 0))
        prefix (take-while (complement neg?) reductions)]
    (count prefix)))



