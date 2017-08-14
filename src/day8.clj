(ns day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day8_input.txt")))
(def input-lines (str/split-lines input-string))

(defn -count-in-memory-chars [sum s]
  (if (empty? s)
    sum
    (let [[a b c d] s
          to-advance (if (not (= a \\))
                       1
                       (condp = b
                         \\ 2
                         \" 2
                         \x 4))]
      (recur (inc sum) (drop to-advance s)))))

(defn count-in-memory-chars [s]
  (-count-in-memory-chars -2 s))

(defn compute-char-size [c]
  (cond
    (= \\ c) 2
    (= \" c) 2
    :else 1))

(defn count-on-disk-chars [s]
  (reduce #(+ %1 (compute-char-size %2)) 2 s))

(defn part1 []
  (let [size-on-disk (apply + (map count input-lines))
        size-in-memory (apply + (map count-in-memory-chars input-lines))]
    (- size-on-disk size-in-memory)))

(defn part2 []
  (let [size-in-memory (apply + (map count input-lines))
        size-on-disk (apply + (map count-on-disk-chars input-lines))]
    (- size-on-disk size-in-memory)))