(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day5_input.txt")))
(def input-lines (str/split-lines input-string))

(defn is-vowel? [c]
  (#{\a \e \i \o \u} c))

(defn has-three-vowels? [s]
  (>= (count (filter is-vowel? s)) 3))

(defn has-repeat-pair? [pairs]
  (some #(apply = %) pairs))

(defn has-invalid-pair? [pairs]
  (some identity (map #{"ab" "cd" "pq" "xy"} pairs)))

(defn string-pairs [s]
  (for [i (range 0 (dec (count s)))]
    (.substring s i (+ i 2))))

(defn has-nonoverlapping-pair-starting-at [s i]
  (let [end (+ 2 i)
        target (.substring s i end)]
    (>= (.indexOf s target end) 0)))

(defn has-nonoverlapping-pair [s]
  (some identity
    (for [x (range 0 (- (count s) 3))]
      (has-nonoverlapping-pair-starting-at s x))))

(defn has-duplicate-separated-by-one-character [s]
  (let [pairs (map vector s (drop 2 s))]
    (some #(apply = %) pairs)))

(defn part1-is-nice? [s]
  (and
    (has-three-vowels? s)
    (let [pairs (string-pairs s)]
      (and
        (has-repeat-pair? pairs)
        (not (has-invalid-pair? pairs))))))

(defn part2-is-nice? [s]
  (and
    (has-nonoverlapping-pair s)
    (has-duplicate-separated-by-one-character s)))

(defn part1 []
  (count (filter part1-is-nice? input-lines)))

(defn part2 []
  (count (filter part2-is-nice? input-lines)))