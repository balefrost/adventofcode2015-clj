(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-string (slurp (io/resource "day2_input.txt")))
(def input-lines (str/split-lines input-string))

(defn parse-int [n]
  (Integer/parseInt n))

(defn parse-line [l]
  (sort (map parse-int (str/split l #"x"))))

(def input-boxes (map parse-line input-lines))

(defn compute-required-wrapping-paper [sides]
  (let [[a b c] sides]
    (+
      (* 3 a b)
      (* 2 a c)
      (* 2 b c))))

(defn compute-required-ribbon [sides]
  (let [[a b c] sides]
    (+
      (* 2 a)
      (* 2 b)
      (* a b c))))

(defn part1 []
  (let [paper (map compute-required-wrapping-paper input-boxes)]
    (apply + paper)))

(defn part2 []
  (let [paper (map compute-required-ribbon input-boxes)]
    (apply + paper)))
