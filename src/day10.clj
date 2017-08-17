(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input (map #(Integer/parseInt (str %)) "1321131112"))

(defn step [s]
  (let [[hd] s
        [front back] (split-with #(= hd %) s)]
    [[(count front) (first front)] back]))

(defn transform [s]
  (lazy-seq
    (if (empty? s)
      []
      (let [[front back] (step s)]
        (concat front (transform back))))))

(defn part1 []
  (count (nth (iterate transform input) 40)))

(defn part2 []
  (count (nth (iterate transform input) 50)))