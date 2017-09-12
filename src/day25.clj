(ns day25
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day25_input.txt")))

(def input (let [[[_ row column]] (re-seq #"Enter the code at row (\d+), column (\d+)." input-string)]
             {:row    (dec (Integer/parseInt row))
              :column (dec (Integer/parseInt column))}))

(defn compute-binomial [n]
  (/ (* n (inc n)) 2))

(defn compute-cell-number [row column]
  (+ column (compute-binomial (+ row column))))

(defn step-value [prev]
  (rem (* prev 252533) 33554393))

(def initial-value 20151125)

(defn part1 []
  (let [cell-number (compute-cell-number (:row input) (:column input))]
    (nth (iterate step-value initial-value) cell-number)))