(ns day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day18_input.txt")))

(def initial-board
  (vec (for [line (str/split-lines input-string)
             ch line]
         (case ch
           \# 1
           \. 0))))

(defn cell-index [row column]
  (+ column (* row 100)))

(defn cell-position [index]
  (let [row (quot index 100)
        column (mod index 100)]
    [row column]))

(defn get-cell [board row column]
  (if (and (>= row 0)
           (< row 100)
           (>= column 0)
           (< column 100))
    (nth board (cell-index row column))
    0))

(defn set-cell [board row column value]
  (assoc board (cell-index row column) value))

(defn neighbors [row column]
  (for [y [-1 0 1]
        x [-1 0 1] :when (not= x y 0)]
    [(+ row y) (+ column x)]))

(defn updated-cell-value [board row column]
  (let [current-value (get-cell board row column)
        neighbor-value-count (apply + (for [[row column] (neighbors row column)
                                            :let [v (get-cell board row column)]]
                                        v))]
    (cond
      (and (= 1 current-value) (#{2 3} neighbor-value-count)) 1
      (and (= 0 current-value) (= 3 neighbor-value-count)) 1
      :else 0)))

(defn step-board [board]
  (mapv
    #(let [[row column] (cell-position %)]
       (updated-cell-value board row column))
    (range (* 100 100))))

(defn turn-corners-on [board]
  (-> board
      (set-cell 0 0 1)
      (set-cell 0 99 1)
      (set-cell 99 0 1)
      (set-cell 99 99 1)))

(defn step-board-stuck [board]
  (-> board
      (step-board)
      (turn-corners-on)))

(defn print-board [board]
  (println
    (str/join
      \newline
      (map
        (fn [line]
          (str/join
            (map
              #(case % 1 \# 0 \.)
              line)))
        (partition 100 board)))))

(defn solve [initial-board step]
  (let [iterations (iterate step initial-board)
        final-board (nth iterations 100)]
    (apply + final-board)))

(defn part1 []
  (solve initial-board step-board))

(defn part2 []
  (solve (turn-corners-on initial-board) step-board-stuck))
