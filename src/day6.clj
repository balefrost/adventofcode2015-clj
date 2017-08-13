(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day6_input.txt")))
(def input-lines (str/split-lines input-string))

(def input-line-regex #"^(.*?) (\d+),(\d+) through (\d+),(\d+)$")

(defn make-command [line]
  (if-let [[_ action x1 y1 x2 y2] (re-matches input-line-regex line)]
    (let [action (condp = action
                   "turn on" :on
                   "turn off" :off
                   "toggle" :toggle)]
      {:action action
       :x1     (Integer/parseInt x1)
       :y1     (Integer/parseInt y1)
       :x2     (Integer/parseInt x2)
       :y2     (Integer/parseInt y2)})))

(def input-commands (map make-command input-lines))

(defn affected-indices [x1 y1 x2 y2]
  (for [y (range y1 (inc y2))
        x (range x1 (inc x2))]
    (+ x (* 1000 y))))

(defn turnon [board index]
  (assoc board index true))

(defn turnoff [board index]
  (assoc board index false))

(defn toggle [board index]
  (assoc board index (not (board index))))

(defn inc1 [board index]
  (assoc board index (inc (board index))))

(defn dec1 [board index]
  (let [result (dec (board index))
        result (max 0 result)]
    (assoc board index result)))

(defn inc2 [board index]
  (assoc board index (+ 2 (board index))))

(def part1-implementations {:on     turnon
                            :off    turnoff
                            :toggle toggle})

(def part2-implementations {:on     inc1
                            :off    dec1
                            :toggle inc2})

(defn apply-command [implementations board command]
  (let [{:keys [action x1 y1 x2 y2]} command
        indices (affected-indices x1 y1 x2 y2)
        fn (implementations action)]
    (reduce fn board indices)))

(def part1-initial-board (vec (repeat (* 1000 1000) false)))
(def part2-initial-board (vec (repeat (* 1000 1000) 0)))

(defn part1 []
  (count
    (filter identity
            (reduce #(apply-command part1-implementations %1 %2)
                    part1-initial-board
                    input-commands))))

(defn part2 []
  (apply +
         (reduce #(apply-command part2-implementations %1 %2)
                 part2-initial-board
                 input-commands)))