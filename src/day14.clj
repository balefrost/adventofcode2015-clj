(ns day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day14_input.txt")))

(def line-regex #"^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.$")

(defn parse-input-line [line]
  (let [[_ reindeer speed duration rest] (re-matches line-regex line)]
    {:reindeer reindeer
     :speed    (advent-util/parse-int speed)
     :duration (advent-util/parse-int duration)
     :rest     (advent-util/parse-int rest)}))

(def input-values (map parse-input-line (str/split-lines input-string)))

(defn reindeer-sequence [spec]
  (let [{ :keys [speed duration rest]} spec
        single-iteration (concat (repeat duration speed)
                                 (repeat rest 0))]
    (cycle single-iteration)))

(defn propagate-reindeer [spec]
  (let [sequence (reindeer-sequence spec)]
    (reductions + 0 sequence)))

(defn score-round [round]
  (let [m (apply max round)]
    (map #(if (= m %) 1 0) round)))

(def round-limit 2503)

(defn part1 []
  (apply max (map #(nth (propagate-reindeer %) round-limit) input-values)))

(defn part2 []
  (let [ephemerides (map propagate-reindeer input-values)
        rounds (apply map vector ephemerides)
        scores (map score-round (drop 1 rounds))
        initial-scores (repeat (count ephemerides) 0)
        running-scores (reductions #(map + %1 %2) initial-scores scores)]
    (apply max (nth running-scores round-limit))))
