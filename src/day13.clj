(ns day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day13_input.txt")))

(def line-regex #"^(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+).")

(defn parse-input-line [line]
  (let [[_ person gain-lose amount neighbor] (re-matches line-regex line)
        key {:person person :neighbor neighbor}
        numeric-amount (advent/parse-int amount)]
    (case gain-lose
      "gain" [key numeric-amount]
      "lose" [key (- numeric-amount)])))

(defn find-people [neighbor-values]
  (into #{} (map (comp :person first) neighbor-values)))

(def part1-neighbor-values
  (into {} (map parse-input-line (str/split-lines input-string))))

(def part2-neighbor-values
  (let [new-entries (mapcat
                      (fn [x]
                        [[{:person "me" :neighbor x} 0]
                         [{:person x :neighbor "me"} 0]])
                      (find-people part1-neighbor-values))]
    (into part1-neighbor-values new-entries)))

(defn all-neighbors [arrangement]
  (let [boundary-pair [(last arrangement) (first arrangement)]
        regular-pairs (partition 2 1 arrangement)
        all-pairs (cons boundary-pair regular-pairs)
        mapping-fn (fn [[a b]]
                     [{:person a :neighbor b}
                      {:person b :neighbor a}])]
    (mapcat mapping-fn all-pairs)))

(defn compute-arrangement-value [neighbor-values arrangement]
  (let [n (all-neighbors arrangement)
        costs (map neighbor-values n)]
    (apply + costs)))

(defn solve [neighbor-values]
  (let [arrangements (advent-util/permutations (find-people neighbor-values))
        values (pmap #(compute-arrangement-value neighbor-values %) arrangements)]
    (apply max values)))

(defn part1 []
  (solve part1-neighbor-values))

(defn part2 []
  (solve part2-neighbor-values))
