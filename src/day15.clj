(ns day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day15_input.txt")))

(def line-regex #"^(\w+): capacity ([-\d]+), durability ([-\d]+), flavor ([-\d]+), texture ([-\d]+), calories ([-\d]+)$")

(defn parse-input-line [line]
  (let [[_ name capacity durability flavor texture calories] (re-matches line-regex line)]
    {:name       name
     :attributes {:capacity   (advent-util/parse-int capacity)
                  :durability (advent-util/parse-int durability)
                  :flavor     (advent-util/parse-int flavor)
                  :texture    (advent-util/parse-int texture)
                  :calories   (advent-util/parse-int calories)}}))

(def ingredients (map parse-input-line (str/split-lines input-string)))

(defn map-values [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn mix-cookie-ingredient [cookie ingredient]
  (let [[amount {attributes :attributes}] ingredient
        scaled-attributes (map-values #(* amount %) attributes)]
    (merge-with + cookie scaled-attributes)))

(defn bake-cookie [recipe]
  (reduce mix-cookie-ingredient {} recipe))

(defn score-cookie [cookie]
  (->> cookie
       (filter #(not (#{:calories} (first %))))
       (vals)
       (map #(if (neg? %) 0 %))
       (apply *)))

(defn part1-recipe-score [recipe]
  (let [cookie (bake-cookie recipe)
        score (score-cookie cookie)]
    score))

(defn part2-recipe-score [recipe]
  (let [cookie (bake-cookie recipe)
        score (score-cookie cookie)]
    (if (= (:calories cookie) 500)
      score
      0)))

(defn solve [ingredients score-fn]
  (let [scores (pmap #(score-fn (map vector % ingredients))
                     (advent-util/partitions (count ingredients) 100))]
    (apply max scores)))

(defn part1 []
  (solve ingredients part1-recipe-score))

(defn part2 []
  (solve ingredients part2-recipe-score))

