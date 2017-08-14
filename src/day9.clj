(ns day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day9_input.txt")))
(def input-lines (str/split-lines input-string))

(def input-line-regex #"^(\w+) to (\w+) = (\d+)$")
(defn parse-input-line [line]
  (let [[_ from to dist] (re-matches input-line-regex line)]
    {:from from
     :to   to
     :dist (Integer/parseInt dist)}))

(def parsed-input-lines (map parse-input-line input-lines))

(defn generate-entries [d]
  (let [{:keys [from to dist]} d]
    [[{:from from :to to} dist]
     [{:from to :to from} dist]]))

(def input-map
  (into {} (mapcat generate-entries parsed-input-lines)))

(def all-cities (into #{} (map :from (keys input-map))))

(defn permutations [s]
  (if (empty? s)
    [[]]
    (mapcat
      (fn [el]
        (let [without-el (remove #(= el %) s)]
          (map #(cons el %) (permutations without-el))))
      s)))

(defn all-paths [all-cities]
  (permutations all-cities))

(defn compute-path-length [path input-map]
  (let [path-steps (partition 2 1 path)
        keys (map (fn [[f t]] {:from f :to t}) path-steps)
        lengths (map input-map keys)]
    (apply + lengths)))

(defn part1 []
  (let [paths-with-length (for [path (all-paths all-cities)]
                            [(compute-path-length path input-map) path])
        paths-in-order (sort-by first paths-with-length)]
    (first (first paths-in-order))))

(defn part2 []
  (let [paths-with-length (for [path (all-paths all-cities)]
                            [(compute-path-length path input-map) path])
        paths-in-order (sort-by first paths-with-length)]
    (first (last paths-in-order))))