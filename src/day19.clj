(ns day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day19_input.txt")))

(def replacement-regex #"^(\w+) => (\w+)$")

(def molecule-regex #"[A-Z][a-z]*")

(defn partition-formula [formula]
  (vec (re-seq molecule-regex formula)))

(defn parse-replacement [line]
  (if-let [[_ from to] (re-matches replacement-regex line)]
    {:from from
     :to   (partition-formula to)}))

(defn indexes-of
  ([seq search]
   (indexes-of seq search 0))
  ([seq search idx]
   (lazy-seq
     (loop [seq seq
            idx idx]
       (if (empty? seq)
         []
         (if (= (first seq) search)
           (cons idx (indexes-of (rest seq) search (inc idx)))
           (recur (rest seq) (inc idx))))))))

(defn parse-input [input-string]
  (let [lines (str/split-lines input-string)
        replacement-lines (drop-last 1 lines)
        replacements (->> replacement-lines
                          (map parse-replacement)
                          (remove nil?))
        formula (last lines)]
    {:replacements replacements
     :formula      (partition-formula formula)}))

(def input (parse-input input-string))

(defn splice [seq idx n & new]
  (let [[a b] (split-at idx seq)
        c (drop n b)]
    (concat a new c)))

(defn process-replacements [replacements formula]
  (for [{:keys [from to]} replacements
        index (indexes-of formula from)]
    (apply splice formula index 1 to)))

(defn part1 []
  (let [{:keys [replacements formula]} input]
    (count (set (process-replacements replacements formula)))))
