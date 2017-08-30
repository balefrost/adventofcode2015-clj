(ns day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day19_input.txt")))

(def replacement-regex #"^(\w+) => (\w+)$")

(defn parse-replacement [line]
  (if-let [[_ from to] (re-matches replacement-regex line)]
    {:from from
     :to   to}))

(defn indexes-of
  ([str search] (indexes-of str search 0))
  ([str search start] (lazy-seq
                        (let [idx (.indexOf str search start)]
                          (if (= -1 idx)
                            []
                            (cons idx (indexes-of str search (inc idx))))))))


(defn parse-input [input-string]
  (let [lines (str/split-lines input-string)
        replacement-lines (drop-last 1 lines)
        replacements (->> replacement-lines
                          (map parse-replacement)
                          (remove nil?))
        formula (last lines)]
    {:replacements replacements
     :formula      formula}))

(def input (parse-input input-string))

(defn splice-string [string idx n replacement]
  (str (.substring string 0 idx)
       replacement
       (.substring string (+ idx n))))

(defn process-replacements [replacements formula]
  (for [{:keys [from to]} replacements
        index (indexes-of formula from)]
    (splice-string formula index (.length from) to)))

(defn part1 []
  (let [{:keys [replacements formula]} input]
    (count (set (process-replacements replacements formula)))))
