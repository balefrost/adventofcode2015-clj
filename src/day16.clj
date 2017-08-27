(ns day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day16_input.txt")))

(def line-regex1 #"^Sue (\d+): (.*)$")
(def line-regex2 #"(\w+): (\d+)")

(defn parse-input-line [line]
  (let [[_ n tl] (re-matches line-regex1 line)
        items (re-seq line-regex2 tl)
        parsed-items (map
                       #(let [[_ att val] %]
                          [att (advent-util/parse-int val)])
                       items)]
    {:name       (advent-util/parse-int n)
     :attributes (into {} parsed-items)}))

(def sues (map parse-input-line (str/split-lines input-string)))

(def part1-desired-atts
  {"children"    [= 3]
   "cats"        [= 7]
   "samoyeds"    [= 2]
   "pomeranians" [= 3]
   "akitas"      [= 0]
   "vizslas"     [= 0]
   "goldfish"    [= 5]
   "trees"       [= 3]
   "cars"        [= 2]
   "perfumes"    [= 1]})

(def part2-desired-atts
  {"children"    [= 3]
   "cats"        [> 7]
   "samoyeds"    [= 2]
   "pomeranians" [< 3]
   "akitas"      [= 0]
   "vizslas"     [= 0]
   "goldfish"    [< 5]
   "trees"       [> 3]
   "cars"        [= 2]
   "perfumes"    [= 1]})



(defn sue-matches? [desired-atts sue]
  (every?
    #(let [[att val] %
           [fn target] (desired-atts att)]
       (fn val target))
    (:attributes sue)))

(defn part1 []
  (let [matching-sues (filter #(sue-matches? part1-desired-atts %) sues)]
    (:name (first matching-sues))))

(defn part2 []
  (let [matching-sues (filter #(sue-matches? part2-desired-atts %) sues)]
    (:name (first matching-sues))))
