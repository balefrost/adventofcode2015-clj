(ns day17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day17_input.txt")))

(def containers
  (->> input-string
       (str/split-lines)
       (map advent-util/parse-int)))

(defn container-combinations [containers]
  (lazy-seq
    (if (empty? containers)
      [[]]
      (let [[size & rest] containers
            tail-combs (container-combinations rest)]
        (for [hd [0 1]
              tail tail-combs]
          (if (= 0 hd)
            tail
            (cons size tail)))))))

(defn used-container-storage-capacity [used-containers]
  (apply + used-containers))

(defn containers-holding-exactly [size containers]
  (->> containers
       (container-combinations)
       (filter #(= size (used-container-storage-capacity %)))))

(defn part1 []
  (->> containers
       (containers-holding-exactly 150)
       (count)))

(defn part2 []
  (->> containers
       (containers-holding-exactly 150)
       (group-by count)
       (sort-by first)
       (first)
       (second)
       (count)))