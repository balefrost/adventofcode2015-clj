(ns day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input "bgvyzdsv")

(defn find-prefix [prefix]
  (->> (iterate inc 1)
       (pmap (fn [x] [x (str input x)]))
       (pmap (fn [[i s]] [i (advent/compute-md5-string s)]))
       (filter (fn [[i s]] (str/starts-with? s prefix)))
       (first)
       (first)))

(defn part1 []
  (find-prefix "00000"))

(defn part2 []
  (find-prefix "000000"))
