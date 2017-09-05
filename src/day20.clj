(ns day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input 36000000)

(defn factorize [n]
  (let [lim (Math/sqrt n)
        factors (for [d (range 1 lim)
                      :when (zero? (mod n d))
                      r [d (quot n d)]]
                  r)
        ext (if (zero? (mod n lim)) [(int lim)] [])]
    (sort (concat ext factors))))

(defn compute-house-presents [house-number]
  {:house-number house-number
   :present-count (* 10 (apply + (factorize house-number)))})

(defn part1 []
  (let [present-counts (pmap compute-house-presents (rest (range)))
        houses-with-enough-presents (filter #(>= (:present-count %) input) present-counts)]
    (:house-number (first houses-with-enough-presents))))
