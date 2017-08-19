(ns day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input "cqjxjnds")

(defn lower-char-to-int [c]
  (- (int c) (int \a)))

(defn int-to-lower-char [i]
  (char (+ (int \a) i)))

(defn string-to-vector [s]
  (map lower-char-to-int s))

(defn vector-to-string [v]
  (apply str (map int-to-lower-char v)))

(defn ripple [v]
  (loop [acc []
         r (reverse v)]
    (if (empty? r)
      (reverse acc)
      (let [[hd & tl] r]
        (if (< hd 25)
          (reverse (concat acc [(inc hd)] tl))
          (recur (conj acc 0) tl))))))

(defn split-straight [v]
  (if (empty? v)
    [[] []]
    (loop [prev (first v)
           acc [prev]
           [next & rem :as v] (rest v)]
      (cond
        (empty? v) [acc v]
        (= next (inc prev)) (recur next (conj acc next) rem)
        :else [acc v]))))


(defn iterate-splits [split-fn s]
  (lazy-seq
    (if (empty? s)
      nil
      (let [[front back] (split-fn s)]
        (if (empty? front)
          nil
          (cons front (iterate-splits split-fn back)))))))

(defn straights [v]
  (iterate-splits split-straight v))

(defn has-increasing-three-straight? [v]
  (let [sts (straights v)]
    (some #(>= (count %) 3) sts)))

(defn all-good-chars? [v]
  (not-any? (set (map lower-char-to-int [\i \o \l])) v))

(defn split-run-of-two [v]
  (if (empty? v)
    [[] []]
    (let [[x & rest] v]
      (if (empty? rest)
        [[x] rest]
        (let [[y & rrest] rest]
          (if (= x y)
            [[x y] rrest]
            [[x] rest]))))))

(defn has-two-runs? [v]
  (let [runs (iterate-splits split-run-of-two v)
        good-runs (filter #(>= (count %) 2) runs)]
    (>= (count good-runs) 2)))

(defn is-valid-password? [v]
  (and
    (has-increasing-three-straight? v)
    (all-good-chars? v)
    (has-two-runs? v)))

(defn find-next-password [v]
  (let [all-passwords (rest (iterate ripple v))
        good-passwords (filter is-valid-password? all-passwords)]
    (first good-passwords)))

(defn part1 []
  (-> input
      (string-to-vector)
      (find-next-password)
      (vector-to-string)))

(defn part2 []
  (-> input
      (string-to-vector)
      (find-next-password)
      (find-next-password)
      (vector-to-string)))
