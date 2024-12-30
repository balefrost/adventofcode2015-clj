(ns day19
  (:require
    [clojure.data.priority-map :refer [priority-map-keyfn]]
    [clojure.java.io :as io]
    [clojure.string :as str])
  (:use [advent-util]))

(def input-string (slurp (io/resource "day19_input.txt")))

(def replacement-regex #"^(\w+) => (\w+)$")

(defn parse-replacement [line]
  (if-let [[_ from to] (re-matches replacement-regex line)]
    {:from from
     :to   to}))



(defn indexes-of
  ([^String s search]
   (indexes-of s search 0))
  ([s search idx]
   (lazy-seq
     (let [found (.indexOf s search idx)]
       (if (< found 0)
         nil
         (cons found (indexes-of s search (inc found))))))))

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

(defn process-replacements [replacements formula]
  (for [{:keys [from to]} replacements
        index (indexes-of formula from)]
    (str
      (subs formula 0 index)
      to
      (subs formula (+ index (count from))))))

(defn part1 []
  (let [{:keys [replacements formula]} input]
    (count (set (process-replacements replacements formula)))))

(defn directed-search-seq [init get-neighbors get-score]
  (letfn [(helper [queue results]
            (lazy-seq
              (if (not (empty? queue))
                (let [[item {item-cost :cost :as item-data}] (peek queue)
                      score-queue (pop queue)
                      neighbors (get-neighbors item)
                      updated-entries (for [{neighbor :neighbor neighbor-inc-cost :cost} neighbors
                                            :let [neighbor-score (get-score neighbor)
                                                  neighbor-cost (+ item-cost neighbor-inc-cost)
                                                  existing-entry (get score-queue neighbor)
                                                  {existing-cost :cost} existing-entry]]
                                        (if (or (nil? existing-entry) (< neighbor-cost existing-cost))
                                          [neighbor {:score neighbor-score :cost neighbor-cost :prev item}]
                                          [neighbor existing-entry]))
                      score-queue (reduce (partial apply assoc) score-queue updated-entries)
                      results (update results item
                                      (fn [{existing-cost :cost :as existing-data}]
                                        (if (or (nil? existing-data) (< item-cost existing-cost))
                                          item-data
                                          existing-data)))]
                  (cons
                    {:queue score-queue :results results}
                    (helper score-queue results))))))]

    (let [init-data {:score (get-score init) :cost 0}]
      (helper
        (priority-map-keyfn :score init init-data)
        {}))))

; This makes me kind of sad.
; The problem description indicates that we should find the shortest set of steps.
; However, according to /u/topaz2078, there's only one possible reduction.
; One suggestion was to write a CYK parser, which will perform the minimum number of
; reductions. However, this requires the grammer to be rewritten in CNF, and that
; might make it harder to determine the actual number of reductions.
;
; So I'll do a backtracking greedy algorithm instead.
(defn part2 []
  (let [{:keys [replacements formula]} input
        rev-replacements (for [{:keys [from to]} replacements]
                           {:from to :to from})
        states (directed-search-seq
                 formula
                 (fn [s]
                   (for [r (process-replacements rev-replacements s)]
                     {:neighbor r :cost 1}))
                 (fn [s] (.length s)))]
    (:cost
      (get
        (first
          (drop-while
            #(not (contains? % "e"))
            (map :results states)))
        "e"))))
