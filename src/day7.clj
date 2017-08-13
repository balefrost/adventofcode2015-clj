(ns day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day7_input.txt")))

(defn make-op [type]
  (fn [[_ & args]]
    (let [ins (drop-last 1 args)
          out (last args)
          typed-ins (mapv #(if (re-matches #"^\d+$" %) (Integer/parseInt %) %) ins)]
      {:type type :in typed-ins :out out})))

(defmacro cond-regex1 [s patterns]
  (if (empty? patterns)
    `(throw (Exception. "No match"))
    (let [[r f & patterns] patterns]
      `(let [result# (re-matches ~r ~s)]
         (if result#
           (~f result#)
           (cond-regex1 ~s ~patterns))))))

(defmacro cond-regex [s & patterns]
  (assert
    (= (mod (count patterns) 2) 0)
    "need even number of patterns")
  `(let [evaluated# ~s]
     (cond-regex1 evaluated# ~patterns)))

(defn parse-wiring-command [line]
  (cond-regex line
              #"(\w+) -> (\w+)" (make-op :constant)
              #"(\w+) AND (\w+) -> (\w+)" (make-op :and)
              #"(\w+) OR (\w+) -> (\w+)" (make-op :or)
              #"(\w+) LSHIFT (\d+) -> (\w+)" (make-op :lshift)
              #"(\w+) RSHIFT (\d+) -> (\w+)" (make-op :rshift)
              #"NOT (\w+) -> (\w+)" (make-op :not)))

(def input-wiring-commands (map parse-wiring-command (str/split-lines input-string)))

(def wiring-map (into {} (map #(do [(:out %) %]) input-wiring-commands)))

(defn resolve-argument [values arg]
  (if (number? arg)
    arg
    (values arg)))

(defn step-evaluation [[values queue :as args]]
  (if (empty? queue)
    args
    (let [[target & rest-queue] queue]
      (cond
        (number? target) [values rest-queue]
        (string? target) (let [resolved (values target)]
                           [values (concat (:in resolved) [resolved] rest-queue)])
        (map? target) (let [{:keys [in type out]} target
                            resolved-inputs (map #(resolve-argument values %) in)
                            result (condp = type
                                     :constant (apply identity resolved-inputs)
                                     :and (apply bit-and resolved-inputs)
                                     :or (apply bit-or resolved-inputs)
                                     :lshift (apply bit-shift-left resolved-inputs)
                                     :rshift (apply bit-shift-right resolved-inputs)
                                     :not (apply bit-not resolved-inputs))]
                        [(assoc values out result) rest-queue])))))

(defn solve [wiring-map]
  (let [all-steps (iterate step-evaluation [wiring-map ["a"]])
        final-step (first (filter (comp empty? second) all-steps))]
    ((first final-step) "a")))

(defn part1 []
  (solve wiring-map))

(defn part2 []
  (let [overridden-wiring (assoc wiring-map "b" (part1))]
    (solve overridden-wiring)))



