(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day12_input.txt")))

(def number-regex #"-?\d+")

(defmacro parse-let-bare [in binding-forms body]
  (if (empty? binding-forms)
    `[~body ~in]
    `(if-let [[~(first binding-forms) new-in#] (~(second binding-forms) ~in)]
       (parse-let-bare new-in# ~(drop 2 binding-forms) ~body))))

(defmacro parse-let [binding-forms body]
  `(fn [in#]
     (parse-let-bare in# ~binding-forms ~body)))

(defmacro parse-choice [& choices]
  `(fn [in#]
     (some identity (map #(% in#) [~@choices]))))

(defn parse-literal-char [c]
  (fn [in]
    (if (= (first in) c)
      [c (rest in)])))

(defn parse-any-char-but [c]
  (fn [in]
    (if-let [[result & new-in] in]
      (if (not (= c result))
        [result new-in]))))

(defn parse-any-char-of [& cs]
  (fn [in]
    (if-let [[result & new-in] in]
      (if ((set cs) result)
        [result new-in]))))

(defmacro parse-opt-bare [in p def]
  `(if-let [result# (~p ~in)]
     result#
     [~def ~in]))

(defmacro parse-opt
  ([p] (parse-opt ~p nil))
  ([p def] `(fn [in#]
              (parse-opt-bare in# ~p ~def))))

(defmacro parse-zero-or-more-bare [in p]
  `(loop [in# ~in
          acc# []]
     (if-let [[result# new-in#] (~p in#)]
       (recur new-in# (conj acc# result#))
       [acc# in#])))

(defmacro parse-zero-or-more [p]
  `(fn [in#]
     (let [ev-p# ~p]
       (parse-zero-or-more-bare in# ev-p#))))

(defmacro parse-one-or-more-bare [in p]
  `(parse-let-bare
     ~in
     [a# ~p
      vs# (parse-zero-or-more ~p)]
     (cons a# vs#)))

(defmacro parse-one-or-more [p]
  `(fn [in#]
     (let [evp# ~p]
       (parse-one-or-more-bare in# evp#))))

(def parse-json-string
  (parse-let [_ (parse-literal-char \")
              chars (parse-zero-or-more (parse-any-char-but \"))
              _ (parse-literal-char \")]
    (apply str chars)))

(def parse-digit
  (parse-any-char-of \0 \1 \2 \3 \4 \5 \6 \7 \8 \9))

(defmacro parse-list [element separator]
  `(fn [in#]
     (let [ev-element# ~element
           ev-separator# ~separator]
       (parse-opt-bare
         in#
         (parse-let [hd# ev-element#
                     more# (parse-zero-or-more (parse-let [~'_ ev-separator#
                                                           el# ev-element#]
                                                 el#))]
           (cons hd# more#))
         []))))

(def parse-json-value)

(def parse-kv-pair
  (parse-let [k parse-json-string
              _ (parse-literal-char \:)
              v parse-json-value]
    [k v]))


(def parse-json-object
  (parse-let [_ (parse-literal-char \{)
              kvs (parse-list parse-kv-pair (parse-literal-char \,))
              _ (parse-literal-char \})]
    (into {} kvs)))

(def parse-json-array
  (parse-let [_ (parse-literal-char \[)
              vs (parse-list parse-json-value (parse-literal-char \,))
              _ (parse-literal-char \])]
    vs))

(def parse-json-number
  (parse-let [s (parse-opt (parse-literal-char \-) "")
              ds (parse-one-or-more parse-digit)]
    (advent/parse-int (apply str (cons s ds)))))

(def parse-json-value
  (parse-choice
    parse-json-object
    parse-json-array
    parse-json-string
    parse-json-number))

(defn part1 []
  (let [matches (re-seq number-regex input-string)
        numbers (map advent/parse-int matches)]
    (apply + numbers)))

(defn process-value [v]
  (cond
    (number? v) v
    (string? v) 0
    (map? v) (if (some #{"red"} (vals v))
               0
               (apply + (map process-value (vals v))))
    (coll? v) (apply + (map process-value v))))

(defn part2 []
  (let [[parsed _] (parse-json-value input-string)]
    (process-value parsed)))