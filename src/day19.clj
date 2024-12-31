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
        rules (->> replacement-lines
                   (map parse-replacement)
                   (remove nil?))
        formula (last lines)]
    {:rules   rules
     :formula (partition-formula formula)}))

(def input (parse-input input-string))

(defn splice [seq idx n & new]
  (let [[a b] (split-at idx seq)
        c (drop n b)]
    (concat a new c)))

(defn process-rule [rule formula]
  (for [{:keys [from to]} rule
        index (indexes-of formula from)]
    (apply splice formula index 1 to)))

(defn part1 []
  (let [{:keys [rules formula]} input]
    (count (set (process-rule rules formula)))))

(defn find-terminals [state]
  (let [{:keys [rules formula]} state
        nonterminals (into #{} (map :from) rules)]
    (into #{}
          (filter
            (comp not nonterminals)
            (concat formula (mapcat :to rules))))))

(defn augment-nonterminals [state terminals]
  (let [{:keys [rules formula]} state]
    (letfn [(repl-1 [sym]
              (if (terminals sym) sym [sym 0]))]
      (let [rules (for [{:keys [from to]} rules
                        :let [from (repl-1 from)
                              to (mapv repl-1 to)]]
                    {:from from :to to})
            formula (mapv repl-1 formula)]
        (assoc state :rules rules :formula formula)))))

(defn eliminate-nonsolitary-terminals [state terminals next-id]
  (let [rules (:rules state)
        affected-rules (into #{} (filter #(and (> (count (:to %)) 1)
                                               (some terminals (:to %)))) rules)
        to-replace (into #{} (mapcat #(filter terminals (:to %))) affected-rules)
        replacement-map (into {} (map #(vector % [% (next-id)])) to-replace)
        rules (distinct
                (mapcat
                  (fn [{:keys [from to] :as original-rule}]
                    (if (contains? affected-rules original-rule)
                      (let [new-rules (for [[to from] replacement-map]
                                        {:from from :to [to]})
                            rewritten-rule {:from from
                                            :to   (into [] (for [r to]
                                                             (get replacement-map r r)))}]
                        (concat new-rules [rewritten-rule]))
                      [original-rule]))
                  rules))]
    (assoc state :rules rules)))

(defn eliminate-long-rules [state next-id]
  (letfn [(gen-short-rules [top-from from to]
            (lazy-seq
              (if (< (count to) 3)
                [{:from from :to (vec to)}]
                (let [[a & bs] to
                      _ (assert vector? a)
                      newsym [top-from (next-id)]]
                  (cons
                    {:from from :to [a newsym]}
                    (gen-short-rules top-from newsym bs))))))]
    (let [rules (:rules state)
          rules (into
                  []
                  (mapcat (fn [rule]
                            (gen-short-rules
                              (first (:from rule))
                              (:from rule)
                              (:to rule))))
                  rules)]
      (assoc state :rules rules))))

(defn eliminate-epsilon-rules [state]
  ; TODO: implement this
  state)

(defn eliminate-unit-rules [state]
  ; TODO: implement this
  state)

(defn transform-productions [state]
  (let [id-generator (atom 0)
        next-id (fn [] (let [ret @id-generator]
                         (swap! id-generator inc)
                         ret))
        terminals (find-terminals state)
        state (augment-nonterminals state terminals)
        state (update state :rules #(cons {:from "START" :to ["e"]} %))
        state (eliminate-nonsolitary-terminals state terminals next-id)
        state (eliminate-long-rules state next-id)
        state (eliminate-epsilon-rules state)
        state (eliminate-unit-rules state)]
    state))

(defn pretty-state [state]
  (letfn [(format-symbol [s]
            (cond
              (string? s) s
              (vector? s) (str "[" (str/join " " (map format-symbol s)) "]")
              (number? s) (str s)
              :else (assert false "invalid symbol")))
          (format-seq [s]
            (str/join " " (mapv format-symbol s)))]
    {:rules   (for [{:keys [from to]} (:rules state)]
                {:from from :to (format-seq to)})
     :formula (format-seq (:formula state))}))

(defn part2 []
  (-> input
      transform-productions
      pretty-state))
