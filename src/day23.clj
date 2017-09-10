(ns day23
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day23_input.txt")))

(def valid-opcodes #{"hlf" "tpl" "inc" "jmp" "jie" "jio"})
(def valid-registers #{"a" "b"})

(defn parse-instruction [line]
  (let [[_ opcode argstring] (re-matches #"^(\w+) (.*)$" line)
        args (for [[_ reg num] (re-seq #"(\w+)|([+-]\d+)" argstring)]
               (do
                 (assert (not-every? nil? [reg num]))
                 (if (nil? reg)
                   (Integer/parseInt num)
                   (do
                     (assert (valid-registers reg))
                     (keyword reg)))))]
    (assert valid-opcodes opcode)
    {:opcode (keyword opcode)
     :args   args}))

(def program (mapv parse-instruction (string/split-lines input-string)))

(defn process-instruction [state instruction]
  (let [{:keys [opcode args]} instruction]
    (case opcode
      :hlf (let [[reg] args]
             (-> state
                 (update reg #(do (assert (zero? (mod % 2)))
                                  (quot % 2)))
                 (update :ip inc)))
      :tpl (let [[reg] args]
             (-> state
                 (update reg #(* 3 %))
                 (update :ip inc)))
      :inc (let [[reg] args]
             (-> state
                 (update reg inc)
                 (update :ip inc)))
      :jmp (let [[offset] args]
             (update state :ip #(+ offset %)))
      :jie (let [[reg offset] args
                 contents (get state reg)]
             (if (even? contents)
               (update state :ip #(+ % offset))
               (update state :ip inc)))
      :jio (let [[reg offset] args
                 contents (get state reg)]
             (if (= 1 contents)
               (update state :ip #(+ % offset))
               (update state :ip inc))))))

(defn step-machine [state program]
  (let [{ip :ip} state]
    (if (or (< ip 0)
            (>= ip (count program)))
      nil
      (let [instruction (nth program (:ip state))]
        (process-instruction state instruction)))))

(defn run-to-termination [initial-state program]
  (->> initial-state
       (iterate #(step-machine % program))
       (take-while identity)
       (last)))

(defn part1 []
  (:b
    (run-to-termination
      {:a  0
       :b  0
       :ip 0}
      program)))

(defn part2 []
  (:b
    (run-to-termination
      {:a  1
       :b  0
       :ip 0}
      program)))
