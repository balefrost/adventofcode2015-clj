(ns day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day21_input.txt")))

(def key-mapping {"Hit Points" :hp
                  "Damage"     :damage
                  "Armor"      :armor})

(defn parse-input [input-string]
  (into
    {}
    (map
      #(let [[_ key value] (re-matches #"^([\w ]+): (\d+)" %)
             mapped-key (key-mapping key)]
         [mapped-key (Integer/parseInt value)])
      (str/split-lines input-string))))

(def boss-stats (parse-input input-string))

(def weapons [{:name "Dagger" :cost 8 :damage 4 :armor 0}
              {:name "Shortsword" :cost 10 :damage 5 :armor 0}
              {:name "Warhammer" :cost 25 :damage 6 :armor 0}
              {:name "Longsword" :cost 40 :damage 7 :armor 0}
              {:name "Greataxe" :cost 74 :damage 8 :armor 0}])

(def armor [{:name "Leather" :cost 13 :damage 0 :armor 1}
            {:name "Chainmail" :cost 31 :damage 0 :armor 2}
            {:name "Splintmail" :cost 53 :damage 0 :armor 3}
            {:name "Bandedmail" :cost 75 :damage 0 :armor 4}
            {:name "Platemail" :cost 102 :damage 0 :armor 5}])

(def rings [{:name "Damage +1" :cost 25 :damage 1 :armor 0}
            {:name "Damage +2" :cost 50 :damage 2 :armor 0}
            {:name "Damage +3" :cost 100 :damage 3 :armor 0}
            {:name "Defense +1" :cost 20 :damage 0 :armor 1}
            {:name "Defense +2" :cost 40 :damage 0 :armor 2}
            {:name "Defense +3" :cost 80 :damage 0 :armor 3}])

(def loadouts
  (for [weapon (map vector weapons)
        armor (cons [] (map vector armor))
        rings (concat [[]] (advent-util/combinations rings 1) (advent-util/combinations rings 2))]
    (concat weapon armor rings)))

(defn evaluate-loadout [loadout]
  (let [{:keys [cost damage armor]} (reduce
                                      (fn [a b]
                                        (merge-with + a (dissoc b :name)))
                                      {:cost 0 :damage 0 :armor 0}
                                      loadout)]
    {:cost         cost
     :player-stats {:hp     100
                    :damage damage
                    :armor  armor}}))

(defn simulate-fight-round [active-stats other-stats]
  (let [raw-dmg (- (:damage active-stats) (:armor other-stats))
        damage (max 1 raw-dmg)
        new-hp (- (:hp other-stats) damage)]
    (assoc other-stats :hp new-hp)))


(defn simulate-fight-history-helper [active other]
  (let [[active-key active-stats] active
        [other-key other-stats] other
        other-stats1 (simulate-fight-round active-stats other-stats)
        new-other [other-key other-stats1]
        this-entry (into {} [active other])]
    (if (> (:hp other-stats1) 0)
      (cons this-entry (simulate-fight-history-helper new-other active))
      [this-entry (into {} [active new-other])])))

(defn simulate-fight [player-stats boss-stats]
  (let [last-round (last (simulate-fight-history-helper [:player player-stats] [:boss boss-stats]))]
    (if (<= (:hp (:player last-round)) 0)
      :boss
      :player)))

(defn part1 []
  (let [successful-loadouts (for [loadout loadouts
                                  :let [evaluated-loadout (evaluate-loadout loadout)
                                        {:keys [player-stats]} evaluated-loadout]
                                  :when (= :player (simulate-fight player-stats boss-stats))]
                              evaluated-loadout)]
    (:cost (first (sort-by :cost successful-loadouts)))))

(defn part2 []
  (let [failed-loadouts (for [loadout loadouts
                              :let [evaluated-loadout (evaluate-loadout loadout)
                                    {:keys [player-stats]} evaluated-loadout]
                              :when (= :boss (simulate-fight player-stats boss-stats))]
                          evaluated-loadout)]
    (:cost (last (sort-by :cost failed-loadouts)))))