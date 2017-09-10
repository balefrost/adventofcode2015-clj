(ns day22
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-util :as advent]))

(def input-string (slurp (io/resource "day22_input.txt")))

(def key-mapping {"Hit Points" :hp
                  "Damage"     :damage})

(defn parse-input [input-string]
  (into
    {}
    (map
      #(let [[_ key value] (re-matches #"^([\w ]+): (\d+)" %)
             mapped-key (key-mapping key)]
         [mapped-key (Integer/parseInt value)])
      (string/split-lines input-string))))

(def player-stats {:hp         50
                   :armor      0
                   :mana       500
                   :mana-spent 0})

(def boss-stats (parse-input input-string))

(defn check-instant-spell [cost]
  (fn [state]
    (>= (get-in state [:player-stats :mana]) cost)))

(defn check-timed-spell [name cost]
  (fn [state]
    (and
      (not (get-in state [:effects name]))
      (>= (get-in state [:player-stats :mana]) cost))))

(defn register-effect [opts]
  (let [{:keys [name duration effect end-effect]} opts]
    (fn [state]
      (assoc-in state [:effects name] {:remaining  duration
                                       :effect     effect
                                       :end-effect end-effect}))))

(defn update-boss [stat amount]
  (fn [state]
    (update-in state [:boss-stats stat] + amount)))

(defn update-player [stat amount]
  (fn [state]
    (update-in state [:player-stats stat] + amount)))

(defn set-player [stat amount]
  (fn [state]
    (assoc-in state [:player-stats stat] amount)))

(defn instant-spell [opts]
  (let [{:keys [name cost effect]} opts]
    (-> opts
        (dissoc :effect)
        (assoc :applicable (check-instant-spell cost)
               :initial-effect (comp
                                 (update-player :mana-spent cost)
                                 (update-player :mana (- cost))
                                 effect)))))

(defn timed-spell [opts]
  (let [{:keys [name cost end-effect]} opts]
    (-> opts
        (dissoc :effect)
        (assoc :applicable (check-timed-spell name cost)
               :initial-effect (comp
                                 (update-player :mana-spent cost)
                                 (update-player :mana (- cost))
                                 (register-effect opts))
               :end-effect end-effect))))

(def spells [(instant-spell {:name   "Magic Missile"
                             :cost   53
                             :effect (update-boss :hp -4)})
             (instant-spell {:name   "Drain"
                             :cost   73
                             :effect (comp
                                       (update-boss :hp -2)
                                       (update-player :hp 2))})
             (timed-spell {:name       "Shield"
                           :cost       113
                           :duration   6
                           :effect     (set-player :armor 7)
                           :end-effect (set-player :armor 0)})
             (timed-spell {:name     "Poison"
                           :cost     173
                           :duration 6
                           :effect   (update-boss :hp -3)})
             (timed-spell {:name     "Recharge"
                           :cost     229
                           :duration 5
                           :effect   (update-player :mana 101)})])

(def lookup-spell (into {} (map (fn [spell] [(:name spell) spell]) spells)))

(def initial-state {:player-stats player-stats
                    :boss-stats   boss-stats
                    :effects      {}})

(defn update-effect [state effect-name effect-info]
  (let [{:keys [remaining effect end-effect]} effect-info
        effect (or effect identity)
        end-effect (or end-effect identity)
        state1 (effect state)]
    (if (= remaining 1)
      (-> state1
          (end-effect)
          (update :effects #(dissoc % effect-name)))
      (update-in state1 [:effects effect-name :remaining] dec))))

(defn update-effects [state]
  (reduce-kv update-effect state (:effects state)))

(defn cast-spell [state spell]
  (let [{:keys [name applicable initial-effect]} spell]
    (assert (applicable state) (str name " not applicable"))
    (initial-effect state)))

(defn handle-boss-attack [state]
  (let [raw-damage (get-in state [:boss-stats :damage])
        player-armor (get-in state [:player-stats :armor])
        effective-damage (max 1 (- raw-damage player-armor))]
    (update-in state [:player-stats :hp] - effective-damage)))

(defn boss-dead? [state]
  (<= (get-in state [:boss-stats :hp]) 0))

(defn player-dead? [state]
  (<= (get-in state [:player-stats :hp]) 0))

(defn resolve-turn [state spell]
  (let [state1 (cast-spell state spell)
        state2 (update-effects state1)
        state3 (handle-boss-attack state2)
        state4 (update-effects state3)]
    (cond
      (boss-dead? state1) {:winner :player
                           :state  state1}

      (boss-dead? state2) {:winner :player
                           :state  state2}

      (player-dead? state3) {:winner :boss
                             :state  state3}

      (boss-dead? state4) {:winner :player
                           :state  state4}

      :else {:winner nil
             :state  state4})))


(defn castable-spells [state]
  (filter #((:applicable %) state) spells))

(defn pump-states [state-space]
  (let [{:keys [states min-mana]} state-space]
    (if (empty? states)
      state-space
      (let [[state & states] states
            spells (castable-spells state)
            new-states (map #(resolve-turn state %) spells)
            grouped-states (group-by :winner new-states)
            player-win-states (map :state (grouped-states :player))
            mana-spent-for-wins (map #(get-in % [:player-stats :mana-spent]) player-win-states)
            updated-min-mana (apply min min-mana mana-spent-for-wins)
            no-win-states (map :state (grouped-states nil))]
        {:states (sort-by
                   #(get-in % [:boss-stats :hp])
                   (filter
                     #(>= min-mana (get-in % [:player-stats :mana-spent]))
                     (concat no-win-states states)))
         :min-mana updated-min-mana}))))

(def initial-state-space
  {:states [initial-state]
   :min-mana Integer/MAX_VALUE})


(defn part1 []
  (loop [iterations 0
         state-space initial-state-space]
    (let [{:keys [states min-mana]} state-space]
      ;(if (= 0 (mod iterations 500))
      ;  (println "states: " (count states) ", min-mana: " min-mana))
      (if (empty? (:states state-space))
        min-mana
        (recur (inc iterations) (pump-states state-space))))))


