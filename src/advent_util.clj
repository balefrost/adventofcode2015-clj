(ns advent-util
  (:import (java.security MessageDigest)))

(def ^chars -byte-lookup (char-array [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f]))

(defn bytes-to-hex
  ([^bytes bs]
   (bytes-to-hex bs (alength bs)))
  ([^bytes bs count]
   (let [sb (StringBuilder. ^Integer (* 2 count))]
     (loop [i 0]
       (if (< i count)
         (let [b (aget bs i)
               high (bit-and (unsigned-bit-shift-right b 4) 0x0f)
               low (bit-and b 0x0f)]
           (.append sb (aget ^chars -byte-lookup high))
           (.append sb (aget ^chars -byte-lookup low))
           (recur (inc i)))
         (.toString sb))))))

(def ^ThreadLocal -threadlocal-digest-algorithm (proxy [ThreadLocal] []
                                                  (initialValue [] (MessageDigest/getInstance "MD5"))))

(defn compute-md5 ^bytes [bytes]
  (let [^MessageDigest md (.get -threadlocal-digest-algorithm)]
    (.reset md)
    (.digest md bytes)))

(defn compute-md5-string [^String string]
  (bytes-to-hex (compute-md5 (.getBytes string "US-ASCII"))))

(defn parse-int [str] (Integer/parseInt str))

(defn parse-long [str] (Long/parseLong str))

(defn -combinations-helper [coll n acc]
  (lazy-seq
    (if (= n 0)
      [acc]
      (if (empty? coll)
        nil
        (concat
          (-combinations-helper (rest coll)
                                (dec n)
                                (conj acc (first coll)))
          (-combinations-helper (rest coll)
                                n
                                acc))))))

(defn combinations [coll n]
  (-combinations-helper coll n []))

(defn permutations [s]
  (if (empty? s)
    [[]]
    (mapcat
      (fn [el]
        (let [without-el (remove #(= el %) s)]
          (map #(cons el %) (permutations without-el))))
      s)))

