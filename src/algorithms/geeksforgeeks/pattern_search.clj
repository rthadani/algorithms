(ns algorithms.geeksforgeeks.pattern-search
  (:import (java.util Random)))

;;naive pattern search
(defn naive-search
  [pattern string]
  (for [i (range 0 (inc (- (count string) (count pattern))))
        :let [substr (.substring string i (+ i (count pattern)))]
        :when (= pattern substr)]
    i))
#_(naive-search "AABA" "AABAACAADAABAABA")

;;kmp
(defn build-t-table
  [pattern]
  (loop
    [i 0 pos 1 acc [0]]
    (cond
      (= pos (count pattern)) acc
      (= (nth pattern pos) (nth pattern i)) (recur (inc i) (inc pos) (conj acc (inc i)))
      :else (if (zero? i)
              (recur i (inc pos) (conj acc 0))
              (recur (nth acc (dec i)) pos acc)))))
#_(build-t-table "AAAA")
#_(build-t-table "AAACAAAAAC")

(defn knuth-morris-pratt
  ([pattern string]
   (knuth-morris-pratt pattern string (build-t-table pattern)))
  ([pattern string t]
   (println t)
   (loop [m 0 i 0 acc []]
     #_(println m i acc)
     (cond
       (>= (+ m i) (count string)) acc
       (= i (dec (count pattern))) (recur (- (+ m i) (nth t (dec i))) (nth t (dec i)) (conj acc m))
       (= (nth pattern i) (nth string (+ m i))) (recur m (inc i) acc)
       :else (if (zero? i)
               (recur (inc m) i acc)
               (recur (- (+ m i) (nth t (dec i))) (nth t (dec i)) acc))))))

;;rabin karp

(defn hash-rk
  [key R mod-prime]
  (reduce
    (fn [acc key] (-> (* acc R)
                      (+ (int key))
                      (mod mod-prime)))
       key))

(defn precompute-leading-digit-remover
  [pattern mod-prime R]
  (reduce
    (fn [acc _] (mod (* R acc) mod-prime))
    1
    (range 0 (dec (count pattern)))))

(defn check
  [string pattern]
  (= string pattern))

;;at each stage remove leading hash and add the trailing char tedious but easy
#_(defn rabin-karp
  [pattern string]
  (let [mod-prime (longValue (BigInteger/probablePrime 31 (Random.)))
        remove-value (precompute-leading-digit-remover pattern mod-prime 256)
        pattern-hash (hash-rk pattern 256 mod-prime)
        [i text text-hash]]
    (loop [i 0
           text-hash (hash-rk text 256 mod-prime)
           acc []]
      (cond
        (>= i (count string)) acc
        (and (= text-hash pattern-hash) (check text pattern)) (recur (inc i) (.substring string (inc i) (count pattern)))))
    ))

;;finite automata
(defn copy-row-value
  [fa from to]
  (assoc fa to (get fa from)))

(defn make-state-machine
  [pattern]
  (let [fa-table (transient (mapv (fn [_] (into [] (repeat 255 0))) (range 0 (inc (count pattern)))))]
    (assoc! fa-table 0 (assoc (get fa-table 0) (int (first pattern)) 1))
    (reduce
      (fn [[lps i] char]
        (assoc! fa-table i (get fa-table lps))
        (let [row (get fa-table i)]
          (assoc! fa-table i (assoc row (int char) (inc i))))
        [(get-in fa-table [lps (int char)]) (inc i)])
      [0 1]
      (str (rest pattern) (first pattern)))
    (persistent! fa-table)))

(defn fa-search
  [pattern string]
  (let [sm (make-state-machine pattern)]
    (loop [i 0 j 0 acc []]
      (cond
        (= i (count string)) acc
        (= j (count pattern) (recur (inc i) (get-in sm [j (int (nth string i))]) (conj acc (- i (count pattern)))))
        :else (recur (inc i) (get-in sm [j (int (nth string i))]) acc)))))
