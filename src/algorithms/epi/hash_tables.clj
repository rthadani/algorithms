(ns algorithms.epi.hash-tables
    (:require [clojure.string :as str]))

(defn nearest-repeated-entry
  [sentence]
  (loop [words (str/split sentence #" ")
         distance (Integer/MAX_VALUE)
         indices {}
         i 0]
    (let [word (first words)
          word-idx (get indices word)
          remaining (rest words)
          new-indices (assoc indices word i)]
      (cond (empty? words) (if (= distance Integer/MAX_VALUE) -1 distance)
            word-idx (recur remaining (min distance (- i word-idx)) new-indices (inc i))
            :else (recur remaining distance new-indices (inc i))))))
#_ (nearest-repeated-entry "All work and no play makes for no work no fun and no results")

(declare subarray-length)
(defn smallest-subarray-covering-values
  [words keywords]
  (loop [words words
         found-keywords {}
         current-size (Integer/MAX_VALUE)
         i 0]
        (println words found-keywords)
    (if (empty? words)
      current-size
      (let [word (first words)
            remaining (rest words)
            found (keywords word)
            latest-keywords (if found (assoc found-keywords word i) found-keywords)]
        (if found
          (recur remaining latest-keywords (min current-size (subarray-length keywords latest-keywords)) (inc i))
          (recur remaining latest-keywords current-size (inc i)))))))    

 (defn- subarray-length
  [keywords currently-known]
  (println keywords currently-known)
  (if (< (count currently-known) (count keywords)) 
    (Integer/MAX_VALUE)
    (- (apply max (vals currently-known)) (apply min (vals currently-known)))))       

#_ (smallest-subarray-covering-values ["apple" "banana" "apple" "apple" "dog" "cat" "apple" "dog" "banana" "apple" "cat" "dog"] #{"banana" "cat"})