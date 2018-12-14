(ns algorithms.epi.strings
  (:require [clojure.string :as str]))

(defn atoi
  [string]
  (if (empty? string)
    0
    (+ (* (Math/pow 10 (count (rest string))) (- (int (first string)) (int \0))) 
       (atoi (rest string)))))

#_ (atoi (seq "100"))

(def digit-map
  {\I 1
   \V 5
   \X 10
   \L 50
   \C 100
   \D 500
   \M 1000})

(defn roman-to-int
  [roman]
  (if (= 1 (count roman))
    (digit-map (first roman))
    (if (> (digit-map (fnext roman)) (digit-map (first roman)))
      (-  (roman-to-int (rest roman)) (digit-map (first roman)))
      (+ (digit-map (first roman))(roman-to-int (rest roman))))))

#_ (roman-to-int (seq "MCMXCIX"))
