(ns algorithms.leetcode.google.interview-process
    (:require [clojure.string :as str]))

;;forward-emails
(defn make-email
  [[local domain]]
  (str local "@" domain))

(defn remove-periods
  [[local domain]]
[(str/replace local #"\." "") domain])

(defn remove-after+
  [[local domain]]
  [(str/replace local #"\+.*" "") domain])

(defn split-email
  [email]
  (str/split email #"@"))

(def normalize-email (comp make-email remove-periods remove-after+ split-email))

(defn forward-emails [input]
  (->  (map normalize-email input)
       frequencies
       count))

#_  (remove-after+ ["testemail+david", "letcode.com"])
#_ (remove-periods ["test.e.mail" "letcode.com"])
#_ (forward-emails  ["test.email+alex@leetcode.com","test.e.mail+bob.cathy@leetcode.com","testemail+david@lee.tcode.com"])

;;license-key formatting

(defn insert-hyphen-or-letter
  [letters current-letter key-size]
  (if (= current-letter \-)
    letters
    (if (and (not (zero? (count letters))) (zero? (mod (count letters) key-size)))
        (str letters "-" current-letter)
        (str letters current-letter))))

(defn license-key-formatter
  [key size]
  (reverse (reduce (fn ([result letter] (insert-hyphen-or-letter result letter size))) "" (reverse key))))


;;fruit baskets
(defn next-partition
  [fruit-trees idx]
  (loop [i idx
         result []
         seen #{}]
    (cond (>= i (count fruit-trees)) result
          (> (count seen) 2) (butlast result)
          :else (recur
                 (inc i)
                 (conj result (fruit-trees i))
                 (conj seen (fruit-trees i))) ) ))

(defn fruit-into-baskets
  [fruit-trees]
  (->> (for [i (range 0 (count fruit-trees))]
         (next-partition fruit-trees i))
       (apply max-key count)
       count))
#_ (fruit-into-baskets [1 2 1])
#_ (fruit-into-baskets [3,3,3,1,2,1,1,2,3,3,4])