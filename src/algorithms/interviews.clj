(ns algorithms.interviews
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.predicates :as tp]))

;;first attempt
;;given a reservation list that contains the start and end end times for a day's reservations
;;produce a reservation list for bicycles

#_(defn make-reservation
    [return-list acc available [start end req]]
    (let [before-request (+ available (get return-list start 0))]
      (if (> before-request req)
        [true (- before-request req)]
        [false before-request])))

#_(defn rent-bicycles
    ([reservations available]
     (rent-bicycles reservations available (make-return-list reservations)))
    ([reservations available return-list]
     (reduce
       (fn [[accepted-state avail] r]
         (let [[result available] (make-reservation return-list accepted-state avail r)]
           [(conj accepted-state result) available]))
       [[] available]
       reservations)))

(defn convert-list
  "go over each reservation and add an event that starts at the end time that indicates a return
   a 'o' indicates an outbound request(a rental) and an i an inbound one (a return) "
  [reservations]
  (first
    (reduce (fn [[result index] [s e r]]
              [(conj result [(str "o" index) s e r] [(str "i" index) e e r]) (inc index)])
            [[] 0]
            reservations)))

(defn is-reserved?
  "if the index exists in the current reservation then it can be returned"
  [index current-reservations]
  (some #{(str/replace index #"i" "o") index} current-reservations))

(defn process-entry
  "takes a single entry(inbound/outbound) and the current reservations and returns
   the index of an accepted rental and the availablility after"
  [[i s e r] available current-reservations]
  (if (.startsWith i "o")
    (if (>= available r)
      [i (- available r)]
      ["" available])                                       ;; request a bicycle
    (if (is-reserved? i current-reservations)
      ["" (+ available r)]
      ["" available])                                       ;;return one
    ))

(defn make-final-list
  "filter those reservations that made it in the reserved indexes"
  [reservation-list [reserved-indexes _]]
  (filter #(some #{(first %)} reserved-indexes) reservation-list))

(defn rent-bicycles
  "rent them bicycles"
  [reservations available]
  (let [reservation-list (convert-list reservations)]
    (->>
      (sort-by second reservation-list)                     ;;sort-by start times
      (reduce                                               ;;update entries as you process the list
        (fn [[current-reservations available] entry]
          (let [[i a] (process-entry entry available current-reservations)]
            (if (empty? i)
              [current-reservations a]
              [(conj current-reservations i) a])))
        [#{} available])
      (make-final-list reservation-list))))                 ;finally use the indexes to produce a reservation list

#_(rent-bicycles
  [[9 11 20]
   [10 12 20]
   [12 13 5]
   [14 16 41]] 40)

;;fizzbuzz
(defn fizz-buzz
  [a]
  (match [(mod a 3) (mod a 5)]
         [0 0] "fizzbuzz"
         [0 _] "fizz"
         [_ 0] "buzz"
         :else a))

(defn uppercase-char?
  [char]
  (and (>= (int char) (int \A))
       (<= (int char) (int \Z))))

(defn count-upper-case
  [string]
  (-> (filter uppercase-char? string)
      count))

;;date of business days from start day
(defn next-business-day
  [current-day]
  (->> (iterate #(t/plus % (t/days 1)) current-day)
       (filter #(not (or (tp/saturday? %) (tp/sunday? %))))
       second))

(defn business-days
  [start-date n]
  (let [s (t/plus (tf/parse (tf/formatter "yyyy-MM-dd") start-date) (t/days 1))]
    (->> (iterate next-business-day s)
         (take n)
         last)))

;;in-memory file system
(defrecord Entry [name is-file sub-entries contents])
(def root (Entry. "" false {} ""))
(defn add-entry
  [path contents root]
  (cond (empty? path) (if (empty? contents)
                        root
                        (Entry. (:name root) true {} (str (:contents root) contents)))
        (empty? (first path)) (add-entry (rest path) contents root)
        :else (let [next-part  (first path)
                    next-entry (if (contains? (:sub-entries root) next-part)
                                 ((:sub-entries root) next-part)
                                 (Entry. next-part false {} ""))] 
                (Entry. (:name root)
                        (:is-file root)
                        (assoc (:sub-entries root) next-part (add-entry (rest path) contents next-entry))
                        (:contents root)))))
(defn get-entry 
  [path root]
  (cond 
    (empty? path) root
    (empty? (first path)) (get-entry (rest path) root)
    :else 
    (when (contains? (:sub-entries root) (first path)) 
      (get-entry (rest path) ((:sub-entries root) (first path)) ))))

(defn list-entry 
  [path root]
  (let [entry (get-entry path root)]
    (when entry
      (if (:is-file entry) 
        (:name entry)
        (keys (:sub-entries entry))))))

(defn add-dir 
  [path root]
  (add-entry (clojure.string/split path #"/") "" root))

(defn add-file
  [path contents root]
  (add-entry (clojure.string/split path #"/") contents root))

(defn ls
  [path root]
  (list-entry (clojure.string/split path #"/") root))



#_ (->> (add-entry ["a"] "" root)
        (add-entry ["a" "b"] "") 
        (add-entry ["a" "c"] "hello") 
        (add-entry ["a" "c"] " world"))

#_ (->> (add-dir "/a/b" root)
        (add-file "/a/b/c" "hello")
        (add-dir "/a/b/d")
        (add-file "/a/b/c" " world")
        #_(ls "/a/b"))


(defn make-graph
  [edges]
  (reduce
   (fn [g [s e]] (-> (update g s conj e) 
                     (update e conj s) ))
   {}
   edges))
#_ (make-graph [[:a :b] [:b :c]])