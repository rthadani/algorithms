(ns algorithms.interviews
  (:require [clojure.string :as str]))

;;first attempt
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
      ["" available]) ;; request a bicycle
    (if (is-reserved? i current-reservations)
      ["" (+ available r)]
      ["" available])  ;;return one
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
      (sort-by second reservation-list)          ;;sort-by start times
      (reduce                                    ;;update entries as you process the list
        (fn [[current-reservations available] entry]
          (let [[i a] (process-entry entry available current-reservations)]
            (if (empty? i)
              [current-reservations a]
              [(conj current-reservations i) a])))
        [#{} available])
      (make-final-list reservation-list) )))    ;finally use the indexes to produce a reservation list

(rent-bicycles
  [[9 11 20]
   [10 12 20]
   [12 13 5]
   [14 16 41]] 40)


