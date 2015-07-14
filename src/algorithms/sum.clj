(ns algorithms.sum)

;Find all sequences of digits 1 to 9 such that the sum is 15
;Atleast one digit appears exactly twice
;No digit appears more than twice

(defn valid-digits?
  [sequence]
  (let [values (vals (group-by identity sequence))]
    (and (not-any? #(> (count %) 2) values)
         (some #(= 2 (count %)) values))))

(defn- all-sequences
  [current-seq remaining result]
  (cond
    (< remaining 0)
      nil
    (and (= remaining 0) (valid-digits? current-seq))
      (swap! result conj current-seq)
    :else
      (doseq [i (range 1 10)
              :let [new-seq (conj current-seq i)]]
        (all-sequences new-seq (- remaining i) result))))

(defn sequences-with-sum
  [sum]
  (let [result (atom [])]
    (all-sequences [] sum result)
    (distinct (map sort @result))))

(sequences-with-sum 15)
