(ns algorithms.sum)

;Find all sequences of digits 1 to 9 such that the sum is 15
;Atleast one digit appears exactly twice
;No digit appears more than twice

(defn valid-digits?
  [sequence]
  (let [values (vals (group-by identity sequence))]
    (and (not-any? #(> (count %) 2) values)
         (some #(= 2 (count %)) values))))

(defn all-sequences
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

;(sequences-with-sum 15)

(defn all-sub-sequences
  [array]
  (for [i (range 0 (count array))
        j (range i (inc (count array)))]
    (subvec array i j)))

(defn sum
  [array]
  (if (empty? array)
    0
    (apply + array)))

(defn naive-max-subsequence
  [array]
  (->> (all-sub-sequences array)
       (map sum)
       (apply max)))

(defn mid-array-sum
  [array mid]
  )

(defn divide-and-conquer-subseq
  [array]
  (if (= 1 (count array))
    (get array 0)
    (let [mid (/ (count array) 2)
          leftSum (divide-and-conquer-subseq (subvec array 0 mid))
          rightSum (divide-and-conquer-subseq (subvec array mid (inc (count array))))
          midsum (mid-array-sum array mid)]
      (max leftSum rightSum midsum)))
  )
