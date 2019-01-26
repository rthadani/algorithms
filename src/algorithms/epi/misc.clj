(ns algorithms.epi.misc)

(defn researcher-productivity
  [published]
  (->>  published
        (into (sorted-map-by  (fn [k1 k2] (compare [(get published k1) k1] [(get published k2) k2]))))
        (into [])
        (reduce
         (fn [prod [p c]]
           (if (< (- (count published) c) 0)
             (reduced prod)
             c)))))
;Sort the  papers by citations if there more more papers available than the current citation count then keep looking 
(researcher-productivity {\A 2, \B 1, \C 4})

(defn count-bits[i]
  (loop [current i
         bits 0]
    (cond
      (zero? current) bits
      (= 1 (bit-and current 1))(recur (bit-shift-right current 1) (inc bits))
      :else (recur (bit-shift-right current 1) bits))))

(count-bits 2)

(bit-shift-right 2 1)

(defn venmo-share 
  ([amounts]
   (venmo-share (map-indexed (fn [i a] [i a]) amounts)
                (int (/ (apply + amounts) (count amounts)))))
  ([idx-amounts required]
   (loop
    [remaining (map (fn [[i a]] [i (- required a)]) idx-amounts)
     result []]
     (if (every? (fn [[_ a]] (<= a 0)) remaining)
       result
       (let [[min-idx debit] (apply min-key second remaining)
             [max-idx credit] (apply max-key second remaining)
             next-round (remove (fn [[idx _]] (or (= idx min-idx) (= idx max-idx))) remaining)
             transfer-amount (if (>= (Math/abs debit) credit) credit (Math/abs debit))]
         (recur (conj next-round [min-idx (+ debit transfer-amount)] [max-idx (- credit transfer-amount)])
                (conj result (str "transfer " transfer-amount " from " max-idx " to " min-idx))))))))

#_ (venmo-share [1000 1100 100 400])

(defn greatest-binary-search
  [asks bid left right]
  (if (>= left right)
    (if (= right (dec count asks))
      -1
      (inc left))
    (let [mid (int (/ (+ left right) 2))]
      (cond 
        (= bid (asks mid)) mid
        (< bid (asks mid)) (recur asks bid left (dec mid))
        :else (recur asks bid (inc mid) right)))))

(defn match-bid-ask
  [bids asks]
  (let [sorted-bids (vec (sort  bids))
        sorted-asks (vec (sort  asks))]
    (second (reduce (fn [[unmatched-asks result] bid] 
                      (let [index (greatest-binary-search unmatched-asks bid 0 (dec (count unmatched-asks)))]
                        (if (< index 0)
                          [unmatched-asks result]
                          [(vec (concat (subvec unmatched-asks 0 index) (subvec unmatched-asks (inc index) (count unmatched-asks))))
                           (cons result [bid (unmatched-asks index)])])))
                    [sorted-asks []] sorted-bids))))