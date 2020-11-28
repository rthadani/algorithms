(ns algorithms.epi.arrays)


(defn flag-value
  [color-map key]
  (get color-map key))

(defn dutch-national-flag
  [arr pivot-index colors]
  (letfn [(swap [arr i j]
            (let [tmp (arr i)]
              (assoc! arr i (arr j))
              (assoc! arr j tmp)))]
    (loop [result (transient arr)
           start 0
           equal 0
           end (dec (count arr))]
      (cond
        (> equal end) (persistent! result)
        (< (flag-value colors (get result equal)) (flag-value colors (get arr pivot-index)))
        (recur (swap result start equal) (inc start) (inc equal) end)
        (= (flag-value colors (get result equal)) (flag-value colors (get arr pivot-index)))
        (recur result start (inc equal) end)
        :else
        (recur (swap result equal end) start equal (dec end))))))

#_ (flag-value {:R 0 :W 1 :B 2} :W)
#_ (dutch-national-flag [:R :W :B :R :W :B :R :W :B] 1 {:R 0 :W 1 :B 2})


;Say you have an array for which the ith element is the price of a given stock on day i.
;If you were only permitted to complete at most one transaction (i.e., buy one and sell one share of the stock), design an algorithm to find the maximum profit.
(defn buy-sell-stock
  [& prices]
  (reduce
   (fn [[min-price profit] p]
     (cond
       (< p min-price) [p profit]
       (< profit (- p min-price)) [min-price (- p min-price)]
       :else [min-price profit]))
   [Integer/MAX_VALUE 0]
   prices))

#_ (second (buy-sell-stock 7,1,5,3,6,4))
#_ (second (buy-sell-stock 6,4))

(defn random-sampling
  ([k arr]
   (random-sampling 0 k arr))
  ([start k arr]
   (if (= start k)
     (subvec arr 0 k)
     (let [rest-arr (subvec arr (inc start))
           rand-index (rand-int (count rest-arr))]
       (recur (inc start) k (-> (assoc arr start (rest-arr rand-index))
                                (assoc rand-index (arr start))))))))
#_ (random-sampling 2 [1 2 3 4 5])


(defn spiral-ordering
  ([mat]
   (apply concat
          (for [i (range 0 (int (Math/ceil (* 0.5 (count mat)))))]
            (spiral-ordering mat i))))
  ([mat offset]
   (concat
    (for [j (range offset (dec (- (count mat) offset)))]
      (get-in mat [offset j]))
    (for [i (range offset (dec (- (count mat) offset)))]
      (get-in mat [i (dec (- (count mat) offset))]))
    (for [j (range (dec (- (count mat) offset)) offset -1)]
      (get-in mat [(dec (- (count mat) offset)) j]))
    (for [i (range (dec (- (count mat) offset)) offset -1)]
      (get-in mat [i offset])))))

#_ (spiral-ordering [[1 2 3 4]
                  [5 6 7 8]
                  [9 10 11 12]
                  [13 14 15 16]])

(defn increment-arbitrary-precision
  [arr]
  (reverse (first (reduce
                   (fn [[result first carry i] a]
                     (let [r (if first (inc a) (+ a carry))
                           carry (if (>= r 10) 1 0)
                           sum (if (>= r 10) (- r 10) r)]
                       (if (and (= i (count arr)) (= 1 carry))
                         [(concat result [sum 1]) false 0 (inc i)]
                         [(conj result sum) false carry (inc i)])))
                   [[] true 0 1]
                   (reverse arr)))))

#_ (increment-arbitrary-precision [1 9 9])


(defn sudoku-checker
  [arr]
  (letfn [(row [arr i] (for [j (range 0 (count (arr 0)))]
                         (get-in arr [i j])))
          (col [arr j] (for [i (range 0 (count (arr 0)))]
                         (get-in arr [i j])))
          (box [arr i j] (for [k (range (* i 3) (+ (* i 3) 3))
                               l (range (* j 3) (+ (* j 3) 3))]
                           (get-in arr [k l])))
          (has-no-duplicates [arr]
                                   (first (reduce
                                           (fn [[status bits-set] e]
                                             (cond
                                               (zero? e) [(and status true) bits-set]
                                               (and status (not (zero? (get bits-set (dec e))))) (reduced [false bit-set])
                                               :else [status (assoc bits-set (dec e) 1)]))
                                           [true (into [] (repeat 9 0))]
                                           arr)))])
  (and
   (every? #(has-no-duplicates %) (map #(row arr %) (range 0 9)))
   (every? #(has-no-duplicates %) (map #(col arr %) (range 0 9)))
   (every? #(has-no-duplicates %) (map #(box arr %) (range 0 9)))))


(defn delete-sorted-array-dupes
  [arr]
  (reduce
   (fn [result a]
     (if (= a (last result))
       result
       (conj result a)))
   []
   arr))

#_ (delete-sorted-array-dupes [1 1 1 2 3 3 4])

(defn primes-to-n
  ([n] (take-while #(<= % n) (primes-to-n n (iterate inc 2))))
  ([_ sieve]
   (println (first sieve))
   (cons (first sieve)
         (lazy-seq
          (primes-to-n 0
                       (filter
                        #(not= 0 (mod % (first sieve))) (rest sieve)))))))

#_ (primes-to-n 23)
(defn pascals-triangle
  [rows]
  (take rows (iterate #(mapv + (cons 0 %) (conj % 0)) [1])))
#_ (pascals-triangle 3)




