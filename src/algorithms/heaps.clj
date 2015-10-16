(ns algorithms.heaps)

(defn swap [array i j]
  (assoc (assoc array i (array j)) j (array i)))

(defn max-heapify
  [array i]
  (let [left-index  (when (< (inc (* 2 i)) (count array))
                      (inc (* 2 i)))
        right-index (when (< (+ 2 (* 2 i)) (count array))
                      (+ 2 (* 2 i)))
        max-index   (cond
                      (every? some? [left-index right-index])
                      (condp = (max (array left-index) (array right-index) (array i))
                        (array left-index) left-index
                        (array right-index) right-index
                        i)
                      (some? left-index)
                      (if (> (array left-index) (array i))
                        left-index
                        i)
                      (some? right-index)
                      (if (> (array right-index) (array i))
                        right-index
                        i)
                      :else i)]
    (if (= max-index i)
      array
      (max-heapify (swap array max-index i) max-index))))

(defn build-max-heap
  [array]
  (loop [i   (dec (int (Math/floor (/ (count array) 2))))
         acc array]
    (if (< i 0)
      acc
      (recur (dec i) (max-heapify acc i)))))

(build-max-heap [3 2 7 4 10 1 20])

;;leftist heap

(defprotocol Heap
  (is-empty? [this])
  (merge [this other])
  (insert [this value])
  (rank [this])
  (find-min [this])
  (delete-min [this]))

(defrecord LeftistHeap [rank value left right])

(defn makeT
  [value this other]
  (if (>= (rank this) (rank other))
    (->LeftistHeap (inc (rank other)) value this other)
    (->LeftistHeap (inc (rank this)) value other this)))


(extend-protocol Heap
  nil
  (rank [_] 0)
  (merge [_ other] other)
  (is-empty? [_] true)

  LeftistHeap
  (is-empty? [this]
    (nil? this))

  (merge [{this-value :value this-left :left this-right :right :as this}
          {other-value :value other-left :left other-right :right :as other}]
    (cond
      (is-empty? other) this
      (<= this-value other-value) (->> (merge this-right other)
                                       (makeT this-value this-left))
      :else (->> (merge this other-right)
                 (makeT other-value other-left))))

  (insert [this value]
    (merge (->LeftistHeap 1 value nil nil) this))

  (rank [{this-rank :rank}]
    this-rank)

  (find-min [{:keys [value]}]
    value)

  (delete-min [{:keys [left right]}]
    (merge right left)))

(-> (->LeftistHeap 1 3 nil nil)
    (insert 2)
    (insert 7)
    (insert 4)
    (insert 10)
    (insert 1)
    (insert 20))


