(ns algorithms.heaps)

;;leftist heap

(defprotocol Heap
  (is-empty? [this])
  (merge-heap [this other])
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
  (merge-heap [_ other] other)
  (is-empty? [_] true)

  LeftistHeap
  (is-empty? [this]
    (nil? this))

  (merge-heap [{this-value :value this-left :left this-right :right :as this}
               {other-value :value other-left :left other-right :right :as other}]
    (cond
      (is-empty? other) this
      (<= this-value other-value) (->> (merge-heap this-right other)
                                       (makeT this-value this-left))
      :else (->> (merge-heap this other-right)
                 (makeT other-value other-left))))

  (insert [this value]
    (merge-heap (->LeftistHeap 1 value nil nil) this))

  (rank [{this-rank :rank}]
    this-rank)

  (find-min [{:keys [value]}]
    value)

  (delete-min [{:keys [left right]}]
    (merge-heap right left)))

(-> (->LeftistHeap 1 3 nil nil)
    (insert 2)
    (insert 7)
    (insert 4)
    (insert 10)
    (insert 1)
    (insert 20))

;;binary heap
(defn parent
  [index]
  (/ index 2))

(defn swap
  [heap from to]
  (let [temp (get heap from)]
    (-> (assoc heap from (get heap to))
        (assoc to temp))))

(defn heapify-up
  [heap index is-lighter?]
  (cond
    (zero? index) heap
    (is-lighter? (get heap index) (get heap (parent index)))
    (-> (swap heap index (parent index))
        (heapify-up (parent index) is-lighter?))
    :else heap))

(defn lightest-child
  [heap index is-lighter?]
  (let [left (* 2 index)
        right (inc (*2 index))]
    (cond
      (>= left count) nil
      (>= right count) left
      (is-lighter? (get heap left) (get heap right)) left
      :else right)))

(defn heapify-down
  [heap index is-lighter?]
  (if (= index (dec (count heap)))
    heap
    (if-let [lightest-index (lightest-child heap index is-lighter?)]
      (if (is-lighter? (get heap lightest-index) (get heap index))
        (heapify-down (swap heap index lightest-index) lightest-index is-lighter?)
        heap)
      heap)))

(def max-heap-lighter >)
(def min-heap-lighter <)

(defprotocol Heap)

(defn insert
  [heap element is-lighter?]
  (as-> (conj heap element) $
        (heapify-up $ (dec (count $)) is-lighter?)))

(defn find-min
  [heap]
  (if (empty? heap)
    nil
    (first heap)))

(defn delete-min
  [heap is-lighter?]
  (if (or (empty? heap) (empty? (rest heap)))
    []
    (-> (swap heap (dec (count heap)) 0)
        (subvec 0 (count heap))
        (heapify-down 0 is-lighter?))))

(defn increase-key
  [heap index value is-lighter?]
  (-> (update-in heap [index] + value)
      (heapify-down index is-lighter?)))

(defn decrease-key
  [heap index value is-lighter?]
  (-> (update-in heap [index] - value)
      (heapify-up index is-lighter?)))

