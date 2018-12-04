(ns algorithms.epi.bst
    (:require [algorithms.trees :refer :all]))

(defn is-bst?
  [{:keys [l r v] :as tree}]
 (println v)
 (cond 
   (or (nil? tree) (every? nil? [l r])) true
   (and (not (nil? l))) (and (<= (:v l) v) (is-bst? l)(is-bst? r))
   :else (and (> (:v r) v) (is-bst? l) (is-bst? r))))

#_ (def new-bst (create-tree [19 7 3 2 nil nil 5 nil nil 11 nil 17 13 nil nil nil 43 23 nil 37 29 nil 31 nil nil 41 nil nil 47 nil 53 nil nil]))
#_ (is-bst? new-bst)
#_ (is-bst? (create-tree [1 2 nil nil 3 4 nil nil 5 nil nil]))

(defn find-first-greater
  ([tree val]
   (find-first-greater tree val -1))
  ([{:keys [l r v] :as tree} val greatest-so-far]
   (cond (nil? tree) greatest-so-far
         (> v val) (find-first-greater l val v)
         :else (find-first-greater r  val greatest-so-far))))

#_ (find-first-greater new-bst 23)

(defn k-largest
  [{:keys [l r v] :as t} k result]
  (if (nil? t) result
      (let [right-largest (k-largest r k)
            elements (count right-largest)]
        (println (count right-largest) v)
        (if (< (count right-largest) k)
          (into [] (concat result (conj right-largest v) (k-largest l (- k elements))))
          right-largest))))


#_ (k-largest new-bst 4)


(defn lca
  [{:keys [l r v] :as bst} v1 v2]
  (cond  
   (nil? bst) nil
   (= v1 v2) v1
   :else (let [is-left-v1? (<= v1 v)
               is-left-v2? (<= v2 v)]
           (cond
             (= (not is-left-v1?) is-left-v2?) v
             is-left-v1? (lca l v1 v2)
             :else (lca r v1 v2)))))  

(defn min-height-bst
  [arr]
  (if (empty? arr)
    nil
    (let [mid (int (/ (count arr) 2))
          l (subvec arr 0 mid)
          r (subvec arr (inc mid) (count arr))]
         (-> (new-bst-node (arr mid))
             (assoc :l (min-height-bst l))
             (assoc :r (min-height-bst r))))))
(min-height-bst [1 2 3 4 5 6])

(defn range-lookup
  [[s e] {:keys [l r v] :as t} result]
(println result v)
  (if (nil? t)
    result
    (cond
      (and (<= v s) (<= v e))  (let [left-result (range-lookup [s e] l result)
                                                   right-result (range-lookup [s e] r (conj left-result v) )] 
                                               right-result)
      (and (> s v)) (range-lookup [s e] r result)
      :else (range-lookup [s e] l result))))
#_ (range-lookup [16 42] new-bst [])

      

 