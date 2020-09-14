(ns algorithms.leetcode.facebook.trees-graphs
  (:require [algorithms.trees :refer :all]
            [clojure.string :as string]))

(defn valid-bst?
 ([root] (valid-bst? root nil))
  ([r i]
   (if (nil? r)
     true
     (let [l-bst (valid-bst? (:l r) i)]
       (cond
         (not l-bst) false
         (and i (< (:v r) i)) false
         :else (valid-bst? (:r r) (:v r)))))))

#_ (valid-bst? (create-tree [5 1 nil nil 4 3 nil nil 6 nil nil ]))
#_ (valid-bst? (create-tree [2 1 nil nil 3 nil nil]))
#_ (valid-bst? (create-tree [1 1 nil nil]))

(defn attach-at-end
  [left-tail right-tail]
  (if (nil? (:r left-tail)) 
    (assoc left-tail :r right-tail)
    (assoc left-tail :r (attach-at-end (:r left-tail) right-tail))))

;;flatten binary tree
(defn flatten-binary-tree
  [r]
  (cond 
    (nil? r) r
    (and (nil? (:l r)) (nil? (:r r))) r
    :else 
    (let [left-tail (flatten-binary-tree (:l r))
          right-tail (flatten-binary-tree (:r r))]
      (if (not (nil? left-tail))
        (assoc r :r (attach-at-end left-tail right-tail) #_(assoc left-tail :r right-tail) :l nil)
        (assoc r :r right-tail :l nil)))))

#_ (flatten-binary-tree (create-tree [1 2 3 nil nil 4 nil nil 5 nil 6 nil nil]))

;;binary-tree-max-path-sum
(defn max-path-sum
  [r]
  (cond 
    (nil? r) Integer/MIN_VALUE 
    :else 
    (let [ml (max-path-sum (:l r))
          mr (max-path-sum (:r r))]
      (max (:v r) (+ (:v r) mr ml) (+ (:v r) (max mr ml)) ml mr))))

#_ (max-path-sum (create-tree [1 2 nil nil 3 nil nil]) )
#_ (max-path-sum (create-tree [-10 9 nil nil 20 15 nil nil 7 nil nil]) )
#_ (max-path-sum (create-tree [2 -1  nil nil nil]) )

;;clone graph

;;binary tree right side
;;last for a level order search
(defn right-side-view
  [t]
  (loop [q [t]
         r []]
    (if (empty? q)
      r
      (let [new-level (reduce (fn [a e] (into a (remove nil? (vals (select-keys e [:l :r]))))) [] q)]
        (recur new-level (conj r (:v (last q))))))))
#_ (right-side-view (create-tree [1 2 nil 5 nil nil 3 nil 4 nil nil]))


;;number of islands
(defn flood-island-with-water
  [grid row col]
  (cond
    (or (< row 0) (< col 0)) grid
    (or (>= row (count grid)) (>= col (count (grid 0)))) grid
    (= "1" (get-in grid [row col])) (let [new-grid (assoc-in grid [row col] "0")] 
                                    (-> (flood-island-with-water new-grid (dec row) col)
                                       (flood-island-with-water row (dec col))
                                       (flood-island-with-water (inc row) col)
                                       (flood-island-with-water row (inc col))))
    :else grid))

  (defn number-of-islands
    ([grid] (last (flatten (number-of-islands grid 0))))
    ([grid result]
     (if (every? #(= "0" %) (flatten grid))
       result
       (for [row (range (count grid))
             col (range (count (grid 0)))
             :when (not (= "0" (get-in grid [row col])))]
         (number-of-islands (flood-island-with-water grid row col) (inc result))))))

#_ (number-of-islands [["1","1","0","0","0"]
                    ["1","1","0","0","0"]
                    ["0","0","1","0","0"]
                    ["0","0","0","1","1"]])

;;least common ancestor
(defn lca
  [tree v1 v2]
   (letfn [(find-node [t v] 
                      (cond  
                        (= v (:v t) ) true
                        (nil? t) false
                        :else (or (find-node (:l t)  v) (find-node (:r t) v))))]
     (let [v1-on-left (find-node (:l tree) v1)
          v2-on-left (find-node (:l tree) v2)]
               (cond 
                 (not= v1-on-left v2-on-left) (:v tree)
                 v1-on-left (lca (:l tree) v1 v2)
                 :else (lca (:r tree) v1 v2)))))

#_ (lca (create-tree [3 5 6 nil nil 2 7 nil nil 4 nil nil 1 0 nil nil 8 nil nil]) 5 1)

;;binary tree paths
(defn binary-tree-paths
  ([t] (distinct (binary-tree-paths t "" [])))
  ([t s r]
   (cond 
     (nil? t) [] 
     (and (nil? (:r t)) (nil? (:l t)) ) (conj r (subs s 2))
     :else 
     (->> (binary-tree-paths (:l t) (str s "->" (:v t)) r)
          (binary-tree-paths (:r t) (str s "->" (:v t)))))))

#_ (binary-tree-paths (create-tree [1 2 nil 5 nil nil 3 nil nil]))
;;alien dictionary
(defn build-dependency-words
  [w1 w2]
  (cond 
    (or (empty? w1) (empty? w2) (= w1 w2)) []
    (not= (first w1) (first w2)) [(first w1) (first w2)]
    :else (recur (rest w1) (rest w2))))

(defn build-graph
  [dependencies]
  (reduce
   (fn [r [c1 c2]] (update r c1 (fnil conj []) c2))
   {}
   (remove empty? dependencies)))

(defn initialize-in-degree-map
  [words g]
  (reduce
   (fn [r [_ dependants]] (reduce #(update % %2 inc) r dependants))
   (reduce #(assoc % %2 0) {} (mapcat identity words))
   g))

(defn update-in-degree-map
  [g dependee in-degrees]
  (reduce #(update % %2 dec) in-degrees (g dependee)))

(defn find-free-nodes 
  [in-degrees seen]
  (->> (filter #(zero? (second %2)) in-degrees)
       (map first)
       (remove #(seen %))))

(defn topological-sort
  [g in-degree-map]
  (println in-degree-map)
  (loop [seen (into #{} (find-free-nodes in-degree-map #{}))
         q (into (clojure.lang.PersistentQueue/EMPTY) seen)
         i in-degree-map
         r []]
    (if (empty? q)
      r
      (let [new-i (update-in-degree-map g (peek q) i)
            f (find-free-nodes new-i seen)]
        (recur (into seen f) (into (pop q) f) new-i (conj r (peek q)))))))

(defn alien-order
  [words]
  (let [g (build-graph (map #(apply build-dependency-words %)  (partition 2 1 words)))
        i (initialize-in-degree-map words g)]
    (apply str (topological-sort g i))))
#_ (apply build-dependency-words ["wrt" "wrf"])
#_  (find-free-nodes {\w 0, \r 1, \t 1, \f 1, \e 1} #{})
                    
                    
                    #_(build-graph (map #(apply build-dependency-words %)  (partition 2 1 [
                                                                                         "wrt",
                                                                                         "wrf",
                                                                                         "er",
                                                                                         "ett",
                                                                                         "rftt"
                                                                                         ])))
#_  (alien-order [ "wrt", "wrf", "er", "ett", "rftt" ]) 

;;Diameter of the tree
(defn tree-diameter
  [root]
  (if (nil? root)
    0
    (let [l (tree-diameter (:l root))
          r (tree-diameter (:r root))]
      (max (+ l r) (inc (max l r))))))

#_(tree-diameter (create-tree [1 2 4 nil nil 5 nil nil 3 nil nil]))

;;regex-pattern-match
(defn is-match
  [text pattern]
  (if (empty? pattern)
    (empty? text)
    (let [first-match (and (not (empty? text)) (or (= (first pattern) (first text)) (= \. (first pattern))))]
      (if (and (>= (count pattern) 2) (= (second pattern) \*))
        (or (is-match text (subs pattern 2))
            (and first-match (is-match (subs text 1) pattern)))
        (and first-match (is-match (subs text 1) (subs pattern 1)))))))

#_ (is-match "aab" "c*a*b*")

                    