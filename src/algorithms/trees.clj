(ns algorithms.trees
  (:require [clojure.core.match :refer [match]])
  (:import (java.security MessageDigest)))

;;common tree functions
(defn- leaf?
  [node]
  (and (nil? (:l node)) (nil? (:r node))))

(defn- left-only?
  [node]
  (and (nil? (:r node)) (some? (:l node))))

(defn- right-only?
  [node]
  (and (nil? (:l node)) (some? (:r node))))

(defn- left-most-node
  [node]
  (if (nil? (:l node))
    node
    (left-most-node (:l node))))

(defn height
  [tree]
  (cond
    (nil? tree) 0
    :else (inc (max (height (:l tree)) (height (:r tree))))))

(defn inorder-seq
  [tree]
  (when tree
    (concat (inorder-seq (:l tree))
            [(:v tree)] (inorder-seq (:r tree)))))

(defn is-balanced? [tree]
  (<= (Math/abs (- (height (:l tree)) (height (:r tree)))) 1))

;;ordinary bst
(defn- delete-node
  [node]
  (cond
    (leaf? node) nil
    (left-only? node) (:l node)
    (right-only? node) (:r node)
    :else (let [leftmost (left-most-node (:r node))]
            {:v (:v leftmost) :l (:l node) :r (:r leftmost)})))

(defn new-bst-node
  [val]
  {:v val :l nil :r nil})

(defn conj-bst
  [val tree]
  (cond
    (= nil tree) (new-bst-node val)
    (< val (:v tree)) {:v (:v tree) :l (conj-bst val (:l tree)) :r (:r tree)}
    :else {:v (:v tree) :l (:l tree) :r (conj-bst val (:r tree))}))

(defn remove-key
  [val tree]
  (cond
    (= val (:v tree)) (delete-node tree)
    (< val (:v tree)) {:v (:v tree) :l (remove-key val (:l tree)) :r (:r tree)}
    :else {:v (:v tree) :l (:l tree) :r (remove-key val (:r tree))}))


(def bst
  (->> (conj-bst 8 nil)
       (conj-bst 11)
       (conj-bst 5)
       (conj-bst 4)
       (conj-bst 3)
       (conj-bst 12)
       (conj-bst 10)
       (conj-bst 15)))

;;red-black
(defn- new-rb-node
  [val]
  {:v val :c :red :l nil :r nil})

(defn- balance
  [tree]
  (match [tree]
         [(:or
            {:l {:c :red :l {:c :red :l a :v x :r b} :v y :r c} :v z :r d}
            {:l {:c :red :l a :v x :r {:c :red :l b :v y :r c}} :v z :r d}
            {:l a :v x :r {:c :red :l {:c :red :l b :v y :r c} :v z :r d}}
            {:l a :v x :r {:c :red :l b :v y :r {:c :red :l c :v z :r d}}})] {:c :red
                                                                              :l {:c :black :l a :v x :r b}
                                                                              :v y
                                                                              :r {:c :black :v z :r d}}
         :else tree))

(defn- insert-rb-tree
  [val tree]
  (cond
    (= nil tree) (new-rb-node val)
    (< val (:v tree)) (balance {:c (:c tree) :v (:v tree) :l (insert-rb-tree val (:l tree)) :r (:r tree)})
    :else (balance {:c (:c tree) :v (:v tree) :l (:l tree) :r (insert-rb-tree val (:r tree))})))

(defn conj-rb-tree
  [val tree]
  (let [{:keys [l v r]} (insert-rb-tree val tree)]
    {:c :black :l l :v v :r r}))


(defn- delete-rb-node
  [node]
  (cond
    (leaf? node) nil
    (left-only? node) (:l node)
    (right-only? node) (:r node)
    :else (let [leftmost (left-most-node (:r node))]
            {:c (:c leftmost) :v (:v leftmost) :l (:l node) :r (:r leftmost)})))

(defn remove-rb-key
  [val tree]
  (cond
    (= val (:v tree)) (delete-rb-node tree)
    (< val (:v tree)) (balance {:v (:v tree) :l (remove-rb-key val (:l tree)) :r (:r tree)})
    :else (balance {:v (:v tree) :l (:l tree) :r (remove-rb-key val (:r tree))})))

(def rb-tree
  (->> (conj-rb-tree 1 nil)
       (conj-rb-tree 2)
       (conj-rb-tree 3)
       (conj-rb-tree 4)
       (conj-rb-tree 5)
       (conj-rb-tree 6)
       (conj-rb-tree 7)
       (conj-rb-tree 8)))

;Merkle
(defn md5hash
  [val]
  (->>
    (doto
      (MessageDigest/getInstance "MD5")
      (.update (.getBytes (String. val))))
    .digest
    (map #(format "%02x" %))
    (apply str)))

(defn merkle-tree
  [values]
  (->> (partition-all 2)))


