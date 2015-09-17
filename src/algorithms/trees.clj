(ns algorithms.trees
  (:require [clojure.core.match :refer [match]]))

(defn conj-bst
  [val tree]
  (cond
    (= nil tree)      {:v val :l nil :r nil}
    (< val (:v tree)) {:v (:v tree) :l (conj-bst val (:l tree)) :r (:r tree) }
    :else             {:v (:v tree) :l (:l tree) :r (conj-bst val (:r tree))}))

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

(defn- delete-node
  [node]
  (cond
    (leaf? node) nil
    (left-only? node) (:l node)
    (right-only? node) (:r node)
    :else (let [leftmost (left-most-node (:r node))]
            ()
            {:v (:v leftmost) :l (:l node) :r (:r leftmost)})))

(defn remove-bst
  [val tree]
  (cond
    (= val (:v tree)) (delete-node tree)
    (< val (:v tree)) {:v (:v tree) :l (remove-bst val (:l tree)) :r (:r tree)}
    :else {:v (:v tree) :l (:l tree) :r (remove-bst val (:r tree))}))

(defn inorder-seq
  [tree]
  (when tree
    (concat (inorder-seq (:l tree))
            [(:v tree)] (inorder-seq (:r tree)))))

(def bst (->> (conj-bst 8 nil)
              (conj-bst 11)
              (conj-bst 5)
              (conj-bst 4)
              (conj-bst 3)
              (conj-bst 12)
              (conj-bst 10)
              (conj-bst 15)))

;;red-black
#_(defn balance-tree
  [tree]
  (match [tree]
         ))

