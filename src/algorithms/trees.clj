(ns algorithms.trees)

(defn conj-bst
  [val tree]
  (cond
    (= nil tree)      {:v val :l nil :r nil}
    (< val (:v tree)) {:v (:v tree) :l (conj-bst val (:l tree)) :r (:r tree)}
    :else             {:v (:v tree) :l (:l tree) :r (conj-bst val (:r tree))}))

(defn inorder-seq
  [tree]
  (when tree
    (concat (inorder-seq (:l tree))
            [(:v tree)] (inorder-seq (:r tree)))))

