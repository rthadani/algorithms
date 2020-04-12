(ns algorithms.recommender)


(defn cooccurrence [users]
  (reduce (fn [cooc path]
            (update-in cooc path (fnil inc 0)))
          {}
          (for [ratings users
                pair ratings
                other-pair ratings
                :when (not= pair other-pair)]
            (into [pair] other-pair))))

(defn collab-filter-recommend [{:keys [cooc user-ratings score explore
                         n exploit-ratio cutoff]
                  :or {n 10, exploit-ratio 0.7, cutoff 0.5}}]
  (let [exclude (set (keys user-ratings))
        exploit (->> user-ratings
                     (map cooc)
                     (apply merge-with #(merge-with + %1 %2))
                     (remove (comp exclude first))
                     (map #(update % 1 score))
                     (sort-by second)
                     reverse
                     (take-while #(<= cutoff (second %)))
                     (map first))
        explore (remove exclude explore)]
    (->> exploit
         (take (* exploit-ratio n))
         (#(concat % explore))
         distinct
         (take n)
         shuffle)))


(def alpha 1)
(def beta 1)

(defn score [{:keys [like dislike]
              :or {like 0 dislike 0}}]
  (/ (+ like alpha) (+ like dislike alpha beta)))

;https://www.reddit.com/r/Clojure/comments/fwo145/a_recommender_systtem_in_30_lines_of_clojure/
(let [alice {:a :like
             :b :like
             :c :dislike
             :d :like
             :e :like}
      bob   {:a :like
             :b :dislike
             :c :dislike
             :d :dislike
             :e :like}
      carol {:a :like
             :b :dislike}
      users [alice bob carol]
      explore (distinct (mapcat keys users))
      cooc (cooccurrence users)]
  (println users)
  (println explore)
  (collab-filter-recommend
   {:n       2
    :cooc    cooc
    :user-ratings carol
    :score   (fn [{:keys [like dislike]
                   :or   {like    0
                          dislike 0}}]
               (/ (+ like 1) (+ like dislike 2)))
    :explore explore}))


