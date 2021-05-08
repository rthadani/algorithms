(ns algorithms.debug)
#_(println "test")
(defmacro verbose->>
  [x & forms]
  (loop [x x
         forms forms
         intermediate []]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~@(next form) ~x) (meta form))
                       (list form x))]
        (recur
         threaded
         (next forms)
         (conj intermediate `(do
                               (println "x: ")
                               (pp/pprint ~x)
                               (println "\nthreaded: ")
                               (pp/pprint '~threaded)
                               (println "\n=> ")
                               (pp/pprint ~threaded)
                               (println "\n-------")))))
      (list 'do
            `(doseq [i# ~intermediate]
               i#)
            x))))


(defmacro verbose->
  {:added "1.0"}
  [x & forms]
  (loop [x x, forms forms, intermediate []]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded
               (next forms)
               (conj intermediate `(do
                                     (println "x: ")
                                     (pp/pprint ~x)
                                     (println "\nthreaded: ")
                                     (pp/pprint '~threaded)
                                     (println "\n=> ")
                                     (pp/pprint ~threaded)
                                     (println "\n-------")))))
      (list 'do
            `(doseq [i# ~intermediate]
               i#)
            x))))