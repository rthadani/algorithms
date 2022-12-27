(ns interview.exchange-rate
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

(def exchange-rates 
{:ETH {:USD 180 :EUR 210}
 :USD {:ETH (/ 1 200) :BTC (/ 1 1000)}
 :EUR {:ETH (/ 1 220) :BTC (/ 1 1200)}
 :BTC {:USD 990 :EUR 1150}
 })

(defn all-paths
  ([g src dst]
   (all-paths g src dst #{src}))
  ([g src dst visited]
   (if (= dst src)
     [[dst]] ;;when src and destination are the same then you only have a destination  in the path
     ;;otherwise get all the neighbors for source and get their paths to destination
     ;;once you have that append the source in front of each of the paths
     (for [neighbor (set/difference (into #{} (keys (g src))) visited)
           path (all-paths g neighbor dst (conj visited neighbor))]
       (cons src path)))))

  (defn calc-exchange-rate
    [path]
    ;;take each path component and find its exchange rate
    (->> (partition 2 1 path)
         (reduce (fn [result [from to]] (* result (get-in exchange-rates [from to]))) 1)))

(->> (all-paths exchange-rates :USD :EUR) ;;get all paths
         (map (fn [path] [path (calc-exchange-rate path)])) ;;get the exchange rate for each one
         (apply max-key second)) ;;pick the trade that gives you the maximum value

