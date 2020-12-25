(ns aoc (:gen-class))

(defn next-key [sn k]
  (mod (* sn k) 20201227))

(defn find-loop [sn k]
  (let [ks (iterate (fn [[i k]] [(+ 1 i) (next-key sn k)]) [0 1])]
    (first (first (filter #(= k (second %)) (take 20201227 ks))))
    ))

(defn solve1 [k1 k2]
  (let [sn 7
        l1 (find-loop sn k1)
        k (nth (iterate #(next-key k2 %) 1) l1)]
    k
  ))


(defn -main
  []
  (let [[k1 k2] (map #(Integer/parseInt %) (clojure.string/split (slurp "25.input") #"\n"))]
    ;(println k1 k2)
    (println (solve1 k1 k2))
    ; no part 2
  ))
