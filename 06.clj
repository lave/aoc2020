(ns aoc (:gen-class)
  (:require clojure.set))

(defn solve1 [l]
  (apply + (map #(count (apply clojure.set/union %)) l)))

(defn solve2 [l]
  (apply + (map #(count (apply clojure.set/intersection %)) l)))

(defn -main
  []
  (let [l (map (fn [l_] (map #(into (hash-set) %) (clojure.string/split l_ #"\n"))) (clojure.string/split (slurp "06.input") #"\n\n"))]
    ;(println l)
    (println (solve1 l))
    (println (solve2 l))
  ))
