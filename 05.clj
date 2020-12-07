(ns aoc (:gen-class))

(defn solve1 [ids]
  (apply max ids))

(defn solve2 [ids]
  (first (first (reduce
    (fn [[found prev] curr]
      (list (if (= 2 (- curr prev)) (conj found (- curr 1)) found) curr))
    '(() 0)
    (sort ids)))))

(defn parse [s]
  (reduce
    (fn [n d]
      (if (or (= \B d) (= \R d)) (+ 1 (* 2 n)) (* 2 n))
    )
    0 s))

(defn -main
  []
  (let [ids (map #(parse %) (clojure.string/split (slurp "05.input") #"\n"))]
    ;(println ids)
    (println (solve1 ids))
    (println (solve2 ids))
  ))

