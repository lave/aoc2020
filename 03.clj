(ns aoc (:gen-class))

(defn count-trees [l dx dy]
  (let [[n] (reduce
    (fn [[n y x] r]
      (if (= 0 (mod y dy))
        (list (+ n (get r (mod x (count r)))) (+ y 1) (+ x dx) )
        (list n (+ y 1) x))
    )
    '(0 0 0)
    l)]
    n))

(defn solve1 [l] (count-trees l 3 1))

(defn solve2 [l]
  (* (count-trees l 1 1)
     (count-trees l 3 1)
     (count-trees l 5 1)
     (count-trees l 7 1)
     (count-trees l 1 2)))

(defn parse [r]
    (mapv #(if (= \. %) 0 1) r))

(defn -main
  []
  (let [l (map #(parse %) (clojure.string/split (slurp "03.input") #"\n"))]
    ;(println l)
    (println (solve1 l))
    (println (solve2 l))
  ))

