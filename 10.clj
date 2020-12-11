(ns aoc (:gen-class))

(defn solve1 [nums]
  (let [[n1 n3 _] (reduce
          (fn [[n1 n3 p] n]
            (let [d (- n p)]
              (case d
                1 [(+ 1 n1) n3 n]
                3 [n1 (+ 1 n3) n]
              ))
          )
          [0 1 0]
          nums)
        ]
    (* n1 n3)))

(defn solve2 [nums]
  (let [ns1 (vec (repeat (+ 4 (first nums)) 0))
        ns2 (assoc ns1 (+ 3 (first nums)) 1)
        ns3 (reduce (fn [ns n]
              (let [c (+ (get ns (+ 1 n)) (get ns (+ 2 n)) (get ns (+ 3 n)))]
                (assoc ns n c))
              )
            ns2 nums)]
    (get ns3 0)))

(defn -main
  []
  (let [nums (sort (map #(Integer/parseInt %) (clojure.string/split (slurp "10.input") #"\n")))]
    ;(println nums)
    (println (solve1 nums))
    (println (solve2 (reverse (conj nums 0))))
  ))
