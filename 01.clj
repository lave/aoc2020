(ns aoc (:gen-class))

(defn sum2 [s nums]
  (let [x (first (filter #(contains? nums (- s %)) nums))]
    (if (nil? x) nil (list x (- s x)))))

(defn solve1 [nums]
  (let [[x y] (sum2 2020 nums)]
    (* x y)))

(defn sum3 [s nums]
  (let [z (first (filter #(not (nil? (sum2 (- s %) nums))) nums))
        [x y] (sum2 (- s z) nums)]
    (list x y z)))

(defn solve2 [nums]
  (let [[x y z] (sum3 2020 nums)]
    (* x y z)))

(defn -main
  []
  (let [nums (set (map #(Integer/parseInt %) (clojure.string/split (slurp "01.input") #"\n")))]
    ;(println nums)
    (println (solve1 nums))
    (println (solve2 nums))
  ))
