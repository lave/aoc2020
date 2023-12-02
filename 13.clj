(ns aoc (:gen-class))

(defn solve1 [[time buses]]
  (let [times (map (fn [bus]
         (let [n (quot (+ time bus -1) bus)]
           (* n bus)
           ))
         buses)]
    (println times)
    times))

(defn solve2 [[_ buses]]
  (first (reduce (fn [[n m] [d c]]
    (let [x (first (filter #(= (mod (- c d) c) (mod (+ n (* m %)) c)) (range 0 c)))]
      ;(println n m d c x)
      [(+ n (* m x)) (* m c)]
      ))
    [0 1]
    buses)))


(defn parse1 [[time, buses]]
  (let [time_ (Integer/parseInt time)
        buses_ (map #(Integer/parseInt %) (filter #(not (= "x" %)) (clojure.string/split buses #",")))]
    (list time_ buses_)))

(defn parse [[time, buses]]
  (let [time_ (Integer/parseInt time)
        buses_ (map list (range 0 100) (map #(if (= % "x") nil (Integer/parseInt %)) (clojure.string/split buses #",")))
        buses__ (filter #(not (nil? (nth % 1))) buses_)]
    (list time_ buses__)))

(defn -main
  []
  (let [params1 (parse1 (clojure.string/split (slurp "13.input") #"\n"))
        params (parse (clojure.string/split (slurp "13.input") #"\n"))]
    (println params1)
    (println (solve1 params1))
    (println params)
    (let [n (solve2 params)]
      (println (solve2 params))
      (println (map #(mod n (nth % 1)) (nth params 1))))
  ))
