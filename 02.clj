(ns aoc (:gen-class))

(defn is-valid1 [[l h c s]]
  (let [n (count (filter #(= c %) s))]
    (and (>= n l) (<= n h))))

(defn solve1 [l]
  (count (filter #(is-valid1 %) l)))

(defn is-valid2 [[l h c s]]
  (let [
    n (count (filter #(= c %) s))
    cl (= c (get s (- l 1)))
    ch (= c (get s (- h 1)))]
    (= 1 (+ (if cl 1 0) (if ch 1 0)))))

(defn solve2 [l]
  (count (filter #(is-valid2 %) l)))

(defn nums [l]
  (map #(Integer/parseInt %) (clojure.string/split l #"\t")))

(defn parse [s]
  (let [[_ l h c p] (re-matches #"(\d+)-(\d+) (\w): (\w+)" s)]
    (list (Integer/parseInt l) (Integer/parseInt h) (first c) p)))

(defn -main
  []
  (let [l (map #(parse %) (clojure.string/split (slurp "02.input") #"\n"))]
    ;(println l)
    (println (solve1 l))
    (println (solve2 l))
  ))

