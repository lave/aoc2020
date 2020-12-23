(ns aoc (:gen-class))

(defn index-of [e coll]
  (first (keep-indexed #(if (= e %2) %1) coll)))

(defn round1 [nums]
  (let [[n & nums__] nums
        skip (take 3 nums__)
        nums_ (drop 3 nums__)
        n1 (if (< n 2) (+ n 8) (- n 1))
        n2 (if (< n 3) (+ n 7) (- n 2))
        n3 (if (< n 4) (+ n 6) (- n 3))
        n4 (if (< n 5) (+ n 5) (- n 4))
        dest (first (filter #(not (nil? %)) (map #(index-of % nums_) [n1 n2 n3 n4])))
        d (nth nums_ dest)
        da (drop (+ dest 1) nums_)
        db (take dest nums_)]
    ;(println nums)
    ;(println n skip nums_ dest)
    ;(println db (list d) skip da (list n))
    (concat db (list d) skip da (list n))
  ))


(defn solve1 [nums]
  (let [game (iterate round1 nums)
        end (nth game 100)
        dest (index-of 1 end)
        da (drop (+ dest 1) end)
        db (take dest end)]
    ;(println end)
    ;(println da db)
    (reduce #(+ (* %1 10) %2) (concat da db))
  ))


(defn -main
  []
  (let [nums (map #(Integer/parseInt %) (clojure.string/split (clojure.string/trim-newline (slurp "23.input")) #""))]
      ;(println nums)
      (println (solve1 nums))
      ))
