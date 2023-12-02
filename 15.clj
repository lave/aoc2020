(ns aoc (:gen-class))

(defn add-num [[step nums _] n]
  (let [step_ (+ 1 step)
        nums_ (get nums n)]
    ;(println step nums n)
    (if (nil? nums_)
      [step_ (assoc nums n (list step)) n]
      [step_ (assoc nums n (conj nums_ step)) n])
  ))

(defn make-step [[step nums lastn]]
  (let [step_ (+ 1 step)
        lastnums_ (get nums lastn)
        n (if (= 1 (count lastnums_)) 0 (- (first lastnums_) (nth lastnums_ 1)))
        nums_ (get nums n)
        nums__ (if (nil? nums_) (list step) (conj nums_ step))]
    ;(println step nums lastn n)
    [step_ (assoc nums n nums__) n]
  ))

(defn solve1 [nums n]
  (let [s0 (reduce (fn [nums n] (add-num nums n)) [1 {} nil] nums)
        ss (iterate make-step s0)]
    (nth (nth ss (- n (count nums))) 2)
    ))



(defn -main
  []
  (let [nums (map #(Integer/parseInt (clojure.string/trim-newline %)) (clojure.string/split (slurp "15.input") #","))
        nums_ '(0 3 6)]
    ;(println nums)
    ;(println (solve1 nums 2020))
    ;(println (solve1 nums 30000000))
    (println (solve1 nums 30000000))
  ))
