(ns aoc (:gen-class))

(defn dx [d l]
  (let [d_ (mod d 360)]
    (case d_
      0 l
      180 (- l)
      0)))

(defn dy [d l]
  (let [d_ (mod d 360)]
    (case d_
      270 l
      90 (- l)
      0)))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn solve1 [prg]
  (let [[x y] (reduce (fn [[x y d] [cmd l]]
                        (println cmd l x y d)
                           (case cmd
                             \F [(+ x (dx d l)) (+ y (dy d l)) d]
                             \L [x y (- d l)]
                             \R [x y (+ d l)]
                             \N [x (+ y l) d]
                             \S [x (- y l) d]
                             \E [(+ x l) y d]
                             \W [(- x l) y d]
                             nil
                             ))
                      [0, 0, 0]
                      prg)]
    (+ (abs x) (abs y))))

(defn rot-x [dx dy n]
  (case (mod n 360)
    0 dx
    90 (- dy)
    180 (- dx)
    270 dy
    ))

(defn rot-y [dx dy n]
  (case (mod n 360)
    0 dy
    90 dx
    180 (- dy)
    270 (- dx)
    ))

    

(defn solve2 [prg]
  (let [[x y] (reduce (fn [[x y dx dy] [cmd l]]
                        (println cmd l x y dx dy)
                           (case cmd
                             \F [(+ x (* l dx)) (+ y (* l dy)) dx dy]
                             \L [x y (rot-x dx dy l) (rot-y dx dy l)]
                             \R [x y (rot-x dx dy (- 360 l)) (rot-y dx dy (- 360 l))]
                             \N [x y dx (+ dy l)]
                             \S [x y dx (- dy l)]
                             \E [x y (+ dx l) dy]
                             \W [x y (- dx l) dy]
                             nil
                             ))
                      [0, 0, 10, 1]
                      prg)]
    (+ (abs x) (abs y))))


(defn parseCmd [r]
  (list (first r) (Integer/parseInt (subs r 1))))

(defn -main
  []
  (let [prg (map parseCmd (clojure.string/split (slurp "12.input") #"\n"))]
    ;(println prg)
    (println (solve1 prg))
    (println (solve2 prg))
  ))
