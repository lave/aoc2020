(ns aoc (:gen-class))

(defn next-cell-1 [[y x] map_]
  (let [v00 (get (get map_ (- y 1)) (- x 1))
        v01 (get (get map_ (- y 1)) x)
        v02 (get (get map_ (- y 1)) (+ x 1))
        v10 (get (get map_ y) (- x 1))
        v11 (get (get map_ y) x)
        v12 (get (get map_ y) (+ x 1))
        v20 (get (get map_ (+ y 1)) (- x 1))
        v21 (get (get map_ (+ y 1)) x)
        v22 (get (get map_ (+ y 1)) (+ x 1))
        sum (apply + (filter #(not (nil? %)) [v00 v01 v02 v10 v12 v20 v21 v22]))]
    ;(println y x v11 sum)
    (case v11
      0 (if (= 0 sum) 1 0)
      1 (if (< 3 sum) 0 1)
      nil)))

(defn is-visible [y x dy dx map_ w h]
  (let [y_ (+ y dy)
        x_ (+ x dx)]
  (if (or (< y_ 0) (>= y_ h) (< x_ 0) (>= x_ w))
    0
    (let [v (get (get map_ y_) x_)]
      (if (nil? v)
        (is-visible y_ x_ dy dx map_ w h)
        v)))))

(defn next-cell-2 [[y x] map_]
  (let [h (count map_)
        w (count (get map_ 1))
        v00 (is-visible y x -1 -1 map_ w h)
        v01 (is-visible y x -1  0 map_ w h)
        v02 (is-visible y x -1  1 map_ w h)
        v10 (is-visible y x  0 -1 map_ w h)
        v11 (get (get map_ y) x)
        v12 (is-visible y x  0  1 map_ w h)
        v20 (is-visible y x  1 -1 map_ w h)
        v21 (is-visible y x  1  0 map_ w h)
        v22 (is-visible y x  1  1 map_ w h)
        sum (apply + [v00 v01 v02 v10 v12 v20 v21 v22])]
    ;(println y x v11 sum)
    (case v11
      0 (if (= 0 sum) 1 0)
      1 (if (< 4 sum) 0 1)
      nil)))

(defn printMap [map_]
  (doseq [item (map (fn [r] (mapcat #(case % 0 "L" 1 "#" ".") r)) map_)] (println item)))

(defn next-map [next-cell map_]
  (let [h (count map_)
        w (count (get map_ 1))
        idxs (map (fn [y] (map #(list y %) (range 0 w))) (range 0 h))]
    ;(printMap map_)
    ;(println "--------------")
    (mapv (fn [row] (mapv (fn [idx] (next-cell idx map_)) row)) idxs)))

(defn solve1 [map_ next-cell]
  (let [maps (iterate #(next-map next-cell %) map_)
        pairs (partition 2 1 maps)
        d (first (first (filter (fn [[x y]] (= x y)) pairs)))]
    (apply + (filter #(not (nil? %)) (apply concat d)))))


(defn parseRow [r]
  (mapv #(case % "L" 0 "#" 1 nil) (clojure.string/split r #"")))

(defn -main
  []
  (let [map_ (mapv #(parseRow %) (clojure.string/split (slurp "11.input") #"\n"))]
    ;(println map_)
    (println (solve1 map_ next-cell-1))
    (println (solve1 map_ next-cell-2))
  ))
