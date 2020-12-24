(ns aoc (:gen-class))

(defn walk [path]
  (reduce (fn [[x y] move]
            (case move
              "w" [(- x 1) y]
              "e" [(+ x 1) y]
              "nw" [(- x 1) (- y 1)]
              "ne" [x (- y 1)]
              "sw" [x (+ y 1)]
              "se" [(+ x 1) (+ y 1)]))
          [0 0]
          path))

(defn solve1 [coords]
  (let [tiles (reduce (fn [tiles c]
                  (if (contains? tiles c)
                    (assoc tiles c (- 1 (get tiles c)))
                    (assoc tiles c 1)))
                (hash-map)
                coords)]
    (apply + (vals tiles))
  ))

(defn idx [[x-min y-min w] [x y]]
  (+ (* w (+ y y-min)) (+ x x-min)))

(defn print-floor [floor w h]
  (let [lines (map-indexed (fn [i row] (clojure.string/join
                               (conj (map #(case % 0 ". " 1 "# ") row)
                                     (apply str (repeat (- h i) \space)))))
                           (partition w floor))]
    (doseq [item lines]
     (println item))))

(defn evolve [[floor dim]]
  (let [[x-min y-min w h] dim
        floor_ (mapv (fn [i]
                      (let [y (quot i w)
                            x (mod i w)
                            cells (list (- i w 1)
                                        (- i w)
                                        (- i 1)
                                        (+ i 1)
                                        (+ i w)
                                        (+ i w 1))
                            sum (apply + (filter #(not (nil? %)) (map #(get floor %) cells)))
                            c (get floor i)]
                        (if (= 0 c)
                          (if (= 2 sum) 1 0)
                          (if (or (= 0 sum) (< 2 sum)) 0 1))))
                    (range 0 (* w h)))]
    ;(print-floor floor w h)
    (list floor_ dim)))

(defn solve2 [coords]
  (let [delta 50
        x-min (- (apply min (map first coords)) delta)
        x-max (+ (apply max (map first coords)) delta)
        y-min (- (apply min (map second coords)) delta)
        y-max (+ (apply max (map second coords)) delta)
        w (- x-max x-min)
        h (- y-max y-min)
        floor-dim [(- x-min) (- y-min) w h]
        floor (vec (replicate (* w h) 0))
        floor0 (reduce
                 (fn [floor coord]
                   (let [i (idx floor-dim coord)]
                     (assoc floor i (- 1 (get floor i)))
                   ))
                 floor
                 coords)
        floors (iterate evolve (list floor0 floor-dim))
        floor_ (nth floors 100)
        ]
  (apply + (first floor_))
  ))


(defn parse [s path]
  (if (empty? s)
    (reverse path)
    (if (or (clojure.string/starts-with? s "e") (clojure.string/starts-with? s "w"))
      (parse (subs s 1) (conj path (subs s 0 1)))
      (parse (subs s 2) (conj path (subs s 0 2)))
    )))

(defn -main
  []
  (let [paths (map #(parse % '()) (clojure.string/split (slurp "24.input") #"\n"))
        coords (map walk paths)]
      ;(println paths)
      ;(println coords)
      (println (solve1 coords))
      (println (solve2 coords))
      ))
