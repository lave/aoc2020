(ns aoc (:gen-class))

(def deltas3d (list
               [-1 -1 -1] [-1 -1 0] [-1 -1 1]
               [-1 0 -1]  [-1 0 0]  [-1 0 1]
               [-1 1 -1]  [-1 1 0]  [-1 1 1]
               [0 -1 -1]  [0 -1 0]  [0 -1 1]
               [0 0 -1]             [0 0 1]
               [0 1 -1]   [0 1 0]   [0 1 1]
               [1 -1 -1]  [1 -1 0]  [1 -1 1]
               [1 0 -1]   [1 0 0]   [1 0 1]
               [1 1 -1]   [1 1 0]   [1 1 1]))

(defn step3d [[[d h w] layers]]
  (let [d_ (+ 2 d)
        h_ (+ 2 h)
        w_ (+ 2 w)
        layers_ (mapv
          (fn [z]
            (mapv
              (fn [y]
                (mapv
                  (fn [x]
                    (let [sum (apply + (filter #(not (nil? %)) (map (fn [[dx dy dz]]
                                     (get (get (get layers (- (+ z dz) 1)) (- (+ y dy) 1)) (- (+ x dx) 1)))
                                   deltas3d)))
                          state (get (get (get layers (- z 1)) (- y 1)) (- x 1))]
                      ;(println z y x sum state)
                      (if (= 1 state)
                        (if (or (= 2 sum) (= 3 sum)) 1 0)
                        (if (= 3 sum) 1 0))
                    ))
                  (range 0 w_)
                  ))
              (range 0 h_)
              ))
          (range 0 d_)
          )]
    ;(println (list d_ h_ w_) layers_)
    (list (list d_ h_ w_) layers_)))

(defn solve1 [map_ n]
  (let [maps (iterate step3d map_)
        [_ map__] (nth maps n)
        nums (apply concat (map #(apply concat %) map__))]
    ;(println map__)
    (apply + nums)))


(def deltas4d (list
               [-1 -1 -1 -1] [-1 -1 -1 0] [-1 -1 -1 1]
               [-1 -1 0 -1]  [-1 -1 0 0]  [-1 -1 0 1]
               [-1 -1 1 -1]  [-1 -1 1 0]  [-1 -1 1 1]
               [-1 0 -1 -1]  [-1 0 -1 0]  [-1 0 -1 1]
               [-1 0 0 -1]   [-1 0 0 0]   [-1 0 0 1]
               [-1 0 1 -1]   [-1 0 1 0]   [-1 0 1 1]
               [-1 1 -1 -1]  [-1 1 -1 0]  [-1 1 -1 1]
               [-1 1 0 -1]   [-1 1 0 0]   [-1 1 0 1]
               [-1 1 1 -1]   [-1 1 1 0]   [-1 1 1 1]
               [0 -1 -1 -1] [0 -1 -1 0] [0 -1 -1 1]
               [0 -1 0 -1]  [0 -1 0 0]  [0 -1 0 1]
               [0 -1 1 -1]  [0 -1 1 0]  [0 -1 1 1]
               [0 0 -1 -1]  [0 0 -1 0]  [0 0 -1 1]
               [0 0 0 -1]               [0 0 0 1]
               [0 0 1 -1]   [0 0 1 0]   [0 0 1 1]
               [0 1 -1 -1]  [0 1 -1 0]  [0 1 -1 1]
               [0 1 0 -1]   [0 1 0 0]   [0 1 0 1]
               [0 1 1 -1]   [0 1 1 0]   [0 1 1 1]
               [1 -1 -1 -1] [1 -1 -1 0] [1 -1 -1 1]
               [1 -1 0 -1]  [1 -1 0 0]  [1 -1 0 1]
               [1 -1 1 -1]  [1 -1 1 0]  [1 -1 1 1]
               [1 0 -1 -1]  [1 0 -1 0]  [1 0 -1 1]
               [1 0 0 -1]   [1 0 0 0]   [1 0 0 1]
               [1 0 1 -1]   [1 0 1 0]   [1 0 1 1]
               [1 1 -1 -1]  [1 1 -1 0]  [1 1 -1 1]
               [1 1 0 -1]   [1 1 0 0]   [1 1 0 1]
               [1 1 1 -1]   [1 1 1 0]   [1 1 1 1]))

(defn step4d [[[f d h w] layers]]
  (let [d_ (+ 2 d)
        h_ (+ 2 h)
        w_ (+ 2 w)
        f_ (+ 2 f)
        layers_ (mapv
          (fn [t]
            (mapv
              (fn [z]
                (mapv
                  (fn [y]
                    (mapv
                      (fn [x]
                        (let [sum (apply + (filter #(not (nil? %)) (map (fn [[dt dx dy dz]]
                                         (get (get (get (get layers (- (+ t dt) 1)) (- (+ z dz) 1)) (- (+ y dy) 1)) (- (+ x dx) 1)))
                                       deltas4d)))
                              state (get (get (get (get layers (- t 1)) (- z 1)) (- y 1)) (- x 1))]
                          ;(println z y x sum state)
                          (if (= 1 state)
                            (if (or (= 2 sum) (= 3 sum)) 1 0)
                            (if (= 3 sum) 1 0))
                        ))
                      (range 0 w_)
                      ))
                  (range 0 h_)
                  ))
              (range 0 d_)
              ))
            (range 0 f_)
          )]
    ;(println (list d_ h_ w_) layers_)
    (list (list f_ d_ h_ w_) layers_)))

(defn solve2 [map_ n]
  (let [maps (iterate step4d map_)
        [_ m4] (nth maps n)
        nums (apply concat (map (fn [m3] (apply concat (map (fn [m2] (apply concat m2)) m3))) m4))]
    ;(println map__)
    (apply + nums)))


(defn parse [line]
  (mapv #(case % "#" 1 0) (clojure.string/split line #"")))

(defn -main
  []
  (let [lines (clojure.string/split (slurp "17.input") #"\n")
        h (count lines)
        w (count (first lines))
        layer0 (mapv parse lines)
        map3d (list (list 1 h w) [layer0])
        map4d (list (list 1 1 h w) [[layer0]])
        ]
    ;(println map3d)
    (println (solve1 map3d 6))
    (println (solve2 map4d 6))
  ))
