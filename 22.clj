(ns aoc (:gen-class))

(defn round1 [[p1 r1 p2 r2]]
  (let [c1 (first p1)
        c2 (first p2)
        p1_ (rest p1)
        p2_ (rest p2)
        r1_ (if (< c1 c2) r1 (conj r1 c1 c2))
        r2_ (if (< c1 c2) (conj r2 c2 c1) r2)
        [p1_ r1_] (if (empty? p1_) (list (reverse r1_) ()) (list p1_ r1_))
        [p2_ r2_] (if (empty? p2_) (list (reverse r2_) ()) (list p2_ r2_))]
    ;(println p1_ r1_ p2_ r2_)
    (list p1_ r1_ p2_ r2_)
  ))

(defn solve1 [p1 p2]
  (let [game (iterate round1 (list p1 () p2 ()))
        end (first (filter (fn [[p1 _ p2 _]] (or (empty? p1) (empty? p2))) game))
        [p1_ r1_ p2_ r2_] end
        p1_ (concat p1_ (reverse r1_))
        p2_ (concat p2_ (reverse r2_))
        p (if (empty? p1_) p2_ p1_)
        ]
    ;(println end)
    (apply + (map * (rest (range)) (reverse p)))
  ))

(defn add-card [first? p1 r1 c1 p2 r2 c2]
  (let [r1_ (if first? (conj r1 c1 c2) r1)
        r2_ (if first? r2 (conj r2 c2 c1))
        [p1_ r1_] (if (empty? p1_) (list (reverse r1_) ()) (list p1_ r1_))
        [p2_ r2_] (if (empty? p2_) (list (reverse r2_) ()) (list p2_ r2_))]
    (list p1_ r1_ p2_ r2_)
  ))

(defn round2 [[p1 r1 p2 r2]]
  (let [c1 (first p1)
        c2 (first p2)
        p1 (rest p1)
        p2 (rest p2)]
    (println c1 p1 r1 c2 p2 r2)
    (if (and
          (< c1 (+ (count p1) (count r1)))
          (< c2 (+ (count p2) (count r2))))
      ; play subgame
      (let [p1__ (if (> c1 (count p1))
                   (take c1 p1)
                   (concat p1 (take (- c1 (count p1)) (reverse r1))))
            p2__ (if (> c2 (count p2))
                   (take c2 p2)
                   (concat p2 (take (- c2 (count p2)) (reverse r2))))
            [p1_3 p2_3] (play2 p1__ p2__)]
        (if (empty? p1_3)
          (add-card true p1 r1 c1 p2 r2 c2)
          (add-card false p1 r1 c1 p2 r2 c2))
        )
      (if (> c1 c2)
          (add-card true p1 r1 c1 p2 r2 c2)
          (add-card false p1 r1 c1 p2 r2 c2))
      )
  ))

(defn play2 [p1 p2]
  (let [game (iterate round2 (list p1 () p2 ()))
        end (first (filter (fn [[p1 _ p2 _]] (or (empty? p1) (empty? p2))) game))
        [p1_ r1_ p2_ r2_] end
        p1_ (concat p1_ (reverse r1_))
        p2_ (concat p2_ (reverse r2_))]
    ;(println end)
    (list p1_ p2_)
  ))

(defn solve2 [p1 p2]
  (let [[p1_ p2_] (game p1 p2)
        p (if (empty? p1_) p2_ p1_)]
    (apply + (map * (rest (range)) (reverse p)))
  ))


(defn parse [deck]
  (map #(Integer/parseInt %) (rest (clojure.string/split deck #"\n"))))

(defn -main
  []
  (let [[p1 p2] (map parse (clojure.string/split (slurp "22.input") #"\n\n"))]
      ;(println p1 p2)
      ;(println (solve1 p1 p2))
      (println (solve2 p1 p2))
      ))
