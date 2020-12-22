(ns aoc (:gen-class))

(defn add-card [first? p1 r1 c1 p2 r2 c2]
  (let [r1_ (if first? (conj r1 c1 c2) r1)
        r2_ (if first? r2 (conj r2 c2 c1))
        [p1_ r1_] (if (empty? p1) (list (reverse r1_) ()) (list p1 r1_))
        [p2_ r2_] (if (empty? p2) (list (reverse r2_) ()) (list p2 r2_))]
    (list p1_ r1_ p2_ r2_)
  ))

(defn score [[p1 r1 p2 r2]]
  (let [p1 (concat p1 (reverse r1))
        p2 (concat p2 (reverse r2))
        p (if (empty? p1) p2 p1)]
    (apply + (map * (rest (range)) (reverse p)))
  ))

(defn round1 [[p1 r1 p2 r2]]
  (let [[c1 & p1] p1
        [c2 & p2] p2]
    ;(println p1_ r1_ p2_ r2_)
    (if (< c1 c2)
        (add-card false p1 r1 c1 p2 r2 c2)
        (add-card true p1 r1 c1 p2 r2 c2))
    ))

(defn solve1 [p1 p2]
  (let [game (iterate round1 (list p1 () p2 ()))
        end (first (filter (fn [[p1 _ p2 _]] (or (empty? p1) (empty? p2))) game))]
    ;(println end)
    (score end)
  ))


(declare play2)

(defn round2 [[p1 r1 p2 r2]]
  (if (or (empty? p1)
    (empty? p2)) (list p1 r1 p2 r2)
    (let [[c1 & p1] p1
          [c2 & p2] p2]
      ;(println c1 p1 r1 c2 p2 r2)
      (if (and
            (<= c1 (+ (count p1) (count r1)))
            (<= c2 (+ (count p2) (count r2))))
        ; play subgame
        (let [s1 (if (<= c1 (count p1))
                     (take c1 p1)
                     (concat p1 (take (- c1 (count p1)) (reverse r1))))
              s2 (if (<= c2 (count p2))
                     (take c2 p2)
                     (concat p2 (take (- c2 (count p2)) (reverse r2))))
              [s1_ s2_] (play2 s1 s2)]
          ;(println "subgame" s1 s2 "->" s1_ s2_)
          (if (empty? s1_)
            (add-card false p1 r1 c1 p2 r2 c2)
            (add-card true p1 r1 c1 p2 r2 c2))
          )
        (if (< c1 c2)
            (add-card false p1 r1 c1 p2 r2 c2)
            (add-card true p1 r1 c1 p2 r2 c2))
        ))
  ))

(defn first-dup [l prev]
  (if (contains? prev (first l))
    (first l)
    (first-dup (rest l) (conj prev (first l)))
  ))

(defn play2 [p1 p2]
  (let [game (iterate round2 (list p1 () p2 ()))
        end (first-dup game #{})
        [p1_ r1_ p2_ r2_] end
        p1_ (concat p1_ (reverse r1_))
        p2_ (concat p2_ (reverse r2_))]
    ;(println end)
    (list p1_ p2_)
  ))

(defn solve2 [p1 p2]
  (let [[p1_ p2_] (play2 p1 p2)]
    (score (list p1_ () p2_ ()))
  ))


(defn parse [deck]
  (map #(Integer/parseInt %) (rest (clojure.string/split deck #"\n"))))

(defn -main
  []
  (let [[p1 p2] (map parse (clojure.string/split (slurp "22.input") #"\n\n"))]
      ;(println p1 p2)
      (println (solve1 p1 p2))
      (println (solve2 p1 p2))
      ))
