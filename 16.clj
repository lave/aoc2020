(ns aoc (:gen-class))

(defn is-in [n [l h]]
  (and (>= n l) (<= n h)))

(defn is-invalid [n ranges]
  (every? (fn [[l h]] (or (< n l) (> n h))) ranges))

(defn solve1 [rules tickets]
  (let [ranges (mapcat #(nth % 1) rules)
        invalid (filter #(is-invalid % ranges) (apply concat tickets))]
    (apply + invalid)))


(defn is-valid [ticket ranges]
  (every? (fn [n] (some #(is-in n %) ranges)) ticket))

(defn is-good-rule [[_ ranges] nums]
  (is-valid nums ranges))

(defn find-mapping [positions mapping]
  ;(println positions mapping)
  (if (empty? positions)
    (list mapping)
    (let [[idx rules] (first positions)
          positions_ (rest positions)
          rules_ (remove mapping rules)
          mappings (mapcat (fn [rule]
                          (find-mapping positions_ (assoc mapping rule idx))) rules_)
          ]
    (list (first mappings)))))

(defn solve2 [rules ticket tickets]
  (let [ranges (mapcat #(nth % 1) rules)
        valid (filter #(is-valid % ranges) tickets)
        ; transpose tickets
        nums (apply map vector valid)
        nums-rules (map (fn [nums_] (map first (filter #(is-good-rule % nums_) rules))) nums)
        nums-rules_ (sort-by #(count (nth % 1)) (map list (range 0 (count rules)) nums-rules))
        mapping (first (find-mapping nums-rules_ (hash-map)))
        int-rules (filter #(clojure.string/starts-with? % "departure") (keys mapping))
        indexes (map mapping int-rules)
        ]
    ;(println "mapping " mapping)
    ;(println "indexes " indexes)
    (apply * (map #(nth ticket %) indexes))))


(defn parseRange [range_]
  (map #(Integer/parseInt %) (clojure.string/split range_ #"-")))

(defn parseRule [rule]
  (let [[field ranges_] (clojure.string/split rule #": ")
        ranges (map parseRange (clojure.string/split ranges_ #" or "))]
    (list field ranges)))

(defn parseTicket [ticket_]
  (mapv #(Integer/parseInt %) (clojure.string/split ticket_ #",")))


(defn -main
  []
  (let [[rules_ ticket_ tickets_] (clojure.string/split (slurp "16.input") #"\n\n")
        rules (mapv parseRule (clojure.string/split rules_ #"\n"))
        ticket (parseTicket (nth (clojure.string/split ticket_ #"\n") 1))
        tickets (map parseTicket (rest (clojure.string/split tickets_ #"\n")))]
    ;(println rules)
    ;(println ticket)
    ;(println tickets)
    ;(println (solve1 rules tickets))
    (println (solve2 rules ticket tickets))
  ))
