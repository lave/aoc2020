(ns aoc (:gen-class))

(defn is-valid1 [p]
  (every? #(not (nil? (get p %))) '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))

(defn solve1 [l]
  (count (filter #(is-valid1 %) l)))

(defn is-num [l h s]
  (let [n (Integer/parseInt s)]
    (and (>= n l) (<= n h))))

(defn is-num [l h s]
  (if (re-matches #"[0-9]+" s)
    (let [n (Integer/parseInt s)]
      (and (>= n l) (<= n h)))
    false))

(defn is-num-suffix [l h sfx s]
  (let [sl (count s)
        sfx-pos (- sl (count sfx))]
    (and (= sfx (subs s sfx-pos)) (is-num l h (subs s 0 sfx-pos)))))

(defn is-valid2 [p]
  (every? (fn [[k pred]]
    (let [v (get p k)]
      (and (not (nil? v)) (pred v))))
    (list
      ["byr" #(is-num 1920 2002 %)]
      ["iyr" #(is-num 2010 2020 %)]
      ["eyr" #(is-num 2020 2030 %)]
      ["hgt" #(or (is-num-suffix 150 193 "cm" %) (is-num-suffix 59 76 "in" %))]
      ["hcl" #(re-matches #"#[0-9a-f]{6}" %)]
      ["ecl" #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %)]
      ["pid" #(re-matches #"[0-9]{9}" %)]
      )))

(defn solve2 [l]
  (count (filter #(is-valid2 %) l)))

(defn parse [line]
  (let [pairs (clojure.string/split (clojure.string/replace line #"\n" " ") #" ")]
    (into (hash-map) (map #(clojure.string/split % #":") pairs))))

(defn -main
  []
  (let [l (map #(parse %) (clojure.string/split (slurp "04.input") #"\n\n"))]
    ;(println l)
    (println (solve1 l))
    (println (solve2 l))
  ))

