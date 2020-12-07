(ns aoc (:gen-class)
  (:require clojure.set))

(defn rule-contains [[_ colors] color]
  (some #(= color %) (map #(nth % 1) colors)))

(defn solve1 [rules color]
  (let [
    seq_ (iterate
      (fn [[front all]]
        ;(println "front = " front ", all = " all)
        (let [front_ (into (hash-set) (mapcat (fn [c] (map first (filter #(rule-contains % c) rules))) front))
              front_ (clojure.set/difference front_ all)
              all_ (clojure.set/union all front_)]
           ;(println "new front = " front_ ", all = " all_)
           (list front_ all_)))
      [#{color} #{}])
    last_ (first (filter #(empty? (first %)) seq_))
    ]
    (count (nth last_ 1))))


(defn count-bags [color rules]
  (let [[_ bags] (first (filter #(= color (first %)) rules))]
    ;(println color bags)
    (apply + (map (fn [[n c]] (+ n (* n (count-bags c rules)))) bags))))

(defn solve2 [rules color]
  (count-bags color rules))


(defn parse [r]
  (let [[_ color content] (re-matches #"(\w+ \w+) bags contain (.*)\." r)]
    (if (= content "no other bags")
      (list color nil)
      (let [bags (clojure.string/split content #", ")
            bags_ (map
              #(let [[_ n c] (re-matches #"(\d+) (\w+ \w+) bags?" %)]
                (list (Integer/parseInt n) c))
              bags)]
        (list color bags_))
    )))

(defn -main
  []
  (let [rules (map #(parse %) (clojure.string/split (slurp "07.input") #"\n"))]
    ;(println rules)
    (println (solve1 rules "shiny gold"))
    (println (solve2 rules "shiny gold"))
  ))
