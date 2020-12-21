(ns aoc (:gen-class)
  (:require clojure.set))

(defn resolve1 [[alls all-map]] 
  (let [single (first (filter #(= 1 (count (second %))) alls))]
    (if (nil? single)
      [alls all-map]
      (let [[all_ ing_] single
            ing_ (first ing_)]
        [(map (fn [[all ings]] (list all (disj ings ing_))) (remove #(= all_ (first %)) alls))
         (assoc all-map all_ ing_)]))
  ))

(defn allergens-map [products]
  (let [allergens (group-by first (mapcat (fn [[ings alls]] (map #(list % (into (hash-set) ings)) alls)) products))
        allergens (map (fn [[all ings]] (list all (apply clojure.set/intersection (map second ings)))) allergens)
        all-maps (iterate resolve1 [allergens (hash-map)])]
    ;(println allergens)
    ;(println all-map)
    (second (first (filter #(empty? (first %)) all-maps)))
  ))

(defn solve1 [products all-map]
  (let [ings-with-all (into #{} (map second all-map))]
    (count (filter #(not (contains? ings-with-all %)) (mapcat first products)))
  ))

(defn solve2 [all-map]
  (clojure.string/join "," (map second (sort-by first all-map))))

(defn parse [line]
  (let [[_ ingridients allergens] (re-matches #"(.*) \(contains (.*)\)" line)]
    (list (clojure.string/split ingridients #" ") (clojure.string/split allergens #", "))))

(defn -main
  []
  (let [products (map parse (clojure.string/split (slurp "21.input") #"\n"))
        allergens-map (allergens-map products)]
      ;(println products)
      (println (solve1 products allergens-map))
      (println (solve2 allergens-map))
      ))
