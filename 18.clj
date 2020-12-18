(ns aoc (:gen-class))

(defn calc [ts s op]
  (if (empty? ts)
    s
    (let [t (first ts)
          ts (rest ts)]
      ;(println s op " | " t ts)
      (case t
        "+" (calc ts s +)
        "*" (calc ts s *)
        "(" (let [[ts_ v] (calc ts 0 +)] (calc ts_ (op s v) nil))
        ")" (list ts s)
        (calc ts (op s (Integer/parseInt t)) nil)
        ))
  ))

(defn solve1 [exprs]
  (let [vals (map #(calc % 0 +) exprs)]
    (println "vals: " vals)
    (apply + vals)))


(defn parseExpr [s]
  (let [s (clojure.string/replace s #"\(" "( ")
        s (clojure.string/replace s #"\)" " )")
        tokens (clojure.string/split s #" ")]
    (println tokens)
    tokens))

(defn -main
  []
  (let [exprs (map parseExpr (clojure.string/split (slurp "18.input") #"\n"))]
    ;(println exprs)
    (println (solve1 exprs))
    ;(println (solve1 map_ next-cell-2))
  ))
