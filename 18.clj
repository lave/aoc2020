(ns aoc (:gen-class))

(defn calc1 [ts s op]
  (if (empty? ts)
    s
    (let [t (first ts)
          ts (rest ts)]
      ;(println s op " | " t ts)
      (case t
        "+" (calc1 ts s +)
        "*" (calc1 ts s *)
        "(" (let [[ts_ v] (calc1 ts 0 +)] (calc1 ts_ (op s v) nil))
        ")" (list ts s)
        (calc1 ts (op s (Integer/parseInt t)) nil)
        ))
  ))

(defn solve1 [exprs]
  (let [vals (map #(calc1 % 0 +) exprs)]
    ;(println "vals: " vals)
    (apply + vals)))


(defn calc-plus [ts res]
  (if (empty? ts)
    res
    (let [t (first ts)
          ts (rest ts)]
      ;(println "  " t ts res)
      (if (= t "+")
        (let [n (first ts)
              ts (rest ts)
              res (conj (rest res) (+ (first res) n))]
          (calc-plus ts res))
        (calc-plus ts (conj res t)))
      )))
          
(defn calc [ts]
  (let [ops_ (filter #(not (= "*" %)) (calc-plus ts '()))]
    ;(println "calc: " ts ops_)
    (apply * ops_)))

(defn calc2 [ts ops]
  (if (empty? ts)
    (calc ops)
    (let [t (first ts)
          ts (rest ts)]
      (println ops " | " t ts)
      (case t
        "+" (calc2 ts (conj ops "+"))
        "*" (calc2 ts (conj ops "*"))
        "(" (let [[ts_ v] (calc2 ts [])] (calc2 ts_ (conj ops v)))
        ")" (list ts (calc ops))
        (calc2 ts (conj ops (Integer/parseInt t)))
        ))
  ))

(defn solve2 [exprs]
  (let [vals (map #(calc2 % []) exprs)]
    ;(println "vals: " vals)
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
    ;(println (solve1 exprs))
    ;(println (calc '(4 "+" 6 "*" 3 "+" 2 "+" 1)))
    (println (solve2 exprs))
  ))
