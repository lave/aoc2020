(ns aoc (:gen-class))

(defn matches1 [tbl i s]
  (let [[prefixes len rule] (get tbl i)]
    ;(println rule s prefixes len (count s))
    (if (and
          (= len (count s))
          (some #(clojure.string/starts-with? s (str %)) prefixes))
      (some (fn [op]
              (every? #(<= 0 %)
                      (reductions
                        (fn [i t]
                          (if (number? t)
                            (let [len (second (get tbl t))]
                              (if (matches1 tbl t (subs s i (+ i len)))
                                (+ i len)
                                -1))
                            (if (= t (subs s i (+ i (count t))))
                              (+ i (count t))
                              -1)))
                        0
                        op)))
            rule)
      false)
  ))

(defn matches-seq [tbl indexes s]
  (every? #(<= 0 %)
          (reductions
            (fn [i t]
              ;(println i t)
              (if (number? t)
                (let [len (second (get tbl t))]
                  (if (matches1 tbl t (subs s i (+ i len)))
                    (+ i len)
                    -1))
                (if (= t (subs s i (+ i (count t))))
                  (+ i (count t))
                  -1)))
            0
            indexes)))

(defn solve1 [tbl ss]
  (count (filter #(matches1 tbl 0 %) ss)))


(defn matches2 [tbl s]
  (let [l31 (second (get tbl 31))
        l42 (second (get tbl 42))
        n (- (count s) l42 l42 l31)
        n42 (quot n l42)]
    (if (< n 0)
      false
      (some (fn [n42]
              (let [n31 (quot (- n (* n42 l42)) l31)
                    indexes (concat (repeat (+ 2 n42) 42) (repeat (+ 1 n31) 31))]
                  ; there's always more 42 than 31
                  (if (< 0 (- n31 n42))
                    false
                    (matches-seq tbl indexes s))))
            (range 0 (+ 1 n42))))
    ))

(defn solve2 [tbl ss]
  (count (filter #(matches2 tbl %) ss)))


(defn build-tbl-1 [rules tbl i]
  (let [rule (get rules i)
        tbl_ (reduce
               (fn [tbl tok] (build-tbl-1 rules tbl tok))
               tbl
               (filter number? (apply concat rule)))
        ]
    (let [
        starts (map (fn [op]
                      (let [t (first op)]
                        (if (number? t)
                          (first (get tbl_ t))
                          t)))
                    rule)
        lens (map (fn [op]
                    (apply + (map (fn [t]
                                    (if (number? t)
                                      (second (get tbl_ t))
                                      (count t)))
                                  op)))
                    rule)
        lens_ (distinct lens)
        ]
    ;(println i rule starts lens_)
    (if (= 1 (count lens_))
      (assoc tbl_ i (list
                      (set (apply concat starts))
                      (first lens_)
                      rule))
      (throw (Exception. (str "non-unique lens for rule #" i ": " lens_))))
      )))

(defn build-tbl [rules]
  (let [tbl (hash-map)]
    (build-tbl-1 rules tbl 0)))

(defn parse-token [t]
  (if (clojure.string/starts-with? t "\"")
    (subs t 1 (- (count t) 1))
    (Integer/parseInt t)))

(defn parse-rule [s]
  (let [[n ds] (clojure.string/split s #": ")
        ds (clojure.string/split ds #" \| ")]
    [(Integer/parseInt n) (map #(map parse-token (clojure.string/split % #" ")) ds)]
  ))


(defn -main
  []
  (let [[rules_ strings_] (clojure.string/split (slurp "19.input") #"\n\n")
        rules (into (hash-map) (mapv parse-rule (clojure.string/split rules_ #"\n")))
        strings (clojure.string/split strings_ #"\n")
        tbl (build-tbl rules)]
      ;(println tbl)
      (println (solve1 tbl strings))
      (println (solve2 tbl strings))
  ))
