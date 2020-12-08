(ns aoc (:gen-class)
  (:require clojure.set))

(defn run [prg]
  (let [
    execution (iterate
      (fn [[[ip acc _ :as state] visited]]
        ;(println "ip = " ip ", acc = " acc ", visited = " visited)
        (if (>= ip (count prg))
          (list [ip acc (= ip (count prg))] nil)
          (let [
             [op arg] (nth prg ip)
             [ip_ acc_] (case op
               "acc" [(+ ip 1) (+ acc arg)]
               "jmp" [(+ ip arg) acc]
               "nop" [(+ ip 1) acc])
             ]
           (if (contains? visited ip)
             (list [ip acc false] nil)
             (list [ip_ acc_ nil] (conj visited ip)))))
      )
      (list [0 0 nil] #{}))
    [state] (first (filter #(nil? (nth % 1)) execution))
    ]
    state))


(defn solve1 [prg]
  (let [state (run prg)]
    (nth state 1)))


(defn fix [prg addr]
  (let [[op arg] (nth prg addr)
        op_ (case op
          "acc" nil
          "jmp" ["nop" arg]
          "nop" ["jmp" arg])]
    (if (nil? op_) nil (assoc prg addr op_))))

(defn solve2 [prg]
  (let [prgs (filter #(not (nil? %)) (map #(fix prg %) (range 0 (count prg))))
        results (map #(run %) prgs)
        state (first (filter #(= true (nth % 2)) results))]
    (nth state 1)))


(defn parse [instr]
  (let [[op arg] (clojure.string/split instr #" ")]
    (list op (Integer/parseInt arg))))

(defn -main
  []
  (let [prg (mapv #(parse %) (clojure.string/split (slurp "08.input") #"\n"))]
    ;(println prg)
    (println (solve1 prg))
    (println (solve2 prg))
  ))
