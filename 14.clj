(ns aoc (:gen-class))

(defn op1 [[mem [and-mask or-mask]] instr]
  ;(println mem and-mask or-mask instr)
  (case (first instr)
    "mask" [mem [(nth instr 1) (nth instr 2)]]
    "mem" [(assoc mem (nth instr 1) (bit-or or-mask (bit-and and-mask (nth instr 2)))) [and-mask or-mask]]
    ))

(defn solve1 [prg]
  (let [[mem _] (reduce op1 [{} [0 0]] prg)]
    (apply + (vals mem))
    ))

(defn make-addrs [addr float-bits]
  (reduce (fn [addrs bit]
            ;(println "addrs " addrs bit)
            (let [or-mask (bit-shift-left 1 bit)
                  and-mask (bit-not or-mask)]
              (mapcat #(list (bit-and and-mask %) (bit-or or-mask %)) addrs)
              ))
          [addr]
          float-bits))

(defn op2 [[mem [and-mask or-mask float-bits]] instr]
  ;(println mem and-mask or-mask float-bits instr)
  (case (first instr)
    "mask" [mem [(nth instr 1) (nth instr 2) (nth instr 3)]]
    "mem" (let [addrs (make-addrs (bit-or or-mask (nth instr 1)) float-bits)
                val (nth instr 2)
                mem_ (reduce (fn [mem__ addr] (assoc mem__ addr val)) mem addrs)]
            [mem_ [and-mask or-mask float-bits]])))

(defn solve2 [prg]
  (let [[mem _] (reduce op2 [{} [0 0]] prg)]
    (apply + (vals mem))
    ))


(defn and-mask [s]
  (reduce (fn [m c]
            (case c
              \0 (* m 2)
              (+ 1 (* m 2))))
            0
            s))

(defn or-mask [s]
  (reduce (fn [m c]
            (case c
              \1 (+ 1 (* m 2))
              (* m 2)))
            0
            s))

(defn n-x [s]
  (first (reduce (fn [[ns n] c]
            (case c
              \X [(conj ns n) (- n 1)]
              [ns (- n 1)]))
            [[] 35]
            s)))


(defn parse [l]
  (if (clojure.string/starts-with? l "mask")
    (let [[_ mask] (re-matches #"mask = ([01X]{36})" l)]
      (list "mask" (and-mask mask) (or-mask mask) (n-x mask)))
    (let [[_ index value] (re-matches #"mem\[(\d+)\] = (\d+)" l)]
      (list "mem" (Integer/parseInt index) (Integer/parseInt value)))))

(defn -main
  []
  (let [prg (map parse (clojure.string/split (slurp "14.input") #"\n"))]
    ;(println prg)
    (println (solve1 prg))
    (println (solve2 prg))
  ))
