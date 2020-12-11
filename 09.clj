(ns aoc (:gen-class))

(defn sum2 [s nums]
  (let [nums_ (into (hash-set) nums)
        x (first (filter #(nums_ (- s %)) nums))]
    (if (nil? x) nil (list x (- s x)))))

(defn solve1 [nums]
  (let [[preamble, nums] (split-at 25 nums)
        [_ solutions] (reduce
          (fn [[preamble solutions] n]
            (let [preamble_ (concat (rest preamble) (list n))
                  solution (sum2 n preamble)]
              ;(println "preamble = " preamble ", n = " n)
              (if (nil? solution)
                [preamble_ (conj solutions n)]
                [preamble_ solutions]))
          )
          [preamble []]
          nums)
        ]
    (first solutions)))

(defn solve2__ [sum nums low high s]
  ;(println sum nums low high s)
  (if (nil? nums)
    nil
    (if (= s sum)
      [low high] 
      (if (> s sum)
        nil
        (let [high (first nums)]
          (solve2__ sum (rest nums) low high (+ s high)))))))

(defn solve2_ [sum nums]
  ;(println sum nums)
  (if (nil? nums)
    nil
    (let [n (first nums)
          nums_ (rest nums)
          pair (solve2__ (+ sum n) nums_ n n n)]
      (if (nil? pair)
        (solve2_ sum nums_)
        pair))))

(defn solve2 [nums sum]
  (let [[l h] (solve2_ sum nums)]
    (+ l h)))
  

(defn -main
  []
  (let [nums (map #(Long/parseLong %) (clojure.string/split (slurp "09.input") #"\n"))
        sum (solve1 nums)]
    ;(println nums)
    (println sum)
    (println (solve2 nums sum))
  ))
