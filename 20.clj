(ns aoc (:gen-class))

(defn find-corner-tiles [tiles]
  (let [borders (mapcat (fn [[_ b-cw b-cww]] (concat b-cw b-cww)) tiles)
        borders-counts (into {} (for [[k v] (group-by identity borders)] [k (count v)]))
        unique-borders (into (hash-set) (map first (filter #(= 1 (second %)) borders-counts)))
        unique-tiles (map (fn [[id b-cw b-ccw]]
                            (list id (+ (apply + (map (fn [b] (if (contains? unique-borders b) 1 0)) b-cw))
                                        (apply + (map (fn [b] (if (contains? unique-borders b) 1 0)) b-ccw))))) tiles)
        corner-tiles (filter #(= 4 (second %)) unique-tiles)]
    corner-tiles))

(defn solve1 [corner-tiles]
  (apply * (map first corner-tiles)))

(defn get-borders [[id [h1 h2 h3 h4] [h1_ h2_ h3_ h4_]]]
  (list [h1 [id 1]]
        [h2 [id 2]]
        [h3 [id 3]]
        [h4 [id 4]]
        [h1_ [id -1]]
        [h2_ [id -2]]
        [h3_ [id -3]]
        [h4_ [id -4]]))

; returns bottom border for cell with op describing top border
(defn top-to-bottom [[_ [h1 h2 h3 h4] [h1_ h2_ h3_ h4_]] op]
  (case op
    1 h3_
    2 h4_
    3 h1_
    4 h2_
    -1 h3
    -2 h4
    -3 h1
    -4 h2))

; returns right border for cell with op describing top border
(defn left-to-top [[_ [h1 h2 h3 h4] [h1_ h2_ h3_ h4_]] op]
  (case op
    1 h2
    2 h3
    3 h4
    4 h1
    -1 h4_
    -2 h1_
    -3 h2_
    -4 h3_))


(defn make-row [t-map b-map w b0]
  (let []
    (take w (drop 1 (iterate (fn [[id op b]]
               (let [[id_ op_] (first (filter #(not (= id (first %))) (get b-map b)))
                     tile (t-map get id_)]
                 ;(println "--" id op b id_ op_)
                 (list id_ op_ (top-to-bottom (get t-map id_) op_))
               ))
               [nil nil b0]
             )))
    ))

(defn transform [image op]
  (case op
    1 image
    ; rotate 90 degrees counter-clockwise
    2 (reverse (apply map list image))
    ; rotate 180 degrees
    3 (reverse (map reverse image))
    ; rotate 90 degrees clockwise
    4 (apply map list (reverse image))
    ; flip by vertical axis (vertical mirror)
    -1 (map reverse image)
    ; flip by auxiliary diagonal
    -2 (reverse (apply map list (reverse image)))
    ; flip by horizontal axis (horizontal mirror)
    -3 (reverse image)
    ; flip by main diagonal (transpose)
    -4 (apply map list image)
    ))

(defn make-image-tile [image op test?]
  (let [img (transform image op)]
    (case test?
      true (map #(conj % 2) img)
      false (map #(rest (drop-last %)) img))
  ))

(defn make-image-row [t-map row test?]
  (let [row_ (map (fn [[id op]] (make-image-tile (nth (get t-map id) 3) op test?)) row)
        flipped (apply map list row_)
        joined (map #(apply concat %) flipped)
        ]
    (case test?
      true (conj joined '(2))
      false (rest (drop-last joined)))
  ))

(defn make-image [t-map rows test?]
  (apply concat (map #(make-image-row t-map % test?) rows)))


(defn print-image [image]
  (let [lines (map (fn [row] (clojure.string/join (conj (map #(case % 0 "." 1 "#" 2 " ") row) " "))) image)]
    (doseq [item lines]
     (println item))))


(defn sea-monster []
  (let [lines (list
"                  # "
"#    ##    ##    ###"
" #  #  #  #  #  #   ")]
  (mapv (fn [line] (mapv #(if (= " " %) 0 1) (clojure.string/split line #""))) lines)))


(defn is-subimage [image subimage w_ h_ x y]
  (every? true? (for [x_ (range w_)
                y_ (range h_)]
                (>= (get (get image (+ y y_)) (+ x x_))
                   (get (get subimage y_) x_)))
          ))

(defn find-subimages [image subimage]
  (let [w (count (first image))
        h (count image)
        w_ (count (first subimage))
        h_ (count subimage)]
    (for [x (range (- w w_))
          y (range (- h h_))
          :when (is-subimage image subimage w_ h_ x y)]
      (list x y))
    ))


(defn is-sea-monster [image subimage positions x y]
  (let [w_ (count (first subimage))
        h_ (count subimage)]
  (some true? (map (fn [[x_ y_]]
                     (and (<= x_ x)
                          (<= y_ y)
                          (< x (+ x_ w_))
                          (< y (+ y_ h_))
                          (= 1 (get (get subimage (- y y_)) (- x x_)))
                     )) positions))
  ))

(defn get-roughness [[positions image] subimage]
  (let [w (count (first image))
        h (count image)]
    (apply +
      (for [x (range w)
            y (range h)]
        (if (or (= 0 (get (get image y) x))
                (is-sea-monster image subimage positions x y)
            ) 0 1)))
  ))


(defn solve2 [tiles id0 w]
  (let [t-map (into (hash-map) (map (fn [t] [(first t) t]) tiles))
        b-map (into (hash-map) (for [[k v] (group-by first (mapcat get-borders tiles))] [k (map second v)]))
        tile0 (get t-map id0)
        ; select first unique border as top border for tile 0
        b-l (last (filter #(= 1 (count (get b-map %))) (second tile0)))
        row0 (make-row t-map b-map w b-l)
        cols (map (fn [[id op b]] (make-row t-map b-map w (left-to-top (get t-map id) op))) row0)
        rows (apply map list cols)
        image (make-image t-map rows false)
        images (map #(mapv vec (transform image %)) [1 2 3 4 -1 -2 -3 -4])
        images-with-monsters (map #(list (find-subimages % (sea-monster)) %) images)
        good-image (first (filter #(not (empty? (first %))) images-with-monsters))
        habitat-roughness (get-roughness good-image (sea-monster))
       ]
        ;(println t-map)
        ;(println rows)
        ;(print-image (second good-image))
        habitat-roughness
  ))


(defn border-hash [row]
  (reduce (fn [s b] (+ b (* 2 s))) 0 row))

(defn parse-tile [s]
  (let [lines (clojure.string/split s #"\n")
        [_ id] (re-matches #"Tile (\d+):" (first lines))
        image (map (fn [line] (map #(if (= "." %) 0 1) (clojure.string/split line #""))) (rest lines))
        b1 (first image)
        b2 (map last image)
        b3_ (last image)
        b4_ (map first image)
        b1_ (reverse b1)
        b2_ (reverse b2)
        b3 (reverse b3_)
        b4 (reverse b4_)
        b-cw (map border-hash [b1 b2 b3 b4])
        b-ccw (map border-hash [b1_ b2_ b3_ b4_])
        ]
    (list (Integer/parseInt id) b-cw b-ccw image)))

(defn -main
  []
  (let [tiles (map parse-tile (clojure.string/split (slurp "20.input") #"\n\n"))
        w (int (Math/sqrt (count tiles)))
        corner-tiles (find-corner-tiles tiles)]
      ;(println tiles)
      (println (solve1 corner-tiles))
      (println (solve2 tiles (first (first corner-tiles)) w))
  ))
