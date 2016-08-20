(ns tiny-maze.solver)

(def left [0 -1])
(def right [0 1])
(def up [-1 0])
(def down [1 0])

(defn- item-at [maze [row col]]
  (let [r (get maze row 1)]
    (get r col 1)))

(defn- set-at [maze [row col] value]
  (assoc-in maze [row col] value))

(defn- find-item [maze item]
  (let [height (count maze)]
    (if (= height 0)
      nil
      (some #(when (not (nil? %)) %)
        (for [i (range height)
              j (range (count (maze 0)))]
          (when (= item (item-at maze [i j]))
            [i j]))))))

(defn- go-to [maze [row col] [row-delta col-delta]]
  (let [new-pos [(+ row row-delta) (+ col col-delta)]
        item (item-at maze new-pos)]
    (if (or (= item 1) (= item :x))
       nil
       (let [new-maze (-> maze (set-at [row col] :x) (set-at new-pos :x))]
           {:maze new-maze :solved (= item :E) :pos new-pos}))))

(defn- next-possible-steps [{:keys [maze pos]}]
  (filter #(not (nil? %))
    (map #(go-to maze pos %) [left right up down])))

(defn- init-maze [maze]
  {:maze maze
   :pos (find-item maze :S)
   :solved false})

(defn solve-maze [maze]
  (loop [maze-states [(init-maze maze)]]
    (let [solved-state (some #(when (:solved %) %) maze-states)]
      (if solved-state
        (:maze solved-state)
        (recur (mapcat next-possible-steps maze-states))))))
