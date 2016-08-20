(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn- permutations [values]
  (if (empty? values)
    [[]]
    (mapcat (fn [value]
              (map #(cons value %) (permutations (remove #(= value %) values))))
            values)))

(defn- make-matrix [values]
  (vec (map vec (partition 3 values))))

(defn- sum-elements [matrix row-indices col-indices]
  (reduce + (map #(nth (nth %1 %2) %3) (repeat matrix) row-indices col-indices)))

(defn- row-sums [matrix]
  (map #(reduce + %) matrix))

(defn- col-sums [matrix]
  (map #(sum-elements matrix (range 3) (repeat %)) (range 3)))

(defn- diag-sum-1 [matrix]
  (sum-elements matrix (range 3) (range 3)))

(defn- diag-sum-2 [matrix]
  (sum-elements matrix (reverse (range 3)) (range 3)))

(defn- is-magic [matrix]
  (let [rng (range 3)
        ds1 (diag-sum-1 matrix)]
    (every? #(= ds1 %)
      (concat (col-sums matrix)
              (row-sums matrix)
              [(diag-sum-2 matrix)]))))

(defn magic-square [values]
  (->> values
       (permutations)
       (map make-matrix)
       (some #(when (is-magic %) %))))
