(ns wonderland-number.finder)

; has six digits
(def candidates (range 100000 (inc 999999)))

; multiplied by these, the number has the same digits but different positions
(def can-multiply-by (range 2 7))

(defn are-parallel [n1 n2]
  "Checks if n1 and n2 have all the same digits (but perhaps in different order)"
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn is-wonderland-number [n]
  "Checks if the given number has all the same digits if multiplied by 2,3,4,5 or 6"
  (let [multiples (map #(* n %) can-multiply-by)]
    (every? #(are-parallel n %) multiples)))

(defn wonderland-number []
  (first (filter is-wonderland-number candidates)))
