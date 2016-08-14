(ns alphabet-cipher.coder)

(defn rotate-sequence [rotate-amount sequence-to-rotate]
  (let [sequence-length (count sequence-to-rotate)
        to-drop (loop [td rotate-amount]
                  (if (not (neg? td))
                    td
                    (recur (+ td sequence-length))))]
    (->> (cycle sequence-to-rotate) (drop to-drop) (take sequence-length))))

(def alphabet (map char (range (int \a) (inc (int \z)))))

(def rotated-alphabet (map rotate-sequence (range) (repeat alphabet)))

(def cipher-rows (map #(into {} [[:row %1] [:letters %2]]) alphabet rotated-alphabet))

(defn cipher-row-to-triples [cipher-row]
    (map #(into {} [[:row (:row cipher-row)][:col %1][:result %2]]) alphabet (:letters cipher-row)))

(def all-triples (flatten (map cipher-row-to-triples cipher-rows)))

(def forward-lookup (reduce #(assoc %1 {:row (:row %2) :col (:col %2)} (:result %2))
                            {} all-triples))

(def reverse-lookup (reduce #(assoc %1 {:col (:col %2) :result (:result %2)} (:row %2))
                            {} all-triples))

(defn encode [keyword message]
  (let [lookups (map #(into {} [[:col %1] [:row %2]]) (cycle keyword) message)]
    (apply str (map forward-lookup lookups))))

(defn decode [keyword message]
  (let [lookups (map #(into {} [[:col %1] [:result %2]]) (cycle keyword) message)]
    (apply str (map reverse-lookup lookups))))

(defn decipher [encrypted-message original-message]
  "scones")
