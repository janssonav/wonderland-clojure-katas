(ns fox-goose-bag-of-corn.puzzle)
(require '[clojure.set :as s])

(defn to-sets [pos]
  (vec (map set pos)))

(defn remove-boat [[l b r]]
  [l (disj b :boat) r])

(defn add-boat [[l b r]]
  [l (conj b :boat) r])

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def cannot-coexist #{#{:fox :goose} #{:corn :goose}})

(defn is-valid-position? [pos]
  (not-any? #(contains? cannot-coexist %) pos))

(defn is-end [[l b r]]
  (= #{:fox :goose :corn :you} r))

(defn next-positions [[l b r]]
  (cond
   (contains? l :you)
     (let [l-things (disj l :you)
           just-you [l-things #{:you} r]
           move-thing (fn [thing]
                        [(disj l-things thing) (into #{} [:you thing]) r])
           thing-moved-states (map move-thing l-things)]
       (conj thing-moved-states just-you))
   (contains? r :you)
     (let [r-things (disj r :you)
           just-you [l #{:you} r-things]
           move-thing (fn [thing]
                        [l (into #{} [:you thing]) (disj r-things thing)])
           thing-moved-states (map move-thing r-things)]
       (conj thing-moved-states just-you))
   :else
     (let [b-thing (first (disj b :you))
           move-left [(if (nil? b-thing) (conj l :you) (into l [:you b-thing])) #{} r]
           move-right [l #{} (if (nil? b-thing) (conj r :you) (into r [:you b-thing]))]]
           [move-left move-right])))

(defn next-valid-positions
  ([pos] (next-valid-positions pos [pos] #{pos}))
  ([pos path positions-seen]
    (->> pos
         (next-positions)
         (filter #(and (is-valid-position? %) (not (contains? positions-seen %))))
         (map #(assoc {:path (conj path %) :positions-seen (conj positions-seen %)} :position %)))))

(defn traverse [positions]
  (let [next (mapcat #(next-valid-positions (:position %) (:path %) (:positions-seen %)) positions)
        end (some #(when (is-end (:position %)) (:path %)) next)]
    (if end
      end
      (recur next))))

(defn river-crossing-plan []
  (->> start-pos
       (first)
       (to-sets)
       (remove-boat)
       (next-valid-positions)
       (traverse)
       (map add-boat)))
