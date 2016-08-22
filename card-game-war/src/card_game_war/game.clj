(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn deal-cards [cards]
  (partition (/ (count cards) 2) (shuffle cards)))

(defn index-of [vector item]
  (some (fn [[idx i]] (when (= i item) idx))
   (map (fn [idx i] [idx i]) (range) vector)))

(defn card-value [[suit rank]]
  [(index-of suits suit) (index-of ranks rank)])

(defn fight [card-1 card-2]
  (let [[suit-1-value rank-1-value] (card-value card-1)
        [suit-2-value rank-2-value] (card-value card-2)]
    (cond (> rank-1-value rank-2-value) card-1
          (> rank-2-value rank-1-value) card-2
          (> suit-1-value suit-2-value) card-1
          :else card-2)))

(defn play-round [[p1-cards p2-cards]]
  (let [p1-card (first p1-cards)
        p2-card (first p2-cards)
        winner-card (fight p1-card p2-card)]
    (if (= winner-card p1-card)
      [(concat (rest p1-cards) [p1-card p2-card]) (rest p2-cards)]
      [(rest
        p1-cards)(concat (rest p2-cards) [p2-card p1-card])])))

(defn play-game []
  (loop [[p1-cards p2-cards :as players-cards] (deal-cards cards)
         game-states [players-cards]]
    (cond (empty? p1-cards)
          {:result "Player 2 wins" :game-states game-states}
          (empty? p2-cards)
          {:result "Player 1 wins" :game-states game-states}
          :else
          (let [next-state (play-round players-cards)]
            (recur next-state (conj game-states next-state))))))
