(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= (fight [:club 2] [:club 3]) [:club 3])))
  (testing "queens are higher rank than jacks"
    (is (= (fight [:heart :queen] [:heart :jack]) [:heart :queen])))
  (testing "kings are higher rank than queens"
    (is (= (fight [:spade :queen] [:spade :king]) [:spade :king])))
  (testing "aces are higher rank than kings"
    (is (= (fight [:diamond :king] [:diamond :ace]) [:diamond :ace])))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= (fight [:club :queen] [:spade :queen]) [:club :queen])))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= (fight [:club 6] [:diamond 6]) [:diamond 6])))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= (fight [:heart :ace] [:diamond :ace]) [:heart :ace]))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (let [result (play-game)
              [p1-cards p2-cards] (last (:game-states result))]
          (if (= "Player 1 wins" (:result result))
            (empty? p2-cards)
            (empty? p1-cards))))))
