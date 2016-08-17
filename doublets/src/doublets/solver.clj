(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn linked? [word1 word2]
  (let [equal-length (->> [word1 word2] (map count) (reduce =))
        one-difference (= 1 (->> (map = word1 word2) (filter false?) (count)))]
    (and equal-length one-difference)))

(defn word-entry [word]
  { :word word
    :path [word]
    :words-seen #{word} })

(defn get-linked-words [word]
  (->> words
       (filter #(linked? (:word word) %))
       (filter #(not (contains? (:words-seen word) (:word %))))
       (map #(into {}
                   [[:word %]
                    [:path (conj (:path word) %)]
                    [:words-seen (conj (:words-seen word) %)]]))))

(defn get-links [wds target]
  (let [linked-words (mapcat get-linked-words wds)
        target-found (some #(when (= target (:word %)) %) linked-words)]
    (cond
     target-found target-found
     (empty? linked-words) nil
     :else (recur linked-words target))))

(defn doublets [word1 word2]
  (let [result (get-links [(word-entry word1)] word2)]
    (if (nil? result)
      []
      (:path result))))
