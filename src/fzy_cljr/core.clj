(ns fzy-cljr.core
  (:gen-class)
  (:require [fzy-cljr.algo :refer [levenshtein]]))


(def test-words 
  ["test" "best" "buy" "borrow" "rest" "arrow" "time" "lime" "arrow"])

(defn palindrome? [s]
	(or (<= (count s) 1)
		(and (= (first s) (last s))
			(palindrome? (rest (butlast s))))))


(defn add [x y]
	(+ x y))

(defn recursive-sum [numbers]
	(if (empty? numbers)
		0
		(+ (first numbers) (recursive-sum (rest numbers)))))

(defn get-matches [word words]
  (map (fn [w] (levenshtein word w)) words))

(defn fuzzy-search [word words distance]
  (doseq [w words]
    (let [d (levenshtein word w)]
      (if (< d distance)
        (println w)))))

(defn squares-table [numlist]
  (doseq [num numlist]
    (println (str "Square of " num " = " (* num num)))))

(defn -main
  "Do nothing for now"
  [& args])
