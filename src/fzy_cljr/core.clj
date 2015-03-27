(ns fzy-cljr.core
  (:gen-class))


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

(defn -main
  "Do nothing for now"
  [& args])
