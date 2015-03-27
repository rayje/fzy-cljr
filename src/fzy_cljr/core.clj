(ns fzy-cljr.core
  (:gen-class))


(defn palindrome? [s]
	(or (<= (count s) 1)
		(and (= (first s) (last s))
			(palindrome? (rest (butlast s))))))

(defn levenshtein 
	"Finds the min number of edits"
	[s1 s2]
	(let [l1 (count s1)
				l2 (count s2)]
		(cond (zero? l1) l2
					(zero? l2) l1
					:else
					(let [cost (if (= (first s1) (first s2)) 0 1)]
						(min (inc (levenshtein (rest s1) s2))
								 (inc (levenshtein s1 (rest s2)))
								 (+ cost
								 		(levenshtein (rest s1) (rest s2))))))))

(defn levenshtein-iter [w1 w2]
  (letfn [(cell-value [same-char? prev-row cur-row col-idx]
            (min (inc (nth prev-row col-idx))
                 (inc (last cur-row))
                 (+ (nth prev-row (dec col-idx)) (if same-char?
                                                   0
                                                   1))))]
    (loop [row-idx  1
           max-rows (inc (count w2))
           prev-row (range (inc (count w1)))]
      (if (= row-idx max-rows)
        (last prev-row)
        (let [ch2           (nth w2 (dec row-idx))
              next-prev-row (reduce (fn [cur-row i]
                                      (let [same-char? (= (nth w1 (dec i)) ch2)]
                                        (conj cur-row (cell-value same-char?
                                                                  prev-row
                                                                  cur-row
                                                                  i))))
                                    [row-idx] (range 1 (count prev-row)))]
          (recur (inc row-idx) max-rows next-prev-row))))))

(defn add [x y]
	(+ x y))

(defn recursive-sum [numbers]
	(if (empty? numbers)
		0
		(+ (first numbers) (recursive-sum (rest numbers)))))

(defn -main
  "Do nothing for now"
  [& args])
