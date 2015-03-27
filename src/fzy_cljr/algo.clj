(ns fzy-cljr.algo)

(defn get-cv
	"Gets the cell value" 
	[same-char? prev-row cur-row col-idx]
	(min 
		(inc (nth prev-row col-idx))
		(inc (last cur-row))
		(+ (nth prev-row (dec col-idx))
			(if same-char? 0 1))))

(defn eq-ith
	[word char i]
	(= (nth word (dec i)) char))

(defn next-pr 
	"Gets the next prev-row"
	[w1 ch2 prev-row row-idx]
	(reduce 
  			;cur-row is an array
  			;i is a list
  	(fn [cur-row i]
  		    ; set same-char to 
  		    ;   true: if i-1th char == ch2
  		    ;   false: otherwise
  		    
  		    ; Turn this into a partial
			(let [same-char? (eq-ith w1 ch2 i)
				    cv (get-cv same-char? prev-row cur-row i)]
				; add to cur-row
				;   the result of get-cv
				(conj cur-row cv)
			))

  	 ; Args for anonymous fn
  	 ; cur-row  
      [row-idx] 
     ; i - list from 1 to prev-row count - 1
      (range 1 (count prev-row))))

(defn levenshtein 
	[w1 w2]
  (loop [row-idx  1
         max-rows (inc (count w2))
         prev-row (range (inc (count w1)))]

    ; Loop until row-idx == max-rows
    (if (= row-idx max-rows)
      (last prev-row)

      		 ; Set ch2 to rox-idx - 1
      (let [ch2 (nth w2 (dec row-idx))
            next-prev-row (next-pr w1 ch2 prev-row row-idx)]

        ; recursively loop
        ;   increment row-idx 
        (recur (inc row-idx) max-rows next-prev-row)
      )
    )
  )
)