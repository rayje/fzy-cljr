(ns fzy-cljr.algo)

(defn next-ith [p i]
  (inc (nth p i)))

(defn calc-dist [same-char? prev-row i]
  (+ (nth prev-row (dec i))
    (if same-char? 0 1)))

(defn get-min
  "Gets the cell value" 
  [same-char? prev-row cur-row i]
    (min 
      (next-ith prev-row i)
      (inc (last cur-row))
      (calc-dist same-char? prev-row i)
  )
)

(defn same-char
  "Check that the ith-1 character in word macthes
   the char param"
  [word char i]
  (= (nth word (dec i)) char))

(defn next-pr 
  "Gets the next prev-row"
  [w1 ch2 prev-row row-idx]
  ;(println (str "prev-row: " prev-row))
  (reduce 
        ;cur-row is an array
        ;i is a list
    (fn [cur-row i]
          ; set same-char to 
          ;   true: if i-1th char == ch2
          ;   false: otherwise

          ; Turn this into a partial
      (let [same-char? (same-char w1 ch2 i)
            cv (get-min same-char? prev-row cur-row i)]
        ;(println (str "w1: " w1 " cv: " cv))
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
      ; return the last item in prev-row
      (last prev-row)

           ; Set ch2 to row-idx - 1
      (let [ch2 (nth w2 (dec row-idx))
            next-prev-row (next-pr w1 ch2 prev-row row-idx)]

        ; recursively loop
        ;   increment row-idx 
        (recur (inc row-idx) max-rows next-prev-row)
      )
    )
  )
)