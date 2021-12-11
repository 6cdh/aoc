(import-macros {: run : debug} :macros)
(local ut (require :utils))

(fn tonumber-base2 [n]
  (tonumber n 2))

(fn part1 [input]
  (let [input (ut.transpose input)
        gamma-rate (tonumber-base2 (ut.list->string (ut.map ut.most input)))
        epsilon-rate (tonumber-base2 (ut.list->string (ut.map ut.least input)))]
    (* gamma-rate epsilon-rate)))

(fn part2 [input]
  (fn filter-binary [input is-zero]
    (var result input)
    (for [i 1 (length (. input 1))
          :until (= 1 (length result))]
      (let [f (ut.freq (ut.map #(. $ i) result))
            bit (if (is-zero (. f :0) (. f :1)) :0 :1)]
        (set result (ut.retain-if #(= bit (. $ i)) result))))
    (tonumber-base2 (ut.list->string (ut.car result))))

  (* (filter-binary input #(> $1 $2))
     (filter-binary input #(<= $1 $2))))

(fn solve [input]
  [(part1 input)
   (part2 input)])

(run (->> (io.lines)
          (ut.filter (partial not= ""))
          (ut.map ut.string->list)
          (solve)))

