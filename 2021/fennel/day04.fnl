(import-macros {: run : printd : debug : timeit} :macros)
(local {: chunks
        : car
        : cdr
        : map
        : filter
        : flatten
        : for-each
        : for-each-key
        : split
        : any
        : all=
        : transpose
        : replace-flat
        : append
        : last
        : sum} (require :utils))

(fn read-numbers [str sep]
  (map tonumber (split str sep)))

(fn read-board [lines6]
  (map #(read-numbers $ " ") (cdr lines6)))

(fn win? [b]
  (or (any #(all= -1 $) b) (any #(all= -1 $) (transpose b))))

(fn score [b n]
  (* n (->> (flatten b)
            (filter #(not= -1 $))
            (sum))))

(fn solve [numbers boards]
  (let [win-order []
        board-number {}]
    (each [_ n (ipairs numbers)]
      (for-each-key (fn [i b]
                      (when (= nil (. board-number i))
                        (replace-flat b n -1)
                        (when (win? b)
                          (append win-order i)
                          (tset board-number i n)))) boards))
    (let [no1 (car win-order)
          no-1 (car (last win-order))]
      [(score (. boards no1) (. board-number no1))
       (score (. boards no-1) (. board-number no-1))])))

(run (let [lines (map #$ (io.lines))
           numbers (read-numbers (car lines) ",")
           boards (map read-board (chunks 6 (cdr lines)))]
       (solve numbers boards)))

