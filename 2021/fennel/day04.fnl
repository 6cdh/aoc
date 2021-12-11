(import-macros {: run : printd : debug : timeit} :macros)
(local ut (require :utils))

(fn read-numbers [str sep]
  (ut.map tonumber (ut.split str sep)))

(fn read-board [lines6]
  (ut.map #(read-numbers $ " ") (ut.cdr lines6)))

(fn win? [b]
  (or (ut.any #(ut.all= -1 $) b)
      (ut.any #(ut.all= -1 $) (ut.transpose b))))

(fn score [b n]
  (* n (->> (ut.flatten b)
            (ut.filter #(not= -1 $))
            (ut.sum))))

(fn solve [numbers boards]
  (let [win-order []
        board-number {}]
    (each [_ n (ipairs numbers)]
      (each [i b (ipairs boards)]
        (when (= nil (. board-number i))
          (ut.replace-flat b n -1)
          (when (win? b)
            (ut.append win-order i)
            (tset board-number i n)))) boards)
    (let [no1 (ut.car win-order)
          no-1 (ut.car (ut.last win-order))]
      [(score (. boards no1) (. board-number no1))
       (score (. boards no-1) (. board-number no-1))])))

(run (let [lines (ut.map #$ (io.lines))
           numbers (read-numbers (ut.car lines) ",")
           boards (->> (ut.cdr lines)
                       (ut.chunks 6)
                       (ut.map read-board))]
       (solve numbers boards)))

