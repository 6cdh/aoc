(import-macros {: run : debug : printd : timeit} :macros)
(local ut (require :utils))

(fn is-hv [line]
  (let [[x1 y1 x2 y2] line]
    (or (= x1 x2) (= y1 y2))))

(fn is-diag [line]
  (let [[x1 y1 x2 y2] line]
    (= (ut.abs (- x1 x2)) (ut.abs (- y1 y2)))))

(fn all-points [line]
  (let [[x1 y1 x2 y2] line
        dx (ut.sign (- x2 x1))
        dy (ut.sign (- y2 y1))]
    (icollect [_ i (ut.iter (ut.range 0 1000)) :until (and (= (+ x1 (* i dx))
                                                              (+ x2 dx))
                                                           (= (+ y1 (* i dy))
                                                              (+ y2 dy)))]
      [(+ x1 (* i dx)) (+ y1 (* i dy))])))

(fn read []
  (->> (io.lines)
       (ut.map #(ut.map tonumber (string.gmatch $ "(%d+)")))))

(fn solve-part [input pred]
  (->> input
       (ut.filter pred)
       (ut.map all-points)
       (#(ut.flatten $ 1))
       (ut.map #(+ (* 1000 (. $ 1)) (. $ 2)))
       (ut.freq)
       (ut.filter #(> $ 1))
       (length)))

(fn solve [input]
  [(solve-part input is-hv) (solve-part input #(or (is-hv $) (is-diag $)))])

(run (-> (read) (solve)))

