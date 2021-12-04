(import-macros {: iter->list : filter : map : fold : run} :utils)

(local rules {:up (fn [acc cmd]
                    (set acc.y (- acc.y cmd.dist)))
              :down (fn [acc cmd]
                      (set acc.y (+ acc.y cmd.dist)))
              :forward (fn [acc cmd]
                         (set acc.x (+ acc.x cmd.dist)))})

(local rules2
       {:up (fn [acc cmd]
              (set acc.aim (- acc.aim cmd.dist)))
        :down (fn [acc cmd]
                (set acc.aim (+ acc.aim cmd.dist)))
        :forward (fn [acc cmd]
                   (set acc.x (+ acc.x cmd.dist))
                   (set acc.y (+ acc.y (* acc.aim cmd.dist))))})

(fn parse [str]
  (let [(dir dist) ((str:gmatch "(%a+) (%d+)"))]
    (when dir
      {: dir :dist (tonumber dist)})))

(fn solve [input]
  (fn mul [tbl]
    (* tbl.x tbl.y))

  (fn solve-part [init rules]
    (mul (fold input init (fn [acc cmd]
                            ((. rules cmd.dir) acc cmd)
                            acc))))

  [(solve-part {:x 0 :y 0} rules) (solve-part {:aim 0 :x 0 :y 0} rules2)])

(run (->> (io.lines)
          (iter->list)
          (map parse)
          (solve)))

