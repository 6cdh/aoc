(import-macros {: iter->list : map : fold : run} :utils)

(fn parse [str]
  (let [(dir dist) ((str:gmatch "(%a+) (%d+)"))]
    (when dir
      {: dir :dist (tonumber dist)})))

(fn solve [input]
  (fn solve-part [init result]
    (result (fold input init
                  (fn [acc cmd]
                    (match cmd.dir
                      :up {:aim (- acc.aim cmd.dist) :x acc.x :y acc.y}
                      :down {:aim (+ acc.aim cmd.dist) :x acc.x :y acc.y}
                      :forward {:aim acc.aim
                                :x (+ acc.x cmd.dist)
                                :y (+ acc.y (* acc.aim cmd.dist))})))))

  [(solve-part {:aim 0 :x 0 :y 0} #(* $1.x $1.aim))
   (solve-part {:aim 0 :x 0 :y 0} #(* $1.x $1.y))])

(run (->> (io.lines)
          (iter->list)
          (map parse)
          (solve)))

