(local fennel (require :fennel))
(import-macros {: iter->list : windows : filter : map : run : debug} :utils)

(fn solve [input]
  [(length (filter #(> (. $1 2) (. $1 1)) (windows input 2)))
   (length (filter #(> (. $1 4) (. $1 1)) (windows input 4)))])

(run (->> (io.lines)
          (iter->list)
          (map tonumber)
          (solve)))

