(import-macros {: run : debug : printd} :macros)
(local {: windows : filter : map} (require :utils))

(fn solve [input]
  [(length (filter #(> (. $1 2) (. $1 1)) (windows input 2)))
   (length (filter #(> (. $1 4) (. $1 1)) (windows input 4)))])

(run (->> (io.lines)
          (map tonumber)
          (solve)))

