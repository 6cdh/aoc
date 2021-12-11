(import-macros {: run : repeat} :macros)

(local ut (require :utils))

(fn read-numbers [str]
  (ut.map #$ (string.gmatch str "(%d+)")))

(fn solve [input]
  (fn solve-part [days]
    (let [fish-count (ut.freq input)]
      (repeat days
        (ut.rotatel! fish-count 9 1)
        (ut.tinc fish-count 7 (. fish-count 9)))
      (ut.sum fish-count)))
  [(solve-part 80)
   (solve-part 256)])

(run (->> (io.read "*line")
          (read-numbers)
          (ut.map #(+ 1 $))
          (solve)))
