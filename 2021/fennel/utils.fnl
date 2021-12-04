(var M {})

(fn M.debug [...]
  `(let [fennel# (require :fennel)]
     (print (fennel#.view ,...))
     ,...))

(fn M.run [expr]
  `(let [fennel# (require :fennel)]
     (local start# (os.clock))
     (local result# ,expr)
     (local end# (os.clock))
     (print (string.format "result: %s in %.2fms" (fennel#.view result#)
                           (* 1000 (- end# start#))))))

(fn M.enum [name types]
  (local enum-values (collect [k v (ipairs types)]
                       (view v)
                       k))
  `(local ,name ,enum-values))

(fn M.cdr [lst]
  `(let [,lst (M.copy ,lst)]
     (table.remove ,lst 1)
     ,lst))

(fn M.first [lst n from]
  `(do
     (var from# (or ,from 1))
     [(table.unpack ,lst ,from (+ ,from ,n))]))

(fn M.last [lst n]
  `(let [len# (length ,lst)]
     [(table.unpack ,lst (- len# (- n 1)) len#)]))

(fn M.append [lst v]
  `(do
     (table.insert ,lst ,v)
     ,lst))

(fn M.windows [lst n]
  `(do
     (fn windows!# [lst# n# from# result#]
       (let [len# (- (length lst#) (- from# 1))]
         (if (< len# n#) result#
             (do
               ,(M.append `result# (M.first `lst# `n# `from#))
               (windows!# lst# n# (+ from# 1) result#)))))
     (windows!# ,(M.copy lst) ,n 1 [])))

(fn M.iter->list [iter]
  `(let [lst# []]
     (each [v# ,iter]
       (table.insert lst# v#))
     lst#))

(fn M.map [proc tbl]
  `(collect [k# v# (pairs ,tbl)]
     k#
     (,proc v#)))

(fn M.filter [proc tbl]
  `(icollect [_# v# (pairs ,tbl)]
     (when (,proc v#)
       v#)))

(fn M.copy [tbl]
  `(collect [k# v# (pairs ,tbl)]
     k#
     v#))

(fn M.fold [lst init proc]
  `(do
     (var acc# ,init)
     (each [_# v# (ipairs ,lst)]
       (set acc# (,proc acc# v#)))
     acc#))

M

