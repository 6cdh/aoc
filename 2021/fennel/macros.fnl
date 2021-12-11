(var M {})

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

(fn M.printd [...]
  `(let [fennel# (require :fennel)]
     (each [_# arg# (ipairs [,...])]
       (print (fennel#.view arg#)))))

(fn M.debug [...]
  `(let [result# ,...]
     ,(M.printd `result#)
     result#))

(fn M.timeit [form]
  `(do
     (local start# (os.clock))
     (local result# ,form)
     (local end# (os.clock))
     (print (string.format "%.3fms %s" (* 1000 (- end# start#)) ,(view form)))
     result#))

(fn M.repeat [n ...]
  `(for [_# 1 ,n]
     ,...))

(fn M.tinc [tbl key val]
  `(tset ,tbl ,key (+ ,val (. ,tbl ,key))))

M

