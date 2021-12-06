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
     (print (fennel#.view ,...))))

(fn M.debug [...]
  `(let [result# ,...]
     ,(M.printd `result#)
     result#))

M

