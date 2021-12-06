(var M {})

(macro default [variable val]
  `(var ,variable (or ,variable ,val)))

(fn call [proc ...]
  (proc ...))

(fn pickn [n ...]
  "pick n-th argument"
  (. [...] n))

;; iterator

(fn M.iter-list [tbl i]
  (when (< i (length tbl))
    (values (M.inc i) (. tbl (M.inc i)))))

(fn M.iter-table [tbl k]
  (next tbl k))

(let [mt {}]
  (fn M.range [beg end step]
    (default step 1)
    (setmetatable {: end : beg : step} mt))

  (fn M.range? [tbl]
    (= (getmetatable tbl) mt)))

(fn M.iter-range [state k]
  (if (= nil k)
      state.beg
      (let [next (+ k state.step)]
        (when (or (and (> state.step 0) (<= next state.end))
                  (and (< state.step 0) (>= next state.end)))
          next))))

(fn M.iter-string [str i]
  (when (< i (length str))
    (values (M.inc i) (str:sub (M.inc i) (M.inc i)))))

(fn M.iter [obj]
  (match (type obj)
    :string (values M.iter-string obj 0)
    (where :table (M.list? obj)) (values M.iter-list obj 0)
    (where :table (M.range? obj)) (values M.iter-range obj nil)
    :table (values M.iter-table obj nil)
    :function obj))

(fn M.read-only [tbl]
  (let [wrapper {}
        mt {:__index tbl :__newindex #(error "can't modify read only table" 2)}]
    (setmetatable wrapper mt)
    wrapper))

(fn M.list? [tbl]
  (and (not= nil (. tbl 1)) (= nil (. tbl 0))))

;; functions

(fn M.inc [n]
  (+ n 1))

(fn M.dec [n]
  (- n 1))

(fn M.abs [n]
  (math.abs n))

(fn M.sign [n]
  (if (< n 0) -1
      (> n 0) 1
      0))

(fn M.car [lst]
  (. lst 1))

(fn M.cdr [lst]
  [(table.unpack lst 2 (length lst))])

(fn M.first [lst n from]
  (var from (or from 1))
  [(table.unpack lst from (+ from n))])

(fn M.last [lst n]
  (let [len (length lst)]
    [(table.unpack lst (- len (M.dec n)) len)]))

(fn M.append [lst v]
  (table.insert lst v)
  lst)

(fn M.windows [lst n]
  (fn windows! [lst n from result]
    (let [len (- (length lst) (- from 1))]
      (if (< len n)
          result
          (do
            (M.append result (M.first lst n from))
            (windows! lst n (+ from 1) result)))))

  (windows! (M.make-list lst) n 1 []))

(fn M.make-list [obj proc]
  (default proc (fn [...]
                  ...))
  (icollect [k v (M.iter obj)]
    (if (= nil v)
        (proc k)
        (proc v))))

(fn M.make-table [obj proc]
  (default proc (fn [...]
                  ...))
  (collect [k v (M.iter obj)]
    (proc k v)))

(fn M.map [proc obj]
  (M.make-list obj (fn [v]
                     (proc v))))

(fn M.map-table [proc tbl]
  (M.make-table tbl (fn [k v]
                      (values k (proc v)))))

(fn M.filter [proc lst]
  (M.make-list lst (fn [v]
                     (when (proc v)
                       v))))

(fn M.filter-table [proc tbl]
  (M.make-table tbl (fn [k v]
                      (when (proc k v)
                        (values k v)))))

(fn M.fold [lst init proc]
  (var acc init)
  (each [k v (M.iter lst)]
    (if (= nil v)
        (set acc (proc acc k))
        (set acc (proc acc v))))
  acc)

(fn M.fold-table [tbl init proc]
  (var acc init)
  (each [k v (M.iter tbl)]
    (set acc (proc acc k v)))
  acc)

(fn M.transpose [list2d]
  (fn transpose-ith-col [i list2d]
    (M.map #(. $ i) list2d))

  (M.map #(transpose-ith-col $ list2d) (M.range 1 (length (. list2d 1)))))

(fn M.string->list [str]
  (let [lst []]
    (for [i 1 (length str)]
      (M.append lst (str:sub i i)))
    lst))

(fn M.list->string [lst]
  (table.concat lst ""))

(fn M.freq [lst]
  (let [mt (setmetatable {} {:__index #0})]
    (M.fold lst mt (fn [acc v]
                     (tset acc v (M.inc (. acc v)))
                     acc))))

(fn M.max-score [tbl proc]
  (default proc #$)
  (let [max (M.fold-table tbl nil (fn [acc k v]
                                    (if (= nil acc) {: k : v}
                                        (< (proc acc.v) (proc v)) {: k : v}
                                        acc)))]
    (when (not= nil max)
      (values max.k max.v))))

(fn M.max [tbl]
  (M.max-score tbl))

(fn M.min [tbl]
  (M.max-score tbl #(- $)))

(fn M.most [tbl]
  (M.max (M.freq tbl)))

(fn M.least [tbl]
  (M.min (M.freq tbl)))

(fn M.retain-if [proc lst]
  (M.filter #(not (proc $)) lst))

M

