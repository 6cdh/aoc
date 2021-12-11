(import-macros {: debug : printd} :macros)

(var M {})

(macro default [variable val]
  `(var ,variable (or ,variable ,val)))

(fn call [proc ...]
  (proc ...))

(fn pickn [n ...]
  "pick n-th argument"
  (. [...] n))

(fn M.readonly [tbl]
  (let [wrapper {}
        mt {:__index tbl
            :__newindex #(error "can't modify read only table" 2)}]
    (setmetatable wrapper mt)
    wrapper))

(var unpack (or _G.unpack table.unpack))

;; iterator

(fn M.iter-list [tbl]
  (fn next [tbl i]
    (when (< i (length tbl))
      (values (M.inc i) (. tbl (M.inc i)))))

  (values next tbl 0))

(fn M.iter-table [tbl]
  (values next tbl nil))

(let [mt {}]
  (fn M.range [beg end step]
    (default step 1)
    (setmetatable {: end : beg : step} mt))

  (fn M.range? [tbl]
    (and (= :table (type tbl))
         (= (getmetatable tbl) mt))))

(let [placeholder {}]
  (fn M.iter-range [state]
    (fn next [state k]
      (default k (- state.beg state.step))
      (let [next (+ k state.step)]
        (when (or (and (> state.step 0) (<= next state.end))
                  (and (< state.step 0) (>= next state.end)))
          (values next next))))

    (values next state nil))

  (fn M.iter-string [str]
    (fn next [str i]
      (when (< i (length str))
        (values (M.inc i) (str:sub (M.inc i) (M.inc i)))))

    (values next str 0))

  (fn M.iter-function [proc]
    (fn next [proc]
      (let [next (proc)]
        (when next
          (values placeholder next))))

    (values next proc nil)))

(fn M.iter [obj]
  (match (type obj)
    :string (M.iter-string obj)
    (where _ (M.list? obj)) (M.iter-list obj)
    (where _ (M.range? obj)) (M.iter-range obj)
    :table (M.iter-table obj)
    :function (M.iter-function obj)))

(fn M.list? [tbl]
  (and (= :table (type tbl))
       (not= nil (. tbl 1))
       (= nil (. tbl 0))))

;; functions

(fn M.inc [n]
  (+ n 1))

(fn M.dec [n]
  (- n 1))

(fn M.tinc [tbl key val]
  (tset tbl key (+ val (. tbl key))))

(fn M.abs [n]
  (math.abs n))

(fn M.sign [n]
  (if (< n 0) -1
      (> n 0) 1
      0))

(fn M.car [lst]
  (. lst 1))

(fn M.cdr [lst]
  [(unpack lst 2 (length lst))])

(fn M.first [lst n from]
  (default from 1)
  [(unpack lst from (+ from (M.dec n)))])

(fn M.last [lst n]
  (default n 1)
  (let [len (length lst)]
    [(unpack lst (- len (M.dec n)) len)]))

(fn M.append [lst v]
  (table.insert lst v)
  lst)

(fn M.append-list [lst1 lst2]
  (M.for-each #(M.append lst1 $) lst2))

(fn M.length [lst from]
  (default from 1)
  (- (length lst) (- from 1)))

(fn M.windows [lst n]
  (fn windows! [lst n from result]
    (let [len (M.length lst from)]
      (if (< len n) result
          (do
            (M.append result (M.first lst n from))
            (windows! lst n (+ from 1) result)))))

  (windows! (M.make-list lst) n 1 []))

(fn M.make-list [obj proc]
  (default proc (fn [...]
                  ...))
  (icollect [_ v (M.iter obj)]
    (proc v)))

(fn M.make-table [obj proc]
  (default proc (fn [...]
                  ...))
  (collect [k v (M.iter obj)]
    (proc k v)))

(fn M.for-each [proc tbl]
  (each [_ v (M.iter tbl)]
    (proc v)))

(fn M.for-each-key [proc tbl]
  (each [k v (M.iter tbl)]
    (proc k v)))

(fn M.map [proc iterable]
  (M.make-list iterable (fn [v]
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
    (set acc (proc acc v)))
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

(fn M.split [str sep]
  (default sep "%s")
  (->> (string.format "([^%s]+)" sep)
       (string.gmatch str)
       (M.make-list)))

(fn M.chunks [n lst]
  (fn chunks-iter [n lst from result]
    (when (not= 0 (M.length lst from))
      (M.append result (M.first lst n from)))
    (if (< (M.length lst from) n) result
        (chunks-iter n lst (+ from n) result)))

  (chunks-iter n lst 1 []))

(fn M.any [proc iterable]
  (fn any-iter [it iterable k]
    (let [(next-k v) (it iterable k)]
      (if (= nil v) false
          (proc v) true
          (any-iter it iterable next-k))))

  (any-iter (M.iter iterable)))

(fn M.all [proc lst]
  (not (M.any #(not (proc $)) lst)))

(fn M.all= [v lst]
  (M.all #(= v $) lst))

(fn M.sum [lst]
  (M.fold lst 0 (fn [acc v]
                  (+ acc v))))

(fn M.flatten [lst level]
  (default level 1000000.0)

  (fn flatten-iter [result depth it lst k]
    (let [(next-k v) (it lst k)]
      (if (= nil v) result
          (= depth level) (flatten-iter (M.append result v) depth it lst next-k)
          (not= :table (type v)) (flatten-iter (M.append result v) depth it lst next-k)
          (do
            (flatten-iter result (M.inc depth) (M.iter v))
            (flatten-iter result depth it lst next-k)))))

  (flatten-iter [] 0 (M.iter lst)))

(fn M.replace-flat [lst t p]
  (fn replace-flat-iter [it lst k]
    (let [(next-k v) (it lst k)]
      (if (= nil v) nil
          (not= :table (type v)) (do
                                   (when (= v t)
                                     (tset lst next-k p))
                                   (replace-flat-iter it lst next-k))
          (do
            (replace-flat-iter (M.iter v))
            (replace-flat-iter it lst next-k)))))

  (replace-flat-iter (M.iter lst)))

(fn M.swap [lst i j]
  (let [t (. lst i)]
    (tset lst i (. lst j))
    (tset lst j t)))

(fn M.reverse! [lst from to]
  (var from from)
  (var to to)
  (while (< from to)
    (M.swap lst from to)
    (set from (M.inc from))
    (set to (M.dec to))))

(fn M.rotatel! [lst n i]
  (M.reverse! lst 1 i)
  (M.reverse! lst (M.inc i) n)
  (M.reverse! lst 1 n))


M

