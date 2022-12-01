# Run

The solutions for everyday will read input from stdin and output result to stdout.

For output, the $i$-th line ( $1≤i≤2$ ) contain the $i$-th result.

Racket and Julia both have heavy bootstrap time. The solution itself also output the time
the actual function spend.

## Racket

```bash
$ racket racket/day01.rkt < input/day01.txt
71300
209691
cpu time: 4 real time: 4 gc time: 0
```

## Julia

```bash
$ julia julia/day01.jl < ./input/day01.txt
71300
209691
  0.048664 seconds (20.97 k allocations: 1010.854 KiB, 98.84% compilation time)
```

