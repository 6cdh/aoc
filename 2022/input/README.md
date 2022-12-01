# Fetch Input

The fetch script is written in Julia to get input.

```bash
# put your `Cookie` into file `cookie`
echo "your cookie" > cookie
# fetch the day 1 input and writes to `day01.txt`
julia --project=. fetch.jl 1
```

