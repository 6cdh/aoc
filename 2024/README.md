# AOC 2024

The solutions are written in Go and run via the `aoc2024` executable.

## Usage

### Build

```shell
go build
# use `aoc` as alias of `./aoc2024`
alias aoc=./aoc2024
```

### Run

`aoc2024` will ask for your cookie to fetch your input the first time it runs, then cache them.

```shell
# run today
aoc run
# run day 1
aoc run 1
# run all
aoc run all
```

### Run using stdio

```shell
# run day 1, read input from stdin, and output to stdout
aoc test 1
```

### Submit

```shell
# run and submit for today
aoc submit
# run and sumbit for day 1
aoc submit 1
```

## Permissions

`aoc2024` requires these permissions:

### Network

|      Website       | Read Content | Submit Answer |          Rate           |
|:------------------:|:------------:|:-------------:|:-----------------------:|
| `adventofcode.com` |      ✔️      |      ✔️       | at most 2 for each call |

### FileSystem

|    Path     |   Type    | Read | Write |     For      |
|:-----------:|:---------:|:----:|:-----:|:------------:|
| `./.cookie` |   File    |  ✔️  |  ✔️   | store cookie |
|  `./input`  | Directory |  ✔️  |  ✔️   | store input  |
