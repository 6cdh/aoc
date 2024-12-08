package main

import (
	"aoc2024/aoc"
	"aoc2024/day01"
	"aoc2024/day02"
	"aoc2024/day03"
	"aoc2024/day04"
	"aoc2024/day05"
	"aoc2024/day06"
	"aoc2024/day07"
	"aoc2024/day08"
	"aoc2024/log"
	"io"
)

type Solver = func(reader io.Reader, writer io.Writer)

var funcs = []Solver{
	day01.Solve,
	day02.Solve,
	day03.Solve,
	day04.Solve,
	day05.Solve,
	day06.Solve,
	day07.Solve,
	day08.Solve,
}

func main() {
	defer log.Sync()
	a := aoc.New(run, len(funcs))
	a.ParseAndRun()
}

func run(id int, in io.Reader, out io.Writer) {
	solve := funcs[id-1]
	solve(in, out)
}
