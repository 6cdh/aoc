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
	"aoc2024/day09"
	"aoc2024/day10"
	"aoc2024/day11"
	"aoc2024/day12"
	"aoc2024/day13"
	"aoc2024/day14"
	"aoc2024/day15"
	"aoc2024/day16"
	"aoc2024/day17"
	"aoc2024/day18"
	"aoc2024/day19"
	"aoc2024/day20"
	"aoc2024/day21"
	"aoc2024/day22"
	"aoc2024/day23"
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
	day09.Solve,
	day10.Solve,
	day11.Solve,
	day12.Solve,
	day13.Solve,
	day14.Solve,
	day15.Solve,
	day16.Solve,
	day17.Solve,
	day18.Solve,
	day19.Solve,
	day20.Solve,
	day21.Solve,
	day22.Solve,
	day23.Solve,
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
