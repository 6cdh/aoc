package main

import (
	"aoc2024/aoc"
	"aoc2024/day01"
	"aoc2024/day02"
	"aoc2024/day03"
	"aoc2024/log"
	"io"
)

func main() {
	defer log.Sync()
	a := aoc.New(run, 3)
	a.ParseAndRun()
}

func run(id int, in io.Reader, out io.Writer) {
	switch id {
	case 1:
		day01.Solve(in, out)
	case 2:
		day02.Solve(in, out)
	case 3:
		day03.Solve(in, out)
	default:
		log.Fatalf("id %d is out of range", id)
	}
}
