package day04

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"fmt"
	"io"
)

type Dir struct {
	di int
	dj int
}

var (
	right     = Dir{di: 0, dj: 1}
	down      = Dir{di: 1, dj: 0}
	leftDown  = Dir{di: 1, dj: -1}
	rightDown = Dir{di: 1, dj: 1}
)

func Solve(in io.Reader, out io.Writer) {
	grid := iter.ReadStringLines(in).Collect()
	fmt.Fprintln(out, part1(grid))
	fmt.Fprintln(out, part2(grid))
}

func part1(grid []string) int {
	cnt := 0
	pat := "XMAS"
	for i := range grid {
		for j := range grid[i] {
			for _, dir := range []Dir{right, down, leftDown, rightDown} {
				if isPatternAnyOrder(grid, i, j, dir, pat) {
					cnt++
				}
			}
		}
	}
	return cnt
}

func part2(grid []string) int {
	cnt := 0
	pat := "MAS"
	for i := range grid {
		for j := range grid[i] {
			if isPatternAnyOrder(grid, i, j, rightDown, pat) &&
				isPatternAnyOrder(grid, i, j+2, leftDown, pat) {
				cnt++
			}
		}
	}
	return cnt
}

func isChar(grid []string, i int, j int, char byte) bool {
	return 0 <= i && i < len(grid) &&
		0 <= j && j < len(grid[i]) &&
		grid[i][j] == char
}

func isPatternInOrder(grid []string, i int, j int, dir Dir, pat string) bool {
	for k, ch := range pat {
		if !isChar(grid, i+dir.di*k, j+dir.dj*k, byte(ch)) {
			return false
		}
	}
	return true
}

func isPatternAnyOrder(grid []string, i int, j int, dir Dir, pat string) bool {
	revPat := utils.StringReverse(pat)
	return isPatternInOrder(grid, i, j, dir, pat) || isPatternInOrder(grid, i, j, dir, revPat)
}
