package day04

import (
	"aoc2024/log"
	"aoc2024/utils"
	"fmt"
	"io"
)

const (
	horizontal = iota
	vertical
	diagonalL
	diagonalR
)

func Solve(in io.Reader, out io.Writer) {
	grid := []string{}
	for line := range utils.ReadStringLines(in) {
		grid = append(grid, line)
	}
	fmt.Fprintln(out, part1(grid))
	fmt.Fprintln(out, part2(grid))
}

func part1(grid []string) int {
	cnt := 0
	pat := "XMAS"
	for i := range grid {
		for j := range grid[i] {
			for _, dir := range []int{horizontal, vertical, diagonalL, diagonalR} {
				if isPattern(grid, i, j, dir, pat) {
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
			if isPattern(grid, i, j, diagonalL, pat) &&
				isPattern(grid, i, j+2, diagonalR, pat) {
				cnt++
			}
		}
	}
	return cnt
}

func isChar(grid []string, i int, j int, char byte) bool {
	return 0 <= i && i < len(grid) &&
		0 <= j && j < len(grid[0]) &&
		grid[i][j] == char
}

func isPatternDir(grid []string, i int, j int, di int, dj int, pat string) bool {
	for k, ch := range pat {
		if !isChar(grid, i+di*k, j+dj*k, byte(ch)) {
			return false
		}
	}
	return true
}

func isPatternBothDir(grid []string, i int, j int, di int, dj int, pat string) bool {
	revPat := utils.StringReverse(pat)
	return isPatternDir(grid, i, j, di, dj, pat) || isPatternDir(grid, i, j, di, dj, revPat)
}

func isPattern(grid []string, i int, j int, dir int, pat string) bool {
	switch dir {
	case horizontal:
		return isPatternBothDir(grid, i, j, 0, 1, pat)
	case vertical:
		return isPatternBothDir(grid, i, j, 1, 0, pat)
	case diagonalL:
		return isPatternBothDir(grid, i, j, 1, 1, pat)
	case diagonalR:
		return isPatternBothDir(grid, i, j, 1, -1, pat)
	default:
		log.Fatal("Invalid direction")
		return false
	}
}
