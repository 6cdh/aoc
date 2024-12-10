package day10

import (
	"aoc2024/iter"
	"aoc2024/vec"
	"fmt"
	"io"
)

func Solve(in io.Reader, out io.Writer) {
	grid := iter.ReadLines(in).Collect()
	sum := 0
	sum2 := 0
	for pos := range iter.MatrixIndex(grid) {
		sum += trailHeadScore(pos, true, grid)
		sum2 += trailHeadScore(pos, false, grid)
	}
	fmt.Fprintln(out, sum)
	fmt.Fprintln(out, sum2)
}

func trailHeadScore(pos vec.Vec2i, ifMem bool, grid [][]byte) int {
	visited := map[vec.Vec2i]bool{}
	if grid[pos.X][pos.Y] == '0' {
		return findPath(pos, visited, ifMem, grid)
	}
	return 0
}

func findPath(pos vec.Vec2i, visited map[vec.Vec2i]bool, ifMem bool, grid [][]byte) int {
	if ifMem {
		if visited[pos] {
			return 0
		}
		visited[pos] = true
	}

	if grid[pos.X][pos.Y] == '9' {
		return 1
	}
	score := 0
	for _, d := range []vec.Vec2i{vec.UP, vec.DOWN, vec.LEFT, vec.RIGHT} {
		newPos := pos.Add(d)
		if newPos.InRectangle(vec.NewVec2i(0, 0), vec.NewVec2i(len(grid), len(grid[0]))) &&
			grid[newPos.X][newPos.Y] == grid[pos.X][pos.Y]+1 {
			score += findPath(newPos, visited, ifMem, grid)
		}
	}
	return score
}
