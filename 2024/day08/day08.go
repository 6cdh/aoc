package day08

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"fmt"
	"io"
)

func Solve(in io.Reader, out io.Writer) {
	grid := iter.ReadLines(in).Collect()

	fmt.Fprintln(out, countAntiNodes(grid, false))
	fmt.Fprintln(out, countAntiNodes(grid, true))
}

func countAntiNodes(grid [][]byte, countResonant bool) int {
	m := len(grid)
	n := len(grid[0])
	antiNodes := utils.NewMatrix(m, n, false)
	antennas := iter.MatrixIndex(grid).Filter(func(pos vec.Vec2i) bool {
		return grid[pos.X][pos.Y] != '.'
	})
	for antenna1 := range antennas {
		for antenna2 := range antennas {
			if antenna1 != antenna2 && grid[antenna1.X][antenna1.Y] == grid[antenna2.X][antenna2.Y] {
				addAntiNodes(antenna1, antenna2, m, n, antiNodes, countResonant)
			}
		}
	}

	cnt := iter.MatrixIndex(antiNodes).CountIf(func(p vec.Vec2i) bool {
		return antiNodes[p.X][p.Y]
	})
	return cnt
}

func addAntiNodes(antenna1 vec.Vec2i, antenna2 vec.Vec2i, m int, n int, antiNodes [][]bool, countResonant bool) {
	for _, signal := range [][]vec.Vec2i{{antenna1, antenna2}, {antenna2, antenna1}} {
		from := signal[0]
		to := signal[1]
		distVec := to.Minus(from)
		var node vec.Vec2i
		if countResonant {
			node = to
		} else {
			node = to.Add(distVec)
		}
		for node.InRectangle(vec.NewVec2i(0, 0), vec.NewVec2i(m, n)) {
			antiNodes[node.X][node.Y] = true
			node = node.Add(distVec)
			if !countResonant {
				break
			}
		}
	}
}
