package day08

import (
	"aoc2024/iter"
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
	antiNodes := map[vec.Vec2i]bool{}
	antennas := iter.MatrixIndex(grid).Filter(func(pos vec.Vec2i) bool {
		return grid[pos.X][pos.Y] != '.'
	})
	for p := range iter.Pairs(antennas) {
		antenna1 := p.Fst
		antenna2 := p.Snd
		if grid[antenna1.X][antenna1.Y] == grid[antenna2.X][antenna2.Y] {
			addAntiNodes(antenna1, antenna2, m, n, antiNodes, countResonant)
		}
	}

	return len(antiNodes)
}

func addAntiNodes(antenna1 vec.Vec2i, antenna2 vec.Vec2i, m int, n int, antiNodes map[vec.Vec2i]bool, countResonant bool) {
	for _, signal := range [][]vec.Vec2i{{antenna1, antenna2}, {antenna2, antenna1}} {
		from := signal[0]
		to := signal[1]
		distVec := to.Minus(from)

		notValidPos := func(pos vec.Vec2i) bool {
			return !pos.InRectangle(vec.NewVec2i(0, 0), vec.NewVec2i(m, n))
		}

		var it []vec.Vec2i
		if countResonant {
			it = iter.PosIter(to, distVec, notValidPos).Collect()
		} else if !notValidPos(to.Add(distVec)) {
			it = append(it, to.Add(distVec))
		}

		for _, node := range it {
			antiNodes[node] = true
		}
	}
}
