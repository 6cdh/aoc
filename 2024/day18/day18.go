package day18

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"fmt"
	"io"
)

const size int = 70

type Pos = vec.Vec2i

func Solve(in io.Reader, out io.Writer) {
	poses := iter.Map(iter.ReadStringLines(in), func(line string) Pos {
		ints := utils.ReadInts(line, ",")
		return vec.NewVec2i(ints[0], ints[1])
	}).Collect()

	fmt.Fprintln(out, part1(poses, 1024))
	firstPos := part2(poses)
	fmt.Fprintf(out, "%d,%d\n", firstPos.X, firstPos.Y)
}

func part1(poses []Pos, firstBytes int) int {
	grid := utils.NewMatrix(size+1, size+1, '.')
	for _, pos := range poses[:firstBytes] {
		grid[pos.Y][pos.X] = '#'
	}

	edgesof := func(wNode *utils.WeightNode[Pos]) []*utils.WeightNode[Pos] {
		neighbors := []*utils.WeightNode[Pos]{}
		for _, dir := range []Pos{vec.UP, vec.DOWN, vec.LEFT, vec.RIGHT} {
			nextPos := wNode.Node.Add(dir)
			if vec.IsValidPos(nextPos, grid) && utils.MatrixAt(grid, nextPos) != '#' {
				nextWNode := &utils.WeightNode[Pos]{Node: nextPos, Dist: wNode.Dist + 1}
				neighbors = append(neighbors, nextWNode)
			}
		}
		return neighbors
	}

	start := vec.NewVec2i(0, 0)
	end := vec.NewVec2i(size, size)
	dist, _ := utils.Dijkstra(start, end, edgesof)
	if !utils.MapContains(dist, end) {
		return -1
	}
	return dist[end]
}

func part2(poses []Pos) Pos {
	s := 0
	e := len(poses)
	for s != e {
		m := (s + e) / 2
		if part1(poses, m) == -1 {
			e = m
		} else {
			s = m + 1
		}
	}
	return poses[s-1]
}
