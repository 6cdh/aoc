package day20

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"fmt"
	"io"
)

type Pos = vec.Vec2i
type DNode = utils.WeightNode[Pos]

var fourDirs = []vec.Vec2i{vec.UP, vec.DOWN, vec.LEFT, vec.RIGHT}

func Solve(in io.Reader, out io.Writer) {
	grid := iter.ReadLines(in).Collect()

	neighbors := func(dnode *DNode) []*DNode {
		dnodes := []*DNode{}
		for _, dir := range fourDirs {
			nextPos := dnode.Node.Add(dir)
			if vec.IsValidPos(nextPos, grid) && utils.MatrixAt(grid, nextPos) != '#' {
				dnodes = append(dnodes, &DNode{Node: nextPos, Dist: dnode.Dist + 1})
			}
		}
		return dnodes
	}

	start := utils.FindMatrixPos('S', grid)
	end := utils.FindMatrixPos('E', grid)
	distToStart, _ := utils.Dijkstra(start, end, neighbors)
	distToEnd, _ := utils.Dijkstra(end, start, neighbors)
	noCheatDist := distToStart[end]
	fmt.Fprintln(out, countCheats(2, 100, noCheatDist, distToStart, distToEnd, grid))
	fmt.Fprintln(out, countCheats(20, 100, noCheatDist, distToStart, distToEnd, grid))
}

func countCheats(cheatLen int, atLeastSave int, noCheatDist int, distToStart map[Pos]int, distToEnd map[Pos]int, grid [][]byte) int {
	cheatCnt := 0
	for cheatStart := range iter.MatrixIndex(grid) {
		for dx := -cheatLen; dx <= cheatLen; dx++ {
			dyLimit := cheatLen - utils.Abs(dx)
			for dy := -dyLimit; dy <= dyLimit; dy++ {
				cheatEnd := cheatStart.Add(vec.NewVec2i(dx, dy))
				if vec.IsValidPos(cheatEnd, grid) &&
					utils.MatrixAt(grid, cheatStart) != '#' &&
					utils.MatrixAt(grid, cheatEnd) != '#' {
					dist := distToStart[cheatStart] + utils.Abs(dx) + utils.Abs(dy) + distToEnd[cheatEnd]
					if dist+atLeastSave <= noCheatDist {
						cheatCnt += 1
					}
				}
			}
		}
	}
	return cheatCnt
}
