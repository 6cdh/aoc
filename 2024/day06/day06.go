package day06

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"fmt"
	"io"
)

type VisitedPos map[vec.Vec2i]map[vec.Vec2i]bool

func Solve(in io.Reader, out io.Writer) {
	grid := iter.ReadLines(in).Collect()
	guardPos := findCharPos(grid, '^')
	visitedPos, _ := guardMove(grid, guardPos, moveStep)
	fmt.Fprintln(out, len(visitedPos))
	fmt.Fprintln(out, concurrentPart2(grid, guardPos, visitedPos))
}

// Original part2 implementation
func part2(grid [][]byte, guardPos vec.Vec2i, visitedPos VisitedPos) int {
	// New obstruction position should be one of the visited positions that
	// we got in part 1.
	return iter.MapIter(visitedPos).CountIf(willLoop(grid, guardPos))
}

func concurrentPart2(grid [][]byte, guardPos vec.Vec2i, visitedPos VisitedPos) int {
	// force run so that we don't have concurrent map write later
	for i := range grid {
		for j := range grid[i] {
			for _, dir := range []vec.Vec2i{vec.UP, vec.DOWN, vec.LEFT, vec.RIGHT} {
				nextPosBeforeObs(vec.NewVec2i(i, j), dir, grid)
			}
		}
	}

	return iter.MapIter(visitedPos).CountIfParallel(willLoop(grid, guardPos))
}

func willLoop(grid [][]byte, guardPos vec.Vec2i) func(vec.Vec2i) bool {
	return func(obsPos vec.Vec2i) bool {
		if obsPos != guardPos {
			_, canLeave := guardMove(grid, guardPos, moveTeleport(obsPos))
			if !canLeave {
				return true
			}
		}
		return false
	}
}

type moveFunc = func(pos vec.Vec2i, dir vec.Vec2i, grid [][]byte) vec.Vec2i

// Return the next position when move from `pos` in direction `dir`
func moveStep(pos vec.Vec2i, dir vec.Vec2i, grid [][]byte) vec.Vec2i {
	return pos.Add(dir)
}

// Return the position when keep moving from `pos` in direction `dir`, stopping before any obstruction.
func moveTeleport(obs vec.Vec2i) moveFunc {
	return func(pos vec.Vec2i, dir vec.Vec2i, grid [][]byte) vec.Vec2i {
		nextPos := nextPosBeforeObs(pos, dir, grid)
		// If the path from `pos` to `nextPos` needs to cross the new obstruction `obs`,
		// use the position before `obs` as the next position.
		toObs := obs.Sub(pos)
		ToNext := nextPos.Sub(pos)
		if toObs.Sign() == ToNext.Sign() && ToNext.UnitLen() >= toObs.UnitLen() {
			return pos.Add(toObs.Sign().MulI(max(0, utils.RoundToInt(toObs.UnitLen())-1)))
		}
		return nextPos
	}
}

var jumpCache = map[vec.Vec2i]map[vec.Vec2i]vec.Vec2i{}

func nextPosBeforeObs(pos vec.Vec2i, dir vec.Vec2i, grid [][]byte) vec.Vec2i {
	val, hit := jumpCache[pos][dir]
	if hit {
		return val
	}

	nextPos := pos.Add(dir)
	var res vec.Vec2i
	if !isValidPos(nextPos, grid) {
		res = nextPos
	} else if grid[nextPos.X][nextPos.Y] == '#' {
		res = pos
	} else {
		res = nextPosBeforeObs(nextPos, dir, grid)
	}

	if jumpCache[pos] == nil {
		jumpCache[pos] = map[vec.Vec2i]vec.Vec2i{}
	}
	jumpCache[pos][dir] = res
	return res
}

func guardMove(grid [][]byte, pos vec.Vec2i, moveFn moveFunc) (VisitedPos, bool) {
	visited := VisitedPos{}
	dir := vec.UP
	for {
		if visited[pos][dir] {
			return visited, false
		}
		if visited[pos] == nil {
			visited[pos] = map[vec.Vec2i]bool{}
		}
		visited[pos][dir] = true

		nextPos := moveFn(pos, dir, grid)
		if !isValidPos(nextPos, grid) {
			return visited, true
		}
		if nextPos == pos || grid[nextPos.X][nextPos.Y] == '#' {
			dir = dir.RotateRight()
		} else {
			pos = nextPos
		}
	}
}

func isValidPos(pos vec.Vec2i, grid [][]byte) bool {
	return 0 <= pos.X && pos.X < len(grid) &&
		0 <= pos.Y && pos.Y < len(grid[pos.X])
}

func findCharPos(grid [][]byte, ch byte) vec.Vec2i {
	for p := range iter.MatrixIndex(grid) {
		if grid[p.Fst][p.Snd] == ch {
			return vec.NewVec2i(p.Fst, p.Snd)
		}
	}
	return vec.NewVec2i(-1, -1)
}
