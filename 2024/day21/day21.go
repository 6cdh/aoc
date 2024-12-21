package day21

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"fmt"
	"io"
	"math"
)

func Solve(in io.Reader, out io.Writer) {
	codes := iter.ReadLines(in).Collect()

	fmt.Fprintln(out, robotSimulate(3, codes))
	fmt.Fprintln(out, robotSimulate(26, codes))
}

func robotSimulate(me int, codes [][]byte) int {
	numericGrid := [][]byte{
		[]byte("789"),
		[]byte("456"),
		[]byte("123"),
		[]byte(" 0A"),
	}

	directGrid := [][]byte{
		[]byte(" ^A"),
		[]byte("<v>"),
	}

	sum := 0
	for _, code := range codes {
		cur := byte('A')
		seqLen := 0
		for _, c := range code {
			seqLen += press(cur, c, 0, numericGrid, directGrid, me)
			cur = c
		}
		sum += seqLen * numericCode(code)
	}
	return sum
}

type Stat struct {
	cur     byte
	toPress byte
	robot   int
	me      int
}

var dp = map[Stat]int{}

func pressDP(cur byte, toPress byte, robot int, keyboard [][]byte, nextKeyboard [][]byte, me int) int {
	stat := Stat{
		cur:     cur,
		toPress: toPress,
		robot:   robot,
		me:      me,
	}
	if !utils.MapContains(dp, stat) {
		dp[stat] = press(cur, toPress, robot, keyboard, nextKeyboard, me)
	}
	return dp[stat]
}

func press(cur byte, nextPress byte, robot int, keyboard [][]byte, nextKeyboard [][]byte, me int) int {
	if robot == me {
		return 1
	}

	curPos := utils.FindMatrixPos(cur, keyboard)
	nextPos := utils.FindMatrixPos(nextPress, keyboard)
	paths := findAllShortestPath(curPos, nextPos, keyboard)
	minSeq := math.MaxInt
	for path := range paths {
		seq := 0
		cur := byte('A')
		for _, c := range path {
			seq += pressDP(cur, c, robot+1, nextKeyboard, nextKeyboard, me)
			cur = c
		}
		seq += pressDP(cur, 'A', robot+1, nextKeyboard, nextKeyboard, me)
		minSeq = min(minSeq, seq)
	}
	return minSeq
}

func numericCode(code []byte) int {
	return utils.StrToInt(string(code[:len(code)-1]))
}

type Pos = vec.Vec2i
type Dir = vec.Vec2i

func findAllShortestPath(from Pos, to Pos, grid [][]byte) iter.Iter[[]byte] {
	edgesOf := func(pos Pos) []Pos {
		neighbors := []Pos{}
		for _, dir := range []Dir{vec.UP, vec.DOWN, vec.LEFT, vec.RIGHT} {
			nei := pos.Add(dir)
			if vec.IsValidPos(nei, grid) && utils.MatrixAt(grid, nei) != ' ' {
				neighbors = append(neighbors, nei)
			}
		}
		return neighbors
	}

	_, prev := utils.BFS(to, edgesOf)
	return allPathIter(from, to, prev)
}

func allPathIter(from Pos, to Pos, nextOf map[Pos][]Pos) iter.Iter[[]byte] {
	return func(yield func([]byte) bool) {
		allPathRecur(from, []byte{}, to, nextOf, yield)
	}
}

func allPathRecur(cur Pos, path []byte, to Pos, nextOf map[Pos][]Pos, yield func([]byte) bool) bool {
	if cur == to {
		return yield(path)
	}
	for _, next := range nextOf[cur] {
		if !allPathRecur(next, append(path, dirToChar(next.Minus(cur))), to, nextOf, yield) {
			return false
		}
	}
	return true
}

func dirToChar(dir Dir) byte {
	switch dir {
	case vec.UP:
		return '^'
	case vec.DOWN:
		return 'v'
	case vec.LEFT:
		return '<'
	case vec.RIGHT:
		return '>'
	default:
		panic(dir)
	}
}
