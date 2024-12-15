package day15

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"fmt"
	"io"
)

type MoveRule struct {
	from vec.Vec2i
	to   vec.Vec2i
}

// Overall:
// immutable
// for each action, copy the current room, recursively move the robot and boxes,
// if failed, use the old room, otherwise use the new room as the next room.
// Move:
// Every position has a list of MoveRule, it defines how to move the box or the robot
// at this position given an action IN ORDER.
// For an action direction `dir` and a position `pos`, its move rules are one element
//  list [pos -> dir + pos] in part 1. For part 2, a wider box with position (left, right) has
// this move rules:
// if action is up or down: [left -> left + dir, right -> right + dir]
// if action is left: [left -> left + dir, right -> right + dir]
// if action is right: [right -> right + dir, left -> left + dir]

func Solve(in io.Reader, out io.Writer) {
	room, moves := parse(in)

	fmt.Fprintln(out, simulate(room, moves))
	fmt.Fprintln(out, simulate(scaleRoom(room), moves))
}

func simulate(room [][]byte, moves []vec.Vec2i) int {
	robotPos := utils.FindMatrixPos('@', room)
	for _, m := range moves {
		robotPos, room = tryMove(robotPos, m, room)
	}

	sum := 0
	for pos := range iter.MatrixIndex(room) {
		cell := utils.MatrixAt(room, pos)
		if cell == 'O' || cell == '[' {
			sum += 100*pos.X + pos.Y
		}
	}
	return sum
}

func parse(in io.Reader) ([][]byte, []vec.Vec2i) {
	isRoomSection := true
	room := [][]byte{}
	moves := []vec.Vec2i{}
	for line := range iter.ReadLines(in) {
		if len(line) == 0 {
			isRoomSection = false
		} else if isRoomSection {
			room = append(room, line)
		} else {
			for _, m := range line {
				moves = append(moves, parseDir(m))
			}
		}
	}
	return room, moves
}

func parseDir(b byte) vec.Vec2i {
	switch b {
	case '<':
		return vec.LEFT
	case '>':
		return vec.RIGHT
	case '^':
		return vec.UP
	case 'v':
		return vec.DOWN
	default:
		panic(b)
	}
}

func scaleRoom(room [][]byte) [][]byte {
	newRoom := [][]byte{}
	for _, row := range room {
		newRow := []byte{}
		for _, b := range row {
			switch b {
			case '#':
				newRow = append(newRow, '#', '#')
			case 'O':
				newRow = append(newRow, '[', ']')
			case '.':
				newRow = append(newRow, '.', '.')
			case '@':
				newRow = append(newRow, '@', '.')
			default:
				panic(b)
			}
		}
		newRoom = append(newRoom, newRow)
	}
	return newRoom
}

func moveRules(pos vec.Vec2i, action vec.Vec2i, room [][]byte) []MoveRule {
	switch utils.MatrixAt(room, pos) {
	case '[', ']':
		return wideBoxMoveRules(pos, action, room)
	default:
		return []MoveRule{{pos, pos.Add(action)}}
	}
}

func wideBoxMoveRules(pos vec.Vec2i, action vec.Vec2i, room [][]byte) []MoveRule {
	left, right := wideBoxPos(pos, room)
	if action == vec.RIGHT {
		return []MoveRule{{right, right.Add(action)}, {left, left.Add(action)}}
	}
	return []MoveRule{{left, left.Add(action)}, {right, right.Add(action)}}
}

func wideBoxPos(pos vec.Vec2i, room [][]byte) (vec.Vec2i, vec.Vec2i) {
	switch utils.MatrixAt(room, pos) {
	case '[':
		return pos, pos.Add(vec.RIGHT)
	case ']':
		return pos.Add(vec.LEFT), pos
	default:
		panic(pos)
	}
}

func tryMove(robotPos vec.Vec2i, action vec.Vec2i, room [][]byte) (vec.Vec2i, [][]byte) {
	newRoom := utils.CopyMatrix(room)
	newRobotPos, ok := tryMoveRec(robotPos, action, newRoom)
	if ok {
		return newRobotPos, newRoom
	}
	return robotPos, room
}

func tryMoveRec(pos vec.Vec2i, action vec.Vec2i, room [][]byte) (vec.Vec2i, bool) {
	mrs := moveRules(pos, action, room)
	for _, mr := range mrs {
		switch utils.MatrixAt(room, mr.to) {
		case '#':
			return pos, false
		case '.':
			moveTo(mr.from, mr.to, room)
		default:
			_, succ := tryMoveRec(mr.to, action, room)
			if !succ {
				return pos, false
			}
			moveTo(mr.from, mr.to, room)
		}
	}
	return mrs[0].to, true
}

func moveTo(from vec.Vec2i, to vec.Vec2i, room [][]byte) {
	b := room[to.X][to.Y]
	room[to.X][to.Y] = room[from.X][from.Y]
	room[from.X][from.Y] = b
}
