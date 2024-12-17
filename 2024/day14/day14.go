package day14

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"fmt"
	"io"
	"math"
	"regexp"
)

type Robot struct {
	p vec.Vec2i
	v vec.Vec2i
}

type RoomSize struct {
	width  int
	height int
}

var room = RoomSize{101, 103}

func Solve(in io.Reader, out io.Writer) {
	robots := []Robot{}
	re := regexp.MustCompile(`p=(\d+),(\d+) v=(-?\d+),(-?\d+)`)
	for line := range iter.ReadStringLines(in) {
		numStrs := re.FindAllStringSubmatch(line, -1)[0][1:]
		ints := iter.Map(iter.SliceValues(numStrs), utils.StrToInt).Collect()
		robots = append(robots,
			Robot{
				p: vec.NewVec2i(ints[0], ints[1]),
				v: vec.NewVec2i(ints[2], ints[3]),
			})
	}

	fmt.Fprintln(out, part1(robots))
	fmt.Fprintln(out, part2(robots))
}

func part1(robots []Robot) int {
	quadrants := [5]int{}
	for _, r := range robots {
		newPos := simulate(r, 100)
		quadrants[whichQuadrant(newPos)] += 1
	}
	return iter.Product(iter.SliceValues(quadrants[1:]))
}

func part2(robots []Robot) int {
	type Result struct {
		sec      int
		variance float64
	}

	n := 12000
	pool := utils.NewTaskPool[Result](n)

	for sec := range n {
		pool.AddTask(func() (Result, error) {
			hasRobot := utils.NewMatrix(room.width, room.height, false)
			for _, r := range robots {
				newPos := simulate(r, sec)
				hasRobot[newPos.X][newPos.Y] = true
			}
			return Result{sec, spatialVariance(hasRobot)}, nil
		})
	}

	pool.WaitAll()

	return iter.ArgMin(iter.ChannelIter(pool.Result()), math.MaxFloat64, func(v Result) float64 {
		return v.variance
	}).sec
}

func spatialVariance(hasRobot [][]bool) float64 {
	robots := iter.MatrixIndex(hasRobot).Filter(func(v vec.Vec2i) bool {
		return hasRobot[v.X][v.Y]
	})
	n := robots.Count()
	cx := iter.Sum(iter.Map(robots, func(p vec.Vec2i) int {
		return p.X
	})) / n
	cy := iter.Sum(iter.Map(robots, func(p vec.Vec2i) int {
		return p.Y
	})) / n
	c := vec.NewVec2i(cx, cy)
	variance := 0.0
	for r := range robots {
		variance += c.DistTo(r)
	}
	variance /= float64(n)
	return variance
}

func simulate(r Robot, secs int) vec.Vec2i {
	endPos := r.p.Add(r.v.MulI(secs))
	return wrapPos(endPos)
}

func wrapPos(p vec.Vec2i) vec.Vec2i {
	x := ((p.X % room.width) + room.width) % room.width
	y := ((p.Y % room.height) + room.height) % room.height
	return vec.NewVec2i(x, y)
}

func whichQuadrant(p vec.Vec2i) int {
	midX := room.width / 2
	midY := room.height / 2
	sign := p.Minus(vec.NewVec2i(midX, midY)).Sign()
	switch sign {
	case vec.NewVec2i(-1, -1):
		return 1
	case vec.NewVec2i(1, -1):
		return 2
	case vec.NewVec2i(-1, 1):
		return 3
	case vec.NewVec2i(1, 1):
		return 4
	default:
		return 0
	}
}
