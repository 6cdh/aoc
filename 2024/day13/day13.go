package day13

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"fmt"
	"io"
	"regexp"
)

type Machine struct {
	a     vec.Vec2i
	b     vec.Vec2i
	prize vec.Vec2i
}

func Solve(in io.Reader, out io.Writer) {
	machines := parse(in)

	fmt.Fprintln(out, howManyPress(machines))

	delta := 10_000_000_000_000
	machines2 := iter.Map(iter.SliceValues(machines), func(m *Machine) *Machine {
		return &Machine{
			a:     m.a,
			b:     m.b,
			prize: m.prize.Add(vec.NewVec2i(delta, delta)),
		}
	}).Collect()
	fmt.Fprintln(out, howManyPress(machines2))
}

func parse(in io.Reader) []*Machine {
	lines := iter.ReadStringLines(in).Collect()
	machines := []*Machine{}
	for i := 0; i < len(lines); i += 4 {
		re := regexp.MustCompile(`Button A: X\+(\d+), Y\+(\d+)`)
		mat := re.FindAllStringSubmatch(lines[i], -1)
		a := vec.NewVec2i(utils.StrToInt(mat[0][1]), utils.StrToInt(mat[0][2]))

		re2 := regexp.MustCompile(`Button B: X\+(\d+), Y\+(\d+)`)
		mat2 := re2.FindAllStringSubmatch(lines[i+1], -1)
		b := vec.NewVec2i(utils.StrToInt(mat2[0][1]), utils.StrToInt(mat2[0][2]))

		re3 := regexp.MustCompile(`Prize: X=(\d+), Y=(\d+)`)
		mat3 := re3.FindAllStringSubmatch(lines[i+2], -1)
		prize := vec.NewVec2i(utils.StrToInt(mat3[0][1]), utils.StrToInt(mat3[0][2]))

		machines = append(machines, &Machine{
			a:     a,
			b:     b,
			prize: prize,
		})
	}
	return machines
}

func howManyPress(machines []*Machine) int {
	sum := 0
	for _, m := range machines {
		ans, err := solveEquations(m)
		if err == nil {
			sum += ans
		}
	}
	return sum
}

func solveEquations(m *Machine) (int, error) {
	x := m.prize.X
	y := m.prize.Y
	xa := m.a.X
	ya := m.a.Y
	xb := m.b.X
	yb := m.b.Y

	// pa * xa + pb * xb = x
	// pa * ya + pb * yb = y
	pb := (x*(xa+ya) - xa*(x+y)) / (xb*(xa+ya) - xa*(xb+yb))
	pa := (x - pb*xb) / xa
	if pa*xa+pb*xb == x && pa*ya+pb*yb == y {
		return 3*pa + pb, nil
	}
	return 0, fmt.Errorf("no solution")
}
