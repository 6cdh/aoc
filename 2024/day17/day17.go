package day17

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type Computer struct {
	a       int
	b       int
	c       int
	pointer int
	program []int
	output  []int
}

func Solve(in io.Reader, out io.Writer) {
	lines := iter.ReadStringLines(in).Collect()
	c := Computer{
		a:       parseRegister(lines[0]),
		b:       parseRegister(lines[1]),
		c:       parseRegister(lines[2]),
		pointer: 0,
		program: utils.ReadInts(strings.Split(lines[4], ": ")[1], ","),
	}
	fmt.Fprintln(out, part1(c))
	fmt.Fprintln(out, part2(c))
}

func part1(c Computer) string {
	c.output = []int{}
	c.run()
	output := strings.Join(iter.Map(iter.SliceValues(c.output), strconv.Itoa).Collect(), ",")
	return output
}

func part2(c Computer) int {
	rc := RComputer{
		program: c.program,
		st: State{
			a: c.a,
			b: c.b,
			c: c.c,
		},
	}
	mina := ErrNotFound
	for newa := range 8 {
		rc2 := rc
		rc2.st.a = newa
		ra := rc2.run()
		rc2.st.a = ra
		if ra != ErrNotFound && validSol(rc2) && (mina == ErrNotFound || ra < mina) {
			mina = ra
		}
	}
	return mina
}

func parseRegister(str string) int {
	return utils.StrToInt(strings.Split(str, ": ")[1])
}

func (c *Computer) run() {
	for c.pointer < len(c.program) {
		c.run1()
	}
}

func (c *Computer) run1() {
	switch c.program[c.pointer] {
	case 0:
		c.adv()
	case 1:
		c.bxl()
	case 2:
		c.bst()
	case 3:
		c.jnz()
	case 4:
		c.bxc()
	case 5:
		c.out()
	case 6:
		c.bdv()
	case 7:
		c.cdv()
	}
}

func (c *Computer) literal() int {
	return c.program[c.pointer+1]
}

func (c *Computer) combo() int {
	switch v := c.program[c.pointer+1]; v {
	case 0, 1, 2, 3:
		return v
	case 4:
		return c.a
	case 5:
		return c.b
	case 6:
		return c.c
	default:
		panic(v)
	}
}

func (c *Computer) adv() {
	c.a = c.a >> c.combo()
	c.pointer += 2
}

func (c *Computer) bxl() {
	c.b = c.b ^ c.literal()
	c.pointer += 2
}

func (c *Computer) bst() {
	c.b = c.combo() % 8
	c.pointer += 2
}

func (c *Computer) jnz() {
	if c.a != 0 {
		c.pointer = c.literal()
	} else {
		c.pointer += 2
	}
}

func (c *Computer) bxc() {
	c.b = c.b ^ c.c
	c.pointer += 2
}

func (c *Computer) out() {
	c.output = append(c.output, c.combo()%8)
	c.pointer += 2
}

func (c *Computer) bdv() {
	c.b = c.a >> c.combo()
	c.pointer += 2
}

func (c *Computer) cdv() {
	c.c = c.a >> c.combo()
	c.pointer += 2
}
