package day17

import (
	"aoc2024/utils"
	"slices"
)

// A general solver which does not have too much assumptions about the input

func part2(c Computer) int {
	rc := BranchComputer{
		program: c.program,
		st: State{
			a: c.a,
			b: c.b,
			c: c.c,
		},
	}
	minA := -1
	for a := range 8 {
		rc2 := rc
		rc2.st.a = a
		for _, ra := range rc2.run() {
			rc2.st.a = ra
			if validSolution(rc2) && rc2.st.a > 0 && (minA == -1 || ra < minA) {
				minA = rc2.st.a
			}
		}
	}
	return minA
}

type State struct {
	a       int
	b       int
	c       int
	pointer int
	output  int
}

type BranchComputer struct {
	program []int
	st      State
}

var cache = map[State][]int{}

func (c BranchComputer) run() []int {
	if !utils.MapContains(cache, c.st) {
		cache[c.st] = c.run1()
	}
	return cache[c.st]
}

func (c BranchComputer) run1() []int {
	if c.st.pointer == len(c.program) {
		if c.st.output == len(c.program) {
			return []int{c.st.a}
		}
		return []int{}
	}
	switch c.program[c.st.pointer] {
	case 0:
		return c.adv()
	case 1:
		return c.bxl()
	case 2:
		return c.bst()
	case 3:
		return c.jnz()
	case 4:
		return c.bxc()
	case 5:
		return c.out()
	case 6:
		return c.bdv()
	case 7:
		return c.cdv()
	default:
		panic(c)
	}
}

func (c BranchComputer) literal() int {
	return c.program[c.st.pointer+1]
}

func (c BranchComputer) combo() int {
	switch v := c.program[c.st.pointer+1]; v {
	case 0, 1, 2, 3:
		return v
	case 4:
		return c.st.a
	case 5:
		return c.st.b
	case 6:
		return c.st.c
	default:
		panic(v)
	}
}

func validSolution(rc BranchComputer) bool {
	c := Computer{
		a:       rc.st.a,
		b:       rc.st.b,
		c:       rc.st.c,
		pointer: rc.st.pointer,
		program: rc.program,
		output:  slices.Clone(rc.program[:rc.st.output]),
	}
	c.run()
	return slices.Equal(c.output, c.program)
}

func (rc BranchComputer) adv() []int {
	as := []int{}
	for bits := range 8 {
		rc2 := rc
		rc2.st.a = bits
		rc2.st.pointer += 2
		for _, ra := range rc2.run() {
			rc3 := rc
			// combo() is expected to not a value from register A
			rc3.st.a = (ra << rc3.combo()) | rc.st.a
			if validSolution(rc3) {
				as = append(as, rc3.st.a)
			}
		}
	}
	return as
}

func (c BranchComputer) bxl() []int {
	c.st.b = c.st.b ^ c.literal()
	c.st.pointer += 2
	return c.run()
}

func (c BranchComputer) bst() []int {
	c.st.b = c.combo() % 8
	c.st.pointer += 2
	return c.run()
}

func (rc BranchComputer) jnz() []int {
	if rc.st.a != 0 {
		rc2 := rc
		rc2.st.pointer = rc2.literal()
		return rc2.run()
	}
	as := []int{}
	{
		rc2 := rc
		rc2.st.pointer += 2
		for _, ra := range rc2.run() {
			rc3 := rc
			rc3.st.a = ra
			if ra == 0 && validSolution(rc3) {
				as = append(as, rc3.st.a)
			}
		}
	}
	{
		rc2 := rc
		rc2.st.pointer = rc2.literal()
		for _, ra := range rc2.run() {
			rc3 := rc
			rc3.st.a = ra
			if ra != 0 && validSolution(rc3) {
				as = append(as, rc3.st.a)
			}
		}
	}
	return as
}

func (c BranchComputer) bxc() []int {
	c.st.b = c.st.b ^ c.st.c
	c.st.pointer += 2
	return c.run()
}

func (rc BranchComputer) out() []int {
	cur := rc.combo() % 8
	if !(rc.st.output < len(rc.program)) || cur != rc.program[rc.st.output] {
		return []int{}
	}
	rc.st.output += 1
	rc.st.pointer += 2
	return rc.run()
}

func (rc BranchComputer) bdv() []int {
	as := []int{}
	for bits := range 8 {
		rc2 := rc
		rc2.st.b = bits
		rc2.st.pointer += 2
		for _, ra := range rc2.run() {
			rc3 := rc
			rc3.st.a = ra
			if validSolution(rc3) {
				as = append(as, rc3.st.a)
			}
		}
	}
	return as
}

func (rc BranchComputer) cdv() []int {
	as := []int{}
	for bits := range 8 {
		rc2 := rc
		rc2.st.c = bits
		rc2.st.pointer += 2
		for _, ra := range rc2.run() {
			rc3 := rc
			rc3.st.a = ra
			if validSolution(rc3) {
				as = append(as, rc3.st.a)
			}
		}
	}
	return as
}
