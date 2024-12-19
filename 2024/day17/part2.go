package day17

import (
	"aoc2024/utils"
	"slices"
)

// A general solver which does not have too much assumptions about the input

func part2(c Computer) int {
	bc := BranchComputer{
		program: c.program,
		st: State{
			a: c.a,
			b: c.b,
			c: c.c,
		},
	}
	minA := -1
	for a := range 8 {
		bc2 := bc
		bc2.st.a = a
		for _, ra := range bc2.run() {
			bc2.st.a = ra
			if validSolution(bc2) && bc2.st.a > 0 && (minA == -1 || ra < minA) {
				minA = bc2.st.a
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

func (bc BranchComputer) run() []int {
	if !utils.MapContains(cache, bc.st) {
		cache[bc.st] = bc.run1()
	}
	return cache[bc.st]
}

func (bc BranchComputer) run1() []int {
	if bc.st.pointer == len(bc.program) {
		if bc.st.output == len(bc.program) {
			return []int{bc.st.a}
		}
		return []int{}
	}
	switch bc.program[bc.st.pointer] {
	case 0:
		return bc.adv()
	case 1:
		return bc.bxl()
	case 2:
		return bc.bst()
	case 3:
		return bc.jnz()
	case 4:
		return bc.bxc()
	case 5:
		return bc.out()
	case 6:
		return bc.bdv()
	case 7:
		return bc.cdv()
	default:
		panic(bc)
	}
}

func (bc BranchComputer) literal() int {
	return bc.program[bc.st.pointer+1]
}

func (bc BranchComputer) combo() int {
	switch v := bc.program[bc.st.pointer+1]; v {
	case 0, 1, 2, 3:
		return v
	case 4:
		return bc.st.a
	case 5:
		return bc.st.b
	case 6:
		return bc.st.c
	default:
		panic(v)
	}
}

func validSolution(bc BranchComputer) bool {
	c := Computer{
		a:       bc.st.a,
		b:       bc.st.b,
		c:       bc.st.c,
		pointer: bc.st.pointer,
		program: bc.program,
		output:  slices.Clone(bc.program[:bc.st.output]),
	}
	c.run()
	return slices.Equal(c.output, c.program)
}

func (bc BranchComputer) adv() []int {
	as := []int{}
	for bits := range 8 {
		bc2 := bc
		bc2.st.a = bits
		bc2.st.pointer += 2
		for _, ra := range bc2.run() {
			bc3 := bc
			bc3.st.a = (ra << bc3.combo()) | bc.st.a
			if validSolution(bc3) {
				as = append(as, bc3.st.a)
			}
		}
	}
	return as
}

func (bc BranchComputer) bxl() []int {
	bc.st.b = bc.st.b ^ bc.literal()
	bc.st.pointer += 2
	return bc.run()
}

func (bc BranchComputer) bst() []int {
	bc.st.b = bc.combo() % 8
	bc.st.pointer += 2
	return bc.run()
}

func (bc BranchComputer) jnz() []int {
	if bc.st.a != 0 {
		bc2 := bc
		bc2.st.pointer = bc2.literal()
		return bc2.run()
	}
	as := []int{}
	{
		bc2 := bc
		bc2.st.pointer += 2
		for _, ra := range bc2.run() {
			bc3 := bc
			bc3.st.a = ra
			if ra == 0 && validSolution(bc3) {
				as = append(as, bc3.st.a)
			}
		}
	}
	{
		bc2 := bc
		bc2.st.pointer = bc2.literal()
		for _, ra := range bc2.run() {
			bc3 := bc
			bc3.st.a = ra
			if ra != 0 && validSolution(bc3) {
				as = append(as, bc3.st.a)
			}
		}
	}
	return as
}

func (bc BranchComputer) bxc() []int {
	bc.st.b = bc.st.b ^ bc.st.c
	bc.st.pointer += 2
	return bc.run()
}

func (bc BranchComputer) out() []int {
	cur := bc.combo() % 8
	if !(bc.st.output < len(bc.program)) || cur != bc.program[bc.st.output] {
		return []int{}
	}
	bc.st.output += 1
	bc.st.pointer += 2
	return bc.run()
}

func (bc BranchComputer) bdv() []int {
	as := []int{}
	for bits := range 8 {
		bc2 := bc
		bc2.st.b = bits
		bc2.st.pointer += 2
		for _, ra := range bc2.run() {
			bc3 := bc
			bc3.st.a = ra
			if validSolution(bc3) {
				as = append(as, bc3.st.a)
			}
		}
	}
	return as
}

func (bc BranchComputer) cdv() []int {
	as := []int{}
	for bits := range 8 {
		bc2 := bc
		bc2.st.c = bits
		bc2.st.pointer += 2
		for _, ra := range bc2.run() {
			bc3 := bc
			bc3.st.a = ra
			if validSolution(bc3) {
				as = append(as, bc3.st.a)
			}
		}
	}
	return as
}
