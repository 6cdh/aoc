package day17

import (
	"aoc2024/utils"
	"slices"
)

const ErrNotFound = -1

type State struct {
	a       int
	b       int
	c       int
	pointer int
	output  int
}

type RComputer struct {
	program []int
	st      State
}

var cache = map[State]int{}

func (c RComputer) run() int {
	if !utils.MapContains(cache, c.st) {
		cache[c.st] = c.run1()
	}
	return cache[c.st]
}

func (c RComputer) run1() int {
	if c.st.pointer == len(c.program) {
		if c.st.output == len(c.program) {
			return c.st.a
		}
		return ErrNotFound
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

func (c RComputer) literal() int {
	return c.program[c.st.pointer+1]
}

func (c RComputer) combo() int {
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

func validSol(rc RComputer) bool {
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

func (rc RComputer) adv() int {
	mina := ErrNotFound
	for bits := range 8 {
		rc2 := rc
		rc2.st.a = bits
		rc2.st.pointer += 2
		ra := rc2.run()
		rc3 := rc
		rc3.st.a = (ra << rc3.combo()) | rc.st.a
		if ra != ErrNotFound && validSol(rc3) && (mina == ErrNotFound || rc3.st.a < mina) {
			mina = rc3.st.a
		}
	}
	return mina
}

func (c RComputer) bxl() int {
	c.st.b = c.st.b ^ c.literal()
	c.st.pointer += 2
	return c.run()
}

func (c RComputer) bst() int {
	c.st.b = c.combo() % 8
	c.st.pointer += 2
	return c.run()
}

func (rc RComputer) jnz() int {
	if rc.st.a != 0 {
		rc1 := rc
		rc1.st.pointer = rc1.literal()
		return rc1.run()
	} else {
		rc1 := rc
		rc1.st.pointer += 2
		a1 := rc1.run()
		rc2 := rc
		rc2.st.a = a1
		if a1 != ErrNotFound && validSol(rc2) {
			return 0
		}

		rc3 := rc
		rc3.st.pointer = rc3.literal()
		a2 := rc3.run()
		rc4 := rc
		rc4.st.a = a2
		if a2 != ErrNotFound && validSol(rc4) {
			return a2
		}
	}
	return ErrNotFound
}

func (c RComputer) bxc() int {
	c.st.b = c.st.b ^ c.st.c
	c.st.pointer += 2
	return c.run()
}

func (rc RComputer) out() int {
	cur := rc.combo() % 8
	if !(rc.st.output < len(rc.program)) || cur != rc.program[rc.st.output] {
		return ErrNotFound
	}
	rc.st.output += 1
	rc.st.pointer += 2
	return rc.run()
}

func (rc RComputer) bdv() int {
	mina := ErrNotFound
	for bits := range 8 {
		rc2 := rc
		rc2.st.a = (bits << rc2.combo()) | rc.st.a
		rc2.st.b = rc2.st.a >> rc2.combo()
		rc2.st.pointer += 2
		ra := rc2.run()
		rc3 := rc
		rc3.st.a = ra
		if ra != ErrNotFound && validSol(rc3) && (mina == ErrNotFound || rc3.st.a < mina) {
			mina = rc3.st.a
		}
	}
	return mina
}

func (rc RComputer) cdv() int {
	mina := ErrNotFound
	for bits := range 8 {
		rc2 := rc
		rc2.st.a = (bits << rc2.combo()) | rc.st.a
		rc2.st.c = rc2.st.a >> rc2.combo()
		rc2.st.pointer += 2
		ra := rc2.run()
		rc3 := rc
		rc3.st.a = ra
		if ra != ErrNotFound && validSol(rc3) && (mina == ErrNotFound || rc3.st.a < mina) {
			mina = rc3.st.a
		}
	}
	return mina
}
