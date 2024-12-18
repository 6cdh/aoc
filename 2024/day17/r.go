package day17

import (
	"slices"
)

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

var dp = map[State]int{}

func (c RComputer) run() int {
	if c.st.pointer == len(c.program) {
		if c.st.output == len(c.program) {
			return c.st.a
		}
		return -1
	}
	val, ok := dp[c.st]
	if ok {
		return val
	}
	a := 0
	switch c.program[c.st.pointer] {
	case 0:
		a = c.adv()
	case 1:
		a = c.bxl()
	case 2:
		a = c.bst()
	case 3:
		a = c.jnz()
	case 4:
		a = c.bxc()
	case 5:
		a = c.out()
	case 6:
		a = c.bdv()
	case 7:
		a = c.cdv()
	default:
		panic(c)
	}
	dp[c.st] = a
	return a
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
	if rc.program[rc.st.pointer+1] == 4 {
		panic(rc)
	}
	mina := -1
	for newa := range 8 {
		rc1 := rc
		rc1.st.a = newa
		rc1.st.pointer += 2
		ra := rc1.run()
		if ra != -1 {
			rc2 := rc
			rc2.st.a = (ra << rc2.combo()) | rc.st.a
			if validSol(rc2) && (mina == -1 || rc2.st.a < mina) {
				mina = rc2.st.a
			}
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
		if a1 != -1 && validSol(rc2) {
			return 0
		}

		rc3 := rc
		rc3.st.pointer = rc3.literal()
		a2 := rc3.run()
		rc4 := rc
		rc4.st.a = a2
		if a2 != -1 && validSol(rc4) {
			return a2
		}
	}
	return -1
}

func (c RComputer) bxc() int {
	c.st.b = c.st.b ^ c.st.c
	c.st.pointer += 2
	return c.run()
}

func (rc RComputer) out() int {
	cur := rc.combo() % 8
	if !(rc.st.output < len(rc.program)) || cur != rc.program[rc.st.output] {
		return -1
	}
	rc.st.output += 1
	rc.st.pointer += 2
	return rc.run()
}

func (rc RComputer) bdv() int {
	if rc.program[rc.st.pointer+1] == 4 {
		panic(rc)
	}
	mina := -1
	for newb := range 8 {
		rc1 := rc
		rc1.st.a = (newb << rc1.combo()) | rc.st.a
		rc1.st.b = rc1.st.a >> rc1.combo()
		rc1.st.pointer += 2
		ra := rc1.run()
		if ra != -1 {
			rc2 := rc
			rc2.st.a = ra
			if validSol(rc2) && (mina == -1 || rc2.st.a < mina) {
				mina = rc2.st.a
			}
		}
	}
	return mina
}

func (rc RComputer) cdv() int {
	if rc.program[rc.st.pointer+1] == 4 {
		panic(rc)
	}
	mina := -1
	for newb := range 8 {
		rc1 := rc
		rc1.st.a = (newb << rc1.combo()) | rc.st.a
		rc1.st.c = rc1.st.a >> rc1.combo()
		rc1.st.pointer += 2
		ra := rc1.run()
		if ra != -1 {
			rc2 := rc
			rc2.st.a = ra
			if validSol(rc2) && (mina == -1 || rc2.st.a < mina) {
				mina = rc2.st.a
			}
		}
	}
	return mina
}
