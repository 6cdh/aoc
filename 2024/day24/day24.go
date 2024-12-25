package day24

import (
	"aoc2024/iter"
	"aoc2024/log"
	"aoc2024/utils"
	"fmt"
	"io"
	"slices"
	"strings"
)

type Gate struct {
	in1 string
	in2 string
	op  string
	out string
}

func Solve(in io.Reader, out io.Writer) {
	gates, wires := parse(in)

	outToGate := utils.Map[string, *Gate]{}
	for _, gate := range gates {
		outToGate[gate.out] = &gate
	}

	fmt.Fprintln(out, part1(gates, wires, outToGate))
	fmt.Fprintln(out, part2(gates, outToGate))
}

func part1(gates []Gate, wires utils.Map[string, int], outToGate utils.Map[string, *Gate]) int {
	ans := 0
	for _, gate := range gates {
		if strings.HasPrefix(gate.out, "z") {
			digit := utils.StrToInt(gate.out[1:])
			ans = ans | (signal(gate.out, wires, outToGate) << digit)
		}
	}
	return ans
}

func part2(gates []Gate, outToGate utils.Map[string, *Gate]) string {
	swaps := []string{}

	inToXorGate := utils.Map[string, *Gate]{}
	inToAndGate := utils.Map[string, *Gate]{}
	inToOrGate := utils.Map[string, *Gate]{}
	for _, gate := range gates {
		switch gate.op {
		case "XOR":
			inToXorGate[gate.in1] = &gate
			inToXorGate[gate.in2] = &gate
		case "AND":
			inToAndGate[gate.in1] = &gate
			inToAndGate[gate.in2] = &gate
		case "OR":
			inToOrGate[gate.in1] = &gate
			inToOrGate[gate.in2] = &gate
		default:
			panic(gate.op)
		}
	}

	c, ss := halfAdder("x00", inToXorGate, inToAndGate, outToGate)
	for _, s := range ss {
		swaps = append(swaps, s.out1, s.out2)
	}

	for i := 1; i < 60; i++ {
		x := fmt.Sprintf("x%02d", i)
		if !inToXorGate.Contains(x) {
			break
		}
		c, ss = fullAdder(x, c, inToXorGate, inToAndGate, inToOrGate, outToGate)
		for _, s := range ss {
			swaps = append(swaps, s.out1, s.out2)
		}
	}

	slices.Sort(swaps)
	return strings.Join(swaps, ",")
}

func parse(in io.Reader) ([]Gate, utils.Map[string, int]) {
	gates := []Gate{}
	wires := utils.Map[string, int]{}
	isWire := true

	for line := range iter.ReadStringLines(in) {
		if line == "" {
			isWire = false
		} else if isWire {
			strs := strings.Split(line, ": ")
			wires[strs[0]] = utils.StrToInt(strs[1])
		} else {
			strs := strings.Split(line, " ")
			gates = append(gates, Gate{
				strs[0],
				strs[2],
				strs[1],
				strs[4],
			})
		}
	}

	return gates, wires
}

func signal(wire string, wires utils.Map[string, int], outToGate utils.Map[string, *Gate]) int {
	if wires.Contains(wire) {
		return wires[wire]
	}
	gate := outToGate[wire]
	in1 := signal(gate.in1, wires, outToGate)
	in2 := signal(gate.in2, wires, outToGate)
	var ans int
	switch gate.op {
	case "XOR":
		ans = in1 ^ in2
	case "AND":
		ans = in1 & in2
	case "OR":
		ans = in1 | in2
	default:
		panic(gate.op)
	}
	wires[wire] = ans
	return ans
}

type Swap struct {
	out1 string
	out2 string
}

func halfAdder(
	x string,
	inToXorGate utils.Map[string, *Gate],
	inToAndGate utils.Map[string, *Gate],
	outToGate utils.Map[string, *Gate],
) (*Gate, []Swap) {
	xorxy := inToXorGate[x]
	andxy := inToAndGate[x]
	if xorxy.out != "z00" {
		s := swapOut(xorxy, outToGate["z00"], outToGate)
		return andxy, []Swap{s}
	}
	return andxy, []Swap{}
}

func fullAdder(
	x string,
	c *Gate,
	inToXorGate utils.Map[string, *Gate],
	inToAndGate utils.Map[string, *Gate],
	inToOrGate utils.Map[string, *Gate],
	outToGate utils.Map[string, *Gate],
) (*Gate, []Swap) {
	xor1 := inToXorGate[x]
	and1 := inToAndGate[x]
	z := zOf(x)

	swaps := []Swap{}
	var xor2 *Gate
	if inToXorGate[xor1.out] == inToXorGate[c.out] {
		xor2 = inToXorGate[xor1.out]
	} else if inToXorGate[xor1.out] == nil {
		xor2 = inToXorGate[c.out]
		swaps = append(swaps, swapOut(xor1, outToGate[anotherIn(xor2, c.out)], outToGate))
	} else if inToXorGate[c.out] == nil {
		xor2 = inToXorGate[xor1.out]
		swaps = append(swaps, swapOut(c, outToGate[anotherIn(xor2, xor1.out)], outToGate))
	} else if inToXorGate[xor1.out].out == z {
		xor2 = inToXorGate[xor1.out]
		swaps = append(swaps, swapOut(c, outToGate[anotherIn(xor2, xor1.out)], outToGate))
	} else if inToXorGate[c.out].out == z {
		xor2 = inToXorGate[c.out]
		swaps = append(swaps, swapOut(xor1, outToGate[anotherIn(xor2, c.out)], outToGate))
	} else {
		panic(xor1)
	}

	if xor2.out != z {
		swaps = append(swaps, swapOut(xor2, outToGate[z], outToGate))
	}

	and2 := inToAndGate[c.out]
	var or1 *Gate
	if inToOrGate[and1.out] == inToOrGate[and2.out] {
		or1 = inToOrGate[and1.out]
	} else if inToOrGate[and1.out] == nil {
		or1 = inToOrGate[and2.out]
		swaps = append(swaps, swapOut(and1, outToGate[anotherIn(or1, and2.out)], outToGate))
	} else if inToOrGate[and2.out] == nil {
		or1 = inToOrGate[and1.out]
		swaps = append(swaps, swapOut(and2, outToGate[anotherIn(or1, and1.out)], outToGate))
	} else {
		panic(and1)
	}
	return or1, swaps

}

func zOf(x string) string {
	return "z" + x[1:]
}

func anotherIn(g *Gate, in string) string {
	if in == g.in1 {
		return g.in2
	}
	return g.in1
}

func swapOut(gate *Gate, gate2 *Gate, outToGate utils.Map[string, *Gate]) Swap {
	out1 := gate.out
	gate.out = gate2.out
	gate2.out = out1
	outToGate[gate.out] = gate
	outToGate[gate2.out] = gate2
	log.Error("swap: ", gate.out, " ", gate2.out)
	return Swap{gate.out, gate2.out}
}
