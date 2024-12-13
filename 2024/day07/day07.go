package day07

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type Equation struct {
	result    int
	operators []int
}

func Solve(in io.Reader, out io.Writer) {
	equations := []Equation{}
	for line := range iter.ReadStringLines(in) {
		parts := strings.Split(line, ": ")
		eq := Equation{
			result:    utils.StrToInt(parts[0]),
			operators: utils.ReadInts(parts[1], " "),
		}
		equations = append(equations, eq)
	}

	s := 0
	s2 := 0
	for _, eq := range equations {
		if try(len(eq.operators)-1, eq.result, eq, []Op{minusOp, divOp}) {
			s += eq.result
			s2 += eq.result
		} else if try(len(eq.operators)-1, eq.result, eq, []Op{minusOp, divOp, unConcatOp}) {
			s2 += eq.result
		}
	}

	fmt.Fprintln(out, s)
	fmt.Fprintln(out, s2)
}

func try(i int, result int, eq Equation, ops []Op) bool {
	if i == 0 {
		return result == eq.operators[0]
	}
	return iter.SliceValues(ops).Any(func(op Op) bool {
		x, err := op(eq.operators[i], result)
		return errors.Is(err, errDivisorIsZero) || (err == nil && try(i-1, x, eq, ops))
	})
}

type Op func(int, int) (int, error)

var (
	errCantContinue  = fmt.Errorf("can't continue with operator")
	errDivisorIsZero = fmt.Errorf("divisor is zero")
)

func minusOp(y int, r int) (int, error) {
	return r - y, nil
}

func divOp(divisor int, r int) (int, error) {
	if divisor != 0 && r%divisor == 0 {
		return r / divisor, nil
	}
	if divisor == 0 && r == 0 {
		return 0, errDivisorIsZero
	}
	return 0, errCantContinue
}

func unConcatOp(y int, r int) (int, error) {
	sr := strconv.Itoa(r)
	sy := strconv.Itoa(y)
	sx, isSuffix := strings.CutSuffix(sr, sy)
	if isSuffix {
		x, err := strconv.Atoi(sx)
		if err == nil {
			return x, nil
		}
	}
	return 0, errCantContinue
}
