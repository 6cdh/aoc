package day07

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"fmt"
	"io"
	"strconv"
	"strings"
	"sync"
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

	var wg sync.WaitGroup
	results := make(chan [2]int, len(equations))

	for _, eq := range equations {
		wg.Add(1)
		go func() {
			defer wg.Done()
			s := 0
			s2 := 0
			if try(1, eq.operators[0], eq, []Op{plusOp, mulOp}) {
				s = eq.result
				s2 = eq.result
			} else if try(1, eq.operators[0], eq, []Op{plusOp, mulOp, concatOp}) {
				s2 = eq.result
			}
			results <- [2]int{s, s2}
		}()
	}

	go func() {
		wg.Wait()
		close(results)
	}()

	s := 0
	s2 := 0
	for r := range results {
		s += r[0]
		s2 += r[1]
	}

	fmt.Fprintln(out, s)
	fmt.Fprintln(out, s2)
}

func try(i int, cur int, eq Equation, ops []Op) bool {
	if i == len(eq.operators) {
		return cur == eq.result
	}
	if cur > eq.result {
		return false
	}
	return iter.SliceValues(ops).Any(func(op Op) bool {
		return try(i+1, op(cur, eq.operators[i]), eq, ops)
	})
}

type Op func(int, int) int

func plusOp(x int, y int) int {
	return x + y
}

func mulOp(x int, y int) int {
	return x * y
}

func concatOp(x int, y int) int {
	sx := strconv.Itoa(x)
	sy := strconv.Itoa(y)
	return utils.StrToInt(sx + sy)
}
