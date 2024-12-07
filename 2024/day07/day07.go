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
	for line := range utils.ReadStringLines(in) {
		parts := strings.Split(line, ": ")
		eq := Equation{
			result:    utils.StrToInt(parts[0]),
			operators: utils.ReadInts(parts[1], " "),
		}
		equations = append(equations, eq)
	}

	var wg sync.WaitGroup
	ch1 := make(chan int, len(equations))
	ch2 := make(chan int, len(equations))

	s := 0
	s2 := 0
	for _, eq := range equations {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if try(1, eq.operators[0], eq.operators, eq.result, []Op{plusOp, mulOp}) {
				ch1 <- eq.result
				ch2 <- eq.result
			} else if try(1, eq.operators[0], eq.operators, eq.result, []Op{plusOp, mulOp, concatOp}) {
				ch2 <- eq.result
			}
		}()
	}

	go func() {
		wg.Wait()
		close(ch1)
		close(ch2)
	}()

	closedChan := 0
	for {
		select {
		case v, more := <-ch1:
			if !more {
				closedChan++
			} else {
				s += v
			}
		case v, more := <-ch2:
			if !more {
				closedChan++
			} else {
				s2 += v
			}
		}
		if closedChan == 2 {
			break
		}
	}

	fmt.Fprintln(out, s)
	fmt.Fprintln(out, s2)
}

func try(i int, cur int, equation []int, result int, ops []Op) bool {
	if i == len(equation) {
		return cur == result
	}
	if cur > result {
		return false
	}
	return iter.IfAny(iter.SliceValues(ops), func(op Op) bool {
		return try(i+1, op(cur, equation[i]), equation, result, ops)
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
