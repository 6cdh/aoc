package day11

import (
	"aoc2024/utils"
	"fmt"
	"io"
	"strconv"
)

func Solve(in io.Reader, out io.Writer) {
	line, _ := utils.ReadLine(in)
	stones := utils.ReadInts(string(line), " ")
	fmt.Fprintln(out, blink(stones, 25))
	fmt.Fprintln(out, blink(stones, 75))
}

func blink(stones []int, n int) int {
	sum := 0
	dp := map[int]map[int]int{}
	for _, s := range stones {
		sum += blink1(s, n, dp)
	}
	return sum
}

func blink1(stone int, n int, dp map[int]map[int]int) int {
	val, ok := dp[stone][n]
	if ok {
		return val
	}

	if n == 0 {
		return 1
	}

	sum := 0
	if stone == 0 {
		sum += blink1(1, n-1, dp)
	} else if digitStr := strconv.Itoa(stone); len(digitStr)%2 == 0 {
		b := []byte(digitStr)
		left := b[:len(b)/2]
		right := b[len(b)/2:]
		sum += blink1(utils.StrToInt(string(left)), n-1, dp)
		sum += blink1(utils.StrToInt(string(right)), n-1, dp)
	} else {
		sum += blink1(stone*2024, n-1, dp)
	}

	if dp[stone] == nil {
		dp[stone] = map[int]int{}
	}
	dp[stone][n] = sum
	return sum
}
