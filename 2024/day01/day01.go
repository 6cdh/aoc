package day01

import (
	"aoc2024/utils"
	"fmt"
	"io"
	"slices"
	"strings"
)

func Solve(in io.Reader, out io.Writer) {
	var left []int
	var right []int
	for line := range utils.ReadStringLines(in) {
		numStr := strings.Fields(line)
		left = append(left, utils.StrToInt(numStr[0]))
		right = append(right, utils.StrToInt(numStr[1]))
	}
	slices.Sort(left)
	slices.Sort(right)
	fmt.Fprintln(out, part1(left, right))
	fmt.Fprintln(out, part2(left, right))
}

func part1(left []int, right []int) int {
	sum := 0
	for i := 0; i < len(left); i++ {
		sum += utils.AbsDiff(left[i], right[i])
	}
	return sum
}

func part2(left []int, right []int) int {
	rightCounter := utils.CountFreq(right)
	sum := 0
	for _, l := range left {
		sum += l * rightCounter[l]
	}
	return sum
}
