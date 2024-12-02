package day02

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"fmt"
	"io"
	"slices"
	"strings"
)

func Solve(in io.Reader, out io.Writer) {
	var reports [][]int
	for line := range utils.IterLines(in) {
		numStr := strings.Fields(line)
		ints := iter.Map(numStr, utils.StringToInt)
		reports = append(reports, ints)
	}
	fmt.Fprintln(out, part1(reports))
	fmt.Fprintln(out, part2(reports))
}

func isSafe(report []int) bool {
	order := 0
	for i := 1; i < len(report); i++ {
		diff := utils.AbsDiff(report[i], report[i-1])
		if !(1 <= diff && diff <= 3) {
			return false
		}
		curOrder := report[i] - report[i-1]
		if order*curOrder < 0 {
			return false
		}
		if curOrder != 0 {
			order = curOrder
		}
	}
	return true
}

func isSafe2(report []int) bool {
	if isSafe(report) {
		return true
	}
	for rem := 0; rem < len(report); rem++ {
		// NOTE: clone the underlying array
		newReport := slices.Clone(report)
		newReport = slices.Delete(newReport, rem, rem+1)
		if isSafe(newReport) {
			return true
		}
	}
	return false
}

func part1(reports [][]int) int {
	return iter.NewIter(reports).CountIf(isSafe)
}

func part2(reports [][]int) int {
	return iter.NewIter(reports).CountIf(isSafe2)
}
