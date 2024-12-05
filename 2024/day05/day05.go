package day05

import (
	"aoc2024/utils"
	"fmt"
	"io"
	"slices"
)

type Rule = map[int]map[int]bool

func Solve(in io.Reader, out io.Writer) {
	rules := Rule{}
	updates := [][]int{}
	isRuleSection := true
	for line := range utils.ReadStringLines(in) {
		if line == "" {
			isRuleSection = false
		} else if isRuleSection {
			ints := utils.ReadInts(line, "|")
			if rules[ints[0]] == nil {
				rules[ints[0]] = map[int]bool{}
			}
			rules[ints[0]][ints[1]] = true
		} else {
			updates = append(updates, utils.ReadInts(line, ","))
		}
	}

	sum := 0
	sum2 := 0
	for _, pages := range updates {
		if isInRightOrder(pages, rules) {
			sum += pages[len(pages)/2]
		} else {
			SortPagesByRules(pages, rules)
			sum2 += pages[len(pages)/2]
		}
	}
	fmt.Fprintln(out, sum)
	fmt.Fprintln(out, sum2)
}

func isInRightOrder(pages []int, rules Rule) bool {
	for i := 1; i < len(pages); i++ {
		if rules[pages[i]][pages[i-1]] {
			return false
		}
	}
	return true
}

func SortPagesByRules(pages []int, rules Rule) {
	slices.SortFunc(pages, func(x int, y int) int {
		if rules[x][y] {
			return utils.LessThan
		} else {
			return utils.GreaterThan
		}
	})
}
