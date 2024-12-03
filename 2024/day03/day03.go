package day03

import (
	"aoc2024/utils"
	"fmt"
	"io"
	"regexp"
)

func Solve(in io.Reader, out io.Writer) {
	input, _ := io.ReadAll(in)
	fmt.Fprintln(out, part1(string(input)))
	fmt.Fprintln(out, part2(string(input)))
}

func part1(input string) int {
	sum := 0
	re := regexp.MustCompile(`(?m)mul\((\d+),(\d+)\)`)
	for _, shit := range re.FindAllStringSubmatch(input, -1) {
		match := utils.ClearEmptyMatch(shit)
		sum += utils.StringToInt(match[1]) * utils.StringToInt(match[2])
	}
	return sum
}

func part2(input string) int {
	sum := 0
	isEnable := true
	re := regexp.MustCompile(`(mul)\((\d+),(\d+)\)|(do|don't)\(\)`)
	for _, shit := range re.FindAllStringSubmatch(input, -1) {
		match := utils.ClearEmptyMatch(shit)
		switch match[1] {
		case "mul":
			if isEnable {
				sum += utils.StringToInt(match[2]) * utils.StringToInt(match[3])
			}
		case "do":
			isEnable = true
		case "don't":
			isEnable = false
		}
	}
	return sum
}
