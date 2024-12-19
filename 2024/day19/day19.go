package day19

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"fmt"
	"io"
	"strings"
)

func Solve(in io.Reader, out io.Writer) {
	lines := iter.ReadStringLines(in).Collect()
	patterns := strings.Split(lines[0], ", ")
	designs := lines[2:]

	possibleCnt := 0
	waysCnt := 0
	for _, design := range designs {
		dp := map[int]int{}
		ways := countWaysDP(0, design, patterns, dp)
		if ways > 0 {
			possibleCnt += 1
		}
		waysCnt += ways
	}

	fmt.Fprintln(out, possibleCnt)
	fmt.Fprintln(out, waysCnt)
}

func countWaysDP(i int, design string, patterns []string, dp map[int]int) int {
	if !utils.MapContains(dp, i) {
		dp[i] = countWays(i, design, patterns, dp)
	}
	return dp[i]
}

func countWays(i int, design string, patterns []string, dp map[int]int) int {
	if i == len(design) {
		return 1
	}
	sum := 0
	for _, p := range patterns {
		if strings.HasPrefix(design[i:], p) {
			sum += countWaysDP(i+len(p), design, patterns, dp)
		}
	}
	return sum
}
