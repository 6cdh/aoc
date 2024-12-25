package day25

import (
	"aoc2024/iter"
	"fmt"
	"io"
)

func Solve(in io.Reader, out io.Writer) {
	lines := iter.ReadLines(in).Collect()
	locks := [][]int{}
	keys := [][]int{}
	for i := 0; i < len(lines); i += 8 {
		thing := lines[i : i+7]
		if isLock(thing) {
			locks = append(locks, pins(thing))
		} else {
			keys = append(keys, pins(thing))
		}
	}

	cnt := 0
	for _, lock := range locks {
		for _, key := range keys {
			isFit := iter.SliceIndexes(lock).All(func(i int) bool {
				return lock[i]+key[i] <= 5
			})
			if isFit {
				cnt += 1
			}
		}
	}
	fmt.Fprintln(out, cnt)
}

func isLock(thing [][]byte) bool {
	return iter.SliceValues(thing[0]).All(func(b byte) bool {
		return b == '#'
	})
}

func pins(thing [][]byte) []int {
	pins := []int{}
	for j := 0; j < len(thing[0]); j++ {
		c := 0
		for i := 1; i < len(thing)-1; i++ {
			if thing[i][j] == '#' {
				c += 1
			}
		}
		pins = append(pins, c)
	}
	return pins
}
