package utils

import (
	"bufio"
	"io"
	"iter"
	"strconv"
)

type Int interface {
	int | int8 | int16 | int32 | int64
}

func ReadBytesLine(reader io.Reader) ([]byte, error) {
	scanner := bufio.NewScanner(reader)
	if scanner.Scan() {
		return scanner.Bytes(), nil
	} else {
		return nil, scanner.Err()
	}
}

func IterLines(in io.Reader) iter.Seq[string] {
	return func(yield func(line string) bool) {
		scanner := bufio.NewScanner(in)
		for scanner.Scan() {
			if !yield(scanner.Text()) {
				return
			}
		}
	}
}

func AbsDiff[T Int](x T, y T) T {
	diff := x - y
	if diff > 0 {
		return diff
	} else {
		return -diff
	}
}

// IgnoreErr accepts a function which return a result and an error,
// returns another function that only returns the result.
func IgnoreErr[arg any, res any](fn func(arg) (res, error)) func(arg) res {
	return func(x arg) res {
		r, _ := fn(x)
		return r
	}
}

func StrToInt(s string) int {
	return IgnoreErr(strconv.Atoi)(s)
}

// ClearEmptyMatch remove stupid empty string in match slice.
func ClearEmptyMatch(match []string) []string {
	return Filter(match, func(s string) bool {
		return s != ""
	})
}

func CountIf[V any](slice []V, pred func(V) bool) int {
	cnt := 0
	for _, v := range slice {
		if pred(v) {
			cnt++
		}
	}
	return cnt
}

func Filter[V any](slice []V, pred func(V) bool) []V {
	res := []V{}
	for _, v := range slice {
		if pred(v) {
			res = append(res, v)
		}
	}
	return res
}

func CountFreq[K comparable](slice []K) map[K]int {
	counter := map[K]int{}
	for _, v := range slice {
		counter[v]++
	}
	return counter
}

func Map[F any, T any](it []F, fn func(F) T) []T {
	newIt := make([]T, len(it))
	for k, v := range it {
		newIt[k] = fn(v)
	}
	return newIt
}
