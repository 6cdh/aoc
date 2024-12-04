package utils

import (
	"bufio"
	"io"
	"iter"
	"slices"
	"strconv"
)

type Int interface {
	int | int8 | int16 | int32 | int64
}

func ReadLine(reader io.Reader) ([]byte, error) {
	scanner := bufio.NewScanner(reader)
	if scanner.Scan() {
		return scanner.Bytes(), nil
	} else {
		return nil, scanner.Err()
	}
}

func ReadStringLines(in io.Reader) iter.Seq[string] {
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
	if x > y {
		return x - y
	} else {
		return y - x
	}
}

// IgnoreErr wraps a function, ignoring its error and returning only its result.
func IgnoreErr[Arg any, Res any](fn func(Arg) (Res, error)) func(Arg) Res {
	return func(x Arg) Res {
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
	res := make([]V, 0, len(slice))
	for _, v := range slice {
		if pred(v) {
			res = append(res, v)
		}
	}
	return res
}

func CountFreq[K comparable](slice []K) map[K]int {
	counter := make(map[K]int, len(slice))
	for _, v := range slice {
		counter[v]++
	}
	return counter
}

func Map[F any, T any](slice []F, fn func(F) T) []T {
	res := make([]T, len(slice))
	for i, v := range slice {
		res[i] = fn(v)
	}
	return res
}

func StringReverse(s string) string {
	bs := []byte(s)
	slices.Reverse(bs)
	return string(bs)
}
