package utils

import (
	iter2 "aoc2024/iter"
	"bufio"
	"io"
	"iter"
	"math"
	"slices"
	"strconv"
	"strings"
)

type Int interface {
	int | int8 | int16 | int32 | int64
}

func ReadLine(reader io.Reader) ([]byte, error) {
	limit := math.MaxInt32
	scanner := bufio.NewScanner(reader)
	scanner.Buffer([]byte{}, limit)
	if scanner.Scan() {
		return scanner.Bytes(), nil
	} else {
		return nil, scanner.Err()
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
	return iter2.SliceValues(match).Filter(func(s string) bool {
		return s != ""
	}).Collect()
}

func CountFreq[K comparable](slice []K) map[K]int {
	counter := make(map[K]int, len(slice))
	for _, v := range slice {
		counter[v]++
	}
	return counter
}

func StringReverse(s string) string {
	bs := []byte(s)
	slices.Reverse(bs)
	return string(bs)
}

func ReadInts(str string, sep string) []int {
	numStr := strings.Split(str, sep)
	ints := iter2.Map(iter2.SliceValues(numStr), StrToInt).Collect()
	return ints
}

const (
	LessThan    = -1
	Equal       = 0
	GreaterThan = 1
)

func NeighborPairs[T any](slice []T) iter.Seq2[T, T] {
	return func(yield func(x T, y T) bool) {
		for i := 1; i < len(slice); i++ {
			if !yield(slice[i-1], slice[i]) {
				return
			}
		}
	}
}

func SortByLessFunc[T any](slice []T, lessThan func(x T, y T) bool) {
	slices.SortFunc(slice, func(x T, y T) int {
		if lessThan(x, y) {
			return LessThan
		} else {
			return GreaterThan
		}
	})
}

func RoundToInt[F float32 | float64](f F) int {
	return int(math.Round(float64(f)))
}

func NewMatrix[T any](m int, n int, init T) [][]T {
	matrix := make([][]T, m)
	for i := range matrix {
		matrix[i] = make([]T, n)
		for j := range matrix[i] {
			matrix[i][j] = init
		}
	}
	return matrix
}

func MapContains[K comparable, V any](m map[K]V, k K) bool {
	_, ok := m[k]
	return ok
}
