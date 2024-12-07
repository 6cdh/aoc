package iter

import (
	"bufio"
	"io"
	"iter"
	"sync"
)

type Pair[F any, S any] struct {
	fst F
	snd S
}

func ReadLines(in io.Reader) iter.Seq[[]byte] {
	return func(yield func(line []byte) bool) {
		scanner := bufio.NewScanner(in)
		for scanner.Scan() {
			if !yield(scanner.Bytes()) {
				return
			}
		}
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

func SliceIndexes[S ~[]V, V any](slice S) iter.Seq[int] {
	return func(yield func(int) bool) {
		for i := range slice {
			if !yield(i) {
				return
			}
		}
	}
}

func SliceValues[S ~[]V, V any](slice S) iter.Seq[V] {
	return func(yield func(V) bool) {
		for _, v := range slice {
			if !yield(v) {
				return
			}
		}
	}
}

func Enumerate[S ~[]V, V any](slice S) iter.Seq[Pair[int, V]] {
	return func(yield func(Pair[int, V]) bool) {
		for i, v := range slice {
			if !yield(Pair[int, V]{i, v}) {
				return
			}
		}
	}
}

func MatrixIndex[S ~[][]V, V any](matrix S) iter.Seq[Pair[int, int]] {
	return func(yield func(Pair[int, int]) bool) {
		for i, s := range matrix {
			for j := range s {
				if !yield(Pair[int, int]{i, j}) {
					return
				}
			}
		}
	}
}

func Map[F any, T any](it iter.Seq[F], fn func(F) T) iter.Seq[T] {
	return func(yield func(T) bool) {
		for v := range it {
			if !(yield(fn(v))) {
				return
			}
		}
	}
}

func Filter[V any](it iter.Seq[V], pred func(V) bool) iter.Seq[V] {
	return func(yield func(V) bool) {
		for v := range it {
			if pred(v) {
				if !yield(v) {
					return
				}
			}
		}
	}
}

func Reduce[T any, V any](it iter.Seq[V], initial T, fn func(T, V) T) T {
	for v := range it {
		initial = fn(initial, v)
	}
	return initial
}

func CountIf[V any](it iter.Seq[V], pred func(V) bool) int {
	return Reduce(it, 0, func(cnt int, v V) int {
		if pred(v) {
			return cnt + 1
		}
		return cnt
	})
}

func CountIfParallel[V any](it iter.Seq[V], pred func(V) bool) int {
	var wg sync.WaitGroup
	ch := make(chan struct{}, 2000)
	task := func(v V) {
		defer wg.Done()
		if pred(v) {
			ch <- struct{}{}
		}
	}

	for v := range it {
		wg.Add(1)
		go task(v)
	}

	go func() {
		wg.Wait()
		close(ch)
	}()

	cnt := 0
	for {
		_, more := <-ch
		if more {
			cnt++
		} else {
			return cnt
		}
	}
}
