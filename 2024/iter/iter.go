package iter

import (
	"aoc2024/vec"
	"bufio"
	"io"
	"iter"
	"sync"
)

type Pair[F any, S any] struct {
	Fst F
	Snd S
}

func NewPair[F any, S any](fst F, snd S) Pair[F, S] {
	return Pair[F, S]{fst, snd}
}

type Iter[V any] iter.Seq[V]

func ReadLines(in io.Reader) Iter[[]byte] {
	scanner := bufio.NewScanner(in)
	return func(yield func(line []byte) bool) {
		for scanner.Scan() {
			if !yield([]byte(scanner.Text())) {
				return
			}
		}
	}
}

func ReadStringLines(in io.Reader) Iter[string] {
	scanner := bufio.NewScanner(in)
	return func(yield func(line string) bool) {
		for scanner.Scan() {
			if !yield(scanner.Text()) {
				return
			}
		}
	}
}

func (it Iter[V]) Collect() []V {
	slice := []V{}
	for v := range it {
		slice = append(slice, v)
	}
	return slice
}

func SliceIndexes[S ~[]V, V any](slice S) Iter[int] {
	return func(yield func(int) bool) {
		for i := range slice {
			if !yield(i) {
				return
			}
		}
	}
}

func SliceValues[S ~[]V, V any](slice S) Iter[V] {
	return func(yield func(V) bool) {
		for _, v := range slice {
			if !yield(v) {
				return
			}
		}
	}
}

func Enumerate[S ~[]V, V any](slice S) Iter[Pair[int, V]] {
	return func(yield func(Pair[int, V]) bool) {
		for i, v := range slice {
			if !yield(Pair[int, V]{i, v}) {
				return
			}
		}
	}
}

func MatrixIndex[S ~[][]V, V any](matrix S) Iter[vec.Vec2i] {
	return func(yield func(vec.Vec2i) bool) {
		for i, s := range matrix {
			for j := range s {
				if !yield(vec.NewVec2i(i, j)) {
					return
				}
			}
		}
	}
}

func MapIter[K comparable, V any](m map[K]V) Iter[K] {
	return func(yield func(K) bool) {
		for k, _ := range m {
			if !yield(k) {
				return
			}
		}
	}
}

func Map[F any, T any](it Iter[F], fn func(F) T) Iter[T] {
	return func(yield func(T) bool) {
		for v := range it {
			if !(yield(fn(v))) {
				return
			}
		}
	}
}

func (it Iter[V]) Filter(pred func(V) bool) Iter[V] {
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

func Reduce[T any, V any](it Iter[V], initial T, fn func(T, V) T) T {
	for v := range it {
		initial = fn(initial, v)
	}
	return initial
}

func (it Iter[V]) Any(pred func(V) bool) bool {
	for v := range it {
		if pred(v) {
			return true
		}
	}
	return false
}

func (it Iter[V]) All(pred func(V) bool) bool {
	for v := range it {
		if !pred(v) {
			return false
		}
	}
	return true
}

func (it Iter[V]) CountIf(pred func(V) bool) int {
	return Reduce(it, 0, func(cnt int, v V) int {
		if pred(v) {
			return cnt + 1
		}
		return cnt
	})
}

func (it Iter[V]) CountIfParallel(pred func(V) bool) int {
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
	for range ch {
		cnt++
	}
	return cnt
}
