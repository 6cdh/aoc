package iter

type Iter[V any] []V

func NewIter[V any](slice []V) Iter[V] {
	return slice
}

func (it Iter[V]) CountIf(pred func(V) bool) int {
	cnt := 0
	for _, v := range it {
		if pred(v) {
			cnt++
		}
	}
	return cnt
}

func Map[F any, T any](it Iter[F], fn func(F) T) Iter[T] {
	newIt := make([]T, len(it))
	for k, v := range it {
		newIt[k] = fn(v)
	}
	return newIt
}
