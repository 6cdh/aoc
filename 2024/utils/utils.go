package utils

import (
	iter2 "aoc2024/iter"
	"aoc2024/vec"
	"bufio"
	"container/heap"
	"fmt"
	"io"
	"iter"
	"maps"
	"math"
	"slices"
	"strconv"
	"strings"
	"sync"
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

func Abs[T Int](x T) T {
	if x < 0 {
		return -x
	}
	return x
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

type TaskPool[T any] struct {
	ch chan T
	wg sync.WaitGroup
}

var ErrTaskNoResult = fmt.Errorf("no result for this task")

func NewTaskPool[T any](n int) *TaskPool[T] {
	return &TaskPool[T]{
		ch: make(chan T, n),
		wg: sync.WaitGroup{},
	}
}

func (p *TaskPool[T]) AddTask(task func() (T, error)) {
	p.wg.Add(1)
	go func() {
		defer p.wg.Done()
		r, err := task()
		if err == nil {
			p.ch <- r
		}
	}()
}

func (p *TaskPool[T]) AddChannelTask(task func(chan T)) {
	p.wg.Add(1)
	go func() {
		defer p.wg.Done()
		task(p.ch)
	}()
}

func (p *TaskPool[T]) WaitAll() {
	p.wg.Wait()
	close(p.ch)
}

func (p *TaskPool[T]) AsyncWaitAll() {
	go func() {
		p.wg.Wait()
		close(p.ch)
	}()
}

func (p *TaskPool[T]) Result() <-chan T {
	return p.ch
}

func FindMatrixPos[T comparable](b T, room [][]T) vec.Vec2i {
	for pos := range iter2.MatrixIndex(room) {
		if MatrixAt(room, pos) == b {
			return pos
		}
	}
	return vec.NewVec2i(-1, -1)
}

func MatrixAt[T any](mat [][]T, pos vec.Vec2i) T {
	return mat[pos.X][pos.Y]
}

func CopyMatrix[T any](mat [][]T) [][]T {
	new := [][]T{}
	for _, row := range mat {
		newRow := make([]T, len(row))
		copy(newRow, row)
		new = append(new, newRow)
	}
	return new
}

type WeightNode[T any] struct {
	Node T
	Dist int
}

type PriorityQueue[T any] []*WeightNode[T]

func (h PriorityQueue[T]) Len() int {
	return len(h)
}

func (h PriorityQueue[T]) Less(i int, j int) bool {
	return h[i].Dist < h[j].Dist
}

func (h PriorityQueue[T]) Swap(i int, j int) {
	h[i], h[j] = h[j], h[i]
}

func (h *PriorityQueue[T]) Push(x any) {
	*h = append(*h, x.(*WeightNode[T]))
}

func (h *PriorityQueue[T]) Pop() any {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}

func Dijkstra[Node comparable](
	start Node,
	end Node,
	edgesOf func(*WeightNode[Node]) []*WeightNode[Node],
) (map[Node]int, map[Node][]Node) {
	h := &PriorityQueue[Node]{}
	heap.Init(h)
	distTo := map[Node]int{}
	preNodes := map[Node][]Node{}

	startNode := WeightNode[Node]{start, 0}
	heap.Push(h, &startNode)
	distTo[startNode.Node] = 0
	preNodes[startNode.Node] = []Node{}

	for h.Len() > 0 {
		nearest := heap.Pop(h).(*WeightNode[Node])
		for _, neighbor := range edgesOf(nearest) {
			if !MapContains(distTo, neighbor.Node) || distTo[neighbor.Node] > neighbor.Dist {
				distTo[neighbor.Node] = neighbor.Dist
				heap.Push(h, neighbor)
				preNodes[neighbor.Node] = []Node{nearest.Node}
			} else if distTo[neighbor.Node] == neighbor.Dist {
				preNodes[neighbor.Node] = append(preNodes[neighbor.Node], nearest.Node)
			}
		}
	}

	return distTo, preNodes
}

func BFS[Node comparable](start Node, edgesOf func(Node) []Node) (map[Node]int, map[Node][]Node) {
	queue := []Node{start}
	distTo := map[Node]int{}
	preNodes := map[Node][]Node{}
	dist := 1
	for len(queue) > 0 {
		newQ := []Node{}
		for _, node := range queue {
			for _, neighbor := range edgesOf(node) {
				if !MapContains(distTo, neighbor) {
					distTo[neighbor] = dist
					preNodes[neighbor] = []Node{node}
					newQ = append(newQ, neighbor)
				} else if dist == distTo[neighbor] {
					preNodes[neighbor] = append(preNodes[neighbor], node)
				}
			}
		}
		queue = newQ
		dist += 1
	}
	return distTo, preNodes
}

type Map[K comparable, V any] map[K]V

func (m Map[K, V]) Contains(k K) bool {
	_, ok := m[k]
	return ok
}

func (m Map[K, V]) Add(k K, v V) {
	m[k] = v
}

func (m Map[K, V]) Remove(k K) {
	delete(m, k)
}

func (m Map[K, V]) Get(k K) V {
	return m[k]
}

func (m Map[K, V]) GetOrDefault(k K, other V) V {
	val, ok := m[k]
	if ok {
		return val
	}
	return other
}

func (m Map[K, V]) Update(k K, updater func(V) V) {
	m[k] = updater(m.Get(k))
}

func (m Map[K, V]) KeyIter() iter2.Iter[K] {
	return func(yield func(K) bool) {
		for k := range m {
			if !yield(k) {
				return
			}
		}
	}
}

func (m Map[K, V]) ValIter() iter2.Iter[V] {
	return func(yield func(V) bool) {
		for _, v := range m {
			if !yield(v) {
				return
			}
		}
	}
}

func (m Map[K, V]) Iter() iter2.Iter[iter2.Pair[K, V]] {
	return func(yield func(iter2.Pair[K, V]) bool) {
		for k, v := range m {
			if !yield(iter2.NewPair(k, v)) {
				return
			}
		}
	}
}

type Set[T comparable] map[T]struct{}

func (s Set[T]) Add(v T) Set[T] {
	s[v] = struct{}{}
	return s
}

func (s Set[T]) Remove(v T) Set[T] {
	delete(s, v)
	return s
}

func (s Set[T]) Contains(v T) bool {
	_, ok := s[v]
	return ok
}

func (s Set[T]) Clone() Set[T] {
	return maps.Clone(s)
}

func (s Set[T]) Union(s2 Set[T]) Set[T] {
	for v := range s2 {
		s.Add(v)
	}
	return s
}

func (s Set[T]) Intersect(s2 Set[T]) Set[T] {
	for v := range s {
		if !s2.Contains(v) {
			s.Remove(v)
		}
	}
	return s
}

func (s Set[T]) Iter() iter2.Iter[T] {
	return func(yield func(T) bool) {
		for v := range s {
			if !yield(v) {
				return
			}
		}
	}
}

func (s Set[T]) First() T {
	for v := range s {
		return v
	}
	panic(s)
}
