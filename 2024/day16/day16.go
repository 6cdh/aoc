package day16

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"container/heap"
	"fmt"
	"io"
)

type Node struct {
	pos vec.Vec2i
	dir vec.Vec2i
}

type ScoreNode struct {
	node  Node
	score int
}

type Pos = vec.Vec2i
type Dir = vec.Vec2i

func Solve(in io.Reader, out io.Writer) {
	maze := iter.ReadLines(in).Collect()

	edgesOf := func(snode *ScoreNode) []*ScoreNode {
		neighbors := []*ScoreNode{}
		next := snode.node.pos.Add(snode.node.dir)
		if vec.IsValidPos(next, maze) && utils.MatrixAt(maze, next) != '#' {
			neighbors = append(neighbors, &ScoreNode{Node{next, snode.node.dir}, snode.score + 1})
		}
		for _, newDir := range []Dir{snode.node.dir.RotateLeft(), snode.node.dir.RotateRight()} {
			neighbors = append(neighbors, &ScoreNode{Node{snode.node.pos, newDir}, snode.score + 1000})
		}
		return neighbors
	}

	start := utils.FindMatrixPos('S', maze)
	end := utils.FindMatrixPos('E', maze)
	distTo, preNodes := Dijkstra(start, end, edgesOf)
	minScore, endNodes := processShortestPaths(end, distTo)
	fmt.Fprintln(out, minScore)
	fmt.Fprintln(out, bfsTilesOnAllPath(endNodes, preNodes))
}

// return the shortest path to `end` and all nodes at `end` for all shortest paths
func processShortestPaths(end Pos, scoreTo map[Node]int) (int, []Node) {
	minScore := -1
	endNodes := []Node{}
	for node := range scoreTo {
		if node.pos == end {
			if minScore == -1 || scoreTo[node] < minScore {
				minScore = scoreTo[node]
				endNodes = []Node{node}
			} else if scoreTo[node] == minScore {
				endNodes = append(endNodes, node)
			}
		}
	}

	return minScore, endNodes
}

// use BFS to add all predecessor nodes recursively.
func bfsTilesOnAllPath(nodes []Node, preNodes map[Node][]Node) int {
	visited := map[Node]bool{}
	tiles := map[Pos]bool{}
	for len(nodes) > 0 {
		newNodes := []Node{}
		for _, k := range nodes {
			visited[k] = true
			tiles[k.pos] = true
			for _, p := range preNodes[k] {
				if !visited[p] {
					newNodes = append(newNodes, p)
					visited[p] = true
					tiles[p.pos] = true
				}
			}
		}
		nodes = newNodes
	}
	return len(tiles)
}

type PriorityQueue []*ScoreNode

func (h PriorityQueue) Len() int {
	return len(h)
}

func (h PriorityQueue) Less(i int, j int) bool {
	return h[i].score < h[j].score
}

func (h PriorityQueue) Swap(i int, j int) {
	h[i], h[j] = h[j], h[i]
}

func (h *PriorityQueue) Push(x any) {
	*h = append(*h, x.(*ScoreNode))
}

func (h *PriorityQueue) Pop() any {
	old := *h
	n := len(old)
	x := old[n-1]
	*h = old[0 : n-1]
	return x
}

// A normal dijkstra algorithm but explore all shortest paths (not stop after found the first one).
func Dijkstra(start Pos, end Pos, edgesOf func(*ScoreNode) []*ScoreNode) (map[Node]int, map[Node][]Node) {
	h := &PriorityQueue{}
	heap.Init(h)
	distTo := map[Node]int{}
	preNodes := map[Node][]Node{}

	startNode := ScoreNode{Node{start, vec.RIGHT}, 0}
	heap.Push(h, &startNode)
	distTo[startNode.node] = 0
	preNodes[startNode.node] = []Node{}

	for h.Len() > 0 {
		nearest := heap.Pop(h).(*ScoreNode)
		for _, neignbor := range edgesOf(nearest) {
			if !utils.MapContains(distTo, neignbor.node) || distTo[neignbor.node] > neignbor.score {
				distTo[neignbor.node] = neignbor.score
				heap.Push(h, neignbor)
				preNodes[neignbor.node] = []Node{nearest.node}
			} else if distTo[neignbor.node] == neignbor.score {
				preNodes[neignbor.node] = append(preNodes[neignbor.node], nearest.node)
			}
		}
	}

	return distTo, preNodes
}
