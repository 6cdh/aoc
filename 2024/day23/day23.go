package day23

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"fmt"
	"io"
	"slices"
	"strings"
)

type Vertex = string
type Vertexes = utils.Set[Vertex]
type Graph = utils.Map[Vertex, Vertexes]

func Solve(in io.Reader, out io.Writer) {
	graph := Graph{}

	addEdge := func(from Vertex, to Vertex) {
		graph.Update(from, func(v Vertexes) Vertexes {
			if v == nil {
				return Vertexes{}.Add(to)
			}
			return v.Add(to)
		})
	}

	for line := range iter.ReadStringLines(in) {
		vStrs := strings.Split(line, "-")
		addEdge(vStrs[0], vStrs[1])
		addEdge(vStrs[1], vStrs[0])
	}

	fmt.Fprintln(out, part1(graph))
	fmt.Fprintln(out, part2(graph))
}

func part1(graph Graph) int {
	type Group3 = [3]string
	groups := utils.Set[Group3]{}
	for v := range graph {
		for nei := range graph[v] {
			for nei2 := range graph[v] {
				if graph[nei].Contains(nei2) {
					strs := Group3{v, nei, nei2}
					slices.Sort(strs[:])
					groups.Add(strs)
				}
			}
		}
	}

	return groups.Iter().CountIf(func(g Group3) bool {
		return iter.SliceValues(g[:]).Any(func(s string) bool {
			return strings.HasPrefix(s, "t")
		})
	})
}

func part2(graph Graph) string {
	vertex := Vertexes{}
	for v := range graph {
		vertex.Add(v)
	}
	cliqueIter := BronKerboschIter(Vertexes{}, vertex, Vertexes{}, graph)
	maxClique := *iter.ArgMax(cliqueIter, 0, func(clique Vertexes) int { return len(clique) })
	names := maxClique.Iter().Collect()
	slices.Sort(names)
	return strings.Join(names, ",")
}

func BronKerboschIter(r Vertexes, p Vertexes, x Vertexes, graph Graph) iter.Iter[Vertexes] {
	return func(yield func(Vertexes) bool) {
		BronKerbosch(r, p, x, yield, graph)
	}
}

func BronKerbosch(r Vertexes, p Vertexes, x Vertexes, yield func(Vertexes) bool, graph Graph) bool {
	if len(p) == 0 && len(x) == 0 {
		yield(r)
	}
	p2 := p.Clone()
	p3 := p.Union(x)
	for v := range p {
		if graph[p3.First()].Contains(v) {
			continue
		}
		nv := graph[v]
		if !BronKerbosch(r.Clone().Add(v), p2.Clone().Intersect(nv), x.Clone().Intersect(nv), yield, graph) {
			return false
		}
		p2.Remove(v)
		x.Add(v)
	}
	return true
}
