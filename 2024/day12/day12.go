package day12

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"aoc2024/vec"
	"fmt"
	"io"
)

type RegionID = int
type Pos = vec.Vec2i
type Dir = vec.Vec2i

var fourDirs = []Dir{vec.UP, vec.DOWN, vec.LEFT, vec.RIGHT}

func Solve(in io.Reader, out io.Writer) {
	garden := iter.ReadLines(in).Collect()
	area := map[RegionID]int{}
	perimeter := map[RegionID]int{}
	regions := map[Pos]RegionID{}

	// part 1
	// For each position, explore its region.
	for pos := range iter.MatrixIndex(garden) {
		exploreRegion(pos, len(regions), regions, garden)
		id := regions[pos]
		area[id] += 1
		perimeter[id] += plotPerimeter(pos, garden)
	}

	// part 2
	// For each plot and each side of this plot, if it belongs to a side of its region,
	// explore the whole region side.
	// Each plot side is encoded as a pair of position and direction.
	sides := map[RegionID]int{}
	visitedSide := map[Pos]map[Dir]bool{}
	for pos := range iter.MatrixIndex(garden) {
		for _, dir := range fourDirs {
			if isRegionSide(pos, dir, garden) && !visitedSide[pos][dir] {
				exploreRegionSides(pos, dir, visitedSide, garden)
				id := regions[pos]
				sides[id] += 1
			}
		}
	}

	part1 := 0
	part2 := 0
	for id := range area {
		part1 += area[id] * perimeter[id]
		part2 += area[id] * sides[id]
	}
	fmt.Fprintln(out, part1)
	fmt.Fprintln(out, part2)
}

func exploreRegion(pos Pos, id RegionID, regions map[Pos]RegionID, garden [][]byte) {
	if utils.MapContains(regions, pos) {
		return
	}
	regions[pos] = id

	for _, dir := range fourDirs {
		neighbor := pos.Add(dir)
		if vec.IsValidPos(neighbor, garden) && isSame(pos, neighbor, garden) {
			exploreRegion(neighbor, id, regions, garden)
		}
	}
}

func plotPerimeter(pos Pos, garden [][]byte) int {
	return iter.SliceValues(fourDirs).CountIf(func(dir Dir) bool {
		return isRegionSide(pos, dir, garden)
	})
}

func isSame(pos1 Pos, pos2 Pos, garden [][]byte) bool {
	return garden[pos1.X][pos1.Y] == garden[pos2.X][pos2.Y]
}

func isRegionSide(pos Pos, dir Dir, garden [][]byte) bool {
	neighbor := pos.Add(dir)
	return !vec.IsValidPos(neighbor, garden) || !isSame(pos, neighbor, garden)
}

func exploreRegionSides(pos Pos, dir Dir, regionSides map[Pos]map[Dir]bool, garden [][]byte) {
	if regionSides[pos][dir] {
		return
	}

	if regionSides[pos] == nil {
		regionSides[pos] = map[Dir]bool{}
	}
	regionSides[pos][dir] = true

	dirV1 := dir.RotateRight()
	dirV2 := dir.RotateLeft()
	for _, dv := range []Dir{dirV1, dirV2} {
		neighbor := pos.Add(dv)
		if vec.IsValidPos(neighbor, garden) && isSame(pos, neighbor, garden) && isRegionSide(neighbor, dir, garden) {
			exploreRegionSides(neighbor, dir, regionSides, garden)
		}
	}
}
