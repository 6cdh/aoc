package day09

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"fmt"
	"io"
)

func Solve(in io.Reader, out io.Writer) {
	line, _ := utils.ReadLine(in)
	diskMap := iter.Map(iter.SliceValues(line), func(b byte) int {
		return int(b - '0')
	}).Collect()

	fmt.Fprintln(out, part1(diskMap))
	fmt.Fprintln(out, part2(diskMap))
}

type File struct {
	id   int
	size int
}

type Free struct {
	files    []File
	freeSize int
}

const (
	isFile = iota
	isFree
)

// File | Free
type Block struct {
	file *File
	free *Free
	t    int
}

func part1(diskMap []int) int {
	blocks := diskMapToBlocks(diskMap)

	freeI := 1
	for fileI := range RevFileIndexIter(blocks) {
		file := blocks[fileI].file
		for file.size > 0 && freeI < fileI {
			free := blocks[freeI].free
			moveSize := min(file.size, free.freeSize)
			file.size -= moveSize
			free.freeSize -= moveSize
			free.files = append(free.files, File{
				size: moveSize,
				id:   file.id,
			})
			if free.freeSize == 0 {
				freeI += 2
			}
		}
	}

	return checksum(blocks)
}

func part2(diskMap []int) int {
	blocks := diskMapToBlocks(diskMap)
	for fileI := range RevFileIndexIter(blocks) {
		file := blocks[fileI].file
		for freeI := 1; freeI < fileI; freeI += 2 {
			free := blocks[freeI].free
			if free.freeSize >= file.size {
				free.freeSize -= file.size
				free.files = append(free.files, *file)
				blocks[fileI] = Block{
					t: isFree,
					free: &Free{
						freeSize: file.size,
					},
				}
				break
			}
		}
	}
	return checksum(blocks)
}

func diskMapToBlocks(diskMap []int) []Block {
	blocks := []Block{}
	for i, size := range diskMap {
		if i%2 == 0 {
			blocks = append(blocks, Block{
				t: isFile,
				file: &File{
					id:   i / 2,
					size: size,
				},
			})
		} else {
			blocks = append(blocks, Block{
				t: isFree,
				free: &Free{
					freeSize: size,
				},
			})
		}
	}
	return blocks
}

// checksum BEGIN

type checkSumer struct {
	sum   int
	index int
}

func (c *checkSumer) sumFile(file *File) {
	for range file.size {
		c.sum += c.index * file.id
		c.index++
	}
}

func (c *checkSumer) sumFree(free *Free) {
	for _, file := range free.files {
		c.sumFile(&file)
	}
	c.index += free.freeSize
}

func checksum(blocks []Block) int {
	c := checkSumer{}
	for _, b := range blocks {
		switch b.t {
		case isFile:
			c.sumFile(b.file)
		case isFree:
			c.sumFree(b.free)
		}
	}
	return c.sum
}

// checksum END

func RevFileIndexIter(blocks []Block) iter.Iter[int] {
	return func(yield func(int) bool) {
		for i := (len(blocks) - 1) / 2 * 2; i >= 0; i -= 2 {
			if !yield(i) {
				return
			}
		}
	}
}
