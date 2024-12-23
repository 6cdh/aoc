package day22

import (
	"aoc2024/iter"
	"aoc2024/utils"
	"fmt"
	"io"
)

func Solve(in io.Reader, out io.Writer) {
	secrets := iter.Map(iter.ReadStringLines(in), utils.StrToInt).Collect()

	fmt.Fprintln(out, part1(secrets))
	fmt.Fprintln(out, part2(secrets))
}

func part1(secrets []int) int {
	sum := 0
	for _, sec := range secrets {
		sum += next(sec, 2000)
	}
	return sum
}

type ChangeSeq = int
type SeqToPriceT = map[ChangeSeq]int

func part2(secrets []int) int {
	tp := utils.NewTaskPool[SeqToPriceT](len(secrets))
	for _, sec := range secrets {
		tp.AddTask(func() (SeqToPriceT, error) {
			return addPrice(sec), nil
		})
	}
	tp.AsyncWaitAll()

	prices := SeqToPriceT{}
	for r := range tp.Result() {
		for seq, price := range r {
			prices[seq] += price
		}
	}

	maxPrice := 0
	for _, price := range prices {
		maxPrice = max(maxPrice, price)
	}
	return maxPrice
}

func addPrice(sec int) SeqToPriceT {
	seqToPrice := SeqToPriceT{}
	addPriceRec(4, next(sec, 0), next(sec, 1), next(sec, 2), next(sec, 3), next(sec, 4), seqToPrice)
	return seqToPrice
}

func addPriceRec(i int, p1 int, p2 int, p3 int, p4 int, p5 int, seqToPrice SeqToPriceT) {
	if i == 2001 {
		return
	}
	seq := changeSeqHash(
		price(p2)-price(p1),
		price(p3)-price(p2),
		price(p4)-price(p3),
		price(p5)-price(p4),
	)
	if !utils.MapContains(seqToPrice, seq) {
		seqToPrice[seq] = price(p5)
	}
	addPriceRec(i+1, p2, p3, p4, p5, next(p5, 1), seqToPrice)
}

func changeSeqHash(d1 int, d2 int, d3 int, d4 int) int {
	return (d1+9)*19*19*19 + (d2+9)*19*19 + (d3+9)*19 + (d4 + 9)
}

func price(x int) int {
	return x % 10
}

func next(sec int, n int) int {
	if n == 0 {
		return sec
	}
	s1 := process(sec, sec*64)
	s2 := process(s1, s1/32)
	s3 := process(s2, s2*2048)
	return next(s3, n-1)
}

func process(sec int, v int) int {
	return (sec ^ v) % 16777216
}
