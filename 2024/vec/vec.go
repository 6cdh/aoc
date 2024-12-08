package vec

import (
	"math"
)

type Vec2i struct {
	X int
	Y int
}

func NewVec2i(x int, y int) Vec2i {
	return Vec2i{
		X: x,
		Y: y,
	}
}

var (
	UP    = NewVec2i(-1, 0)
	DOWN  = NewVec2i(1, 0)
	LEFT  = NewVec2i(0, -1)
	RIGHT = NewVec2i(0, 1)
)

func (v Vec2i) UnitLen() float64 {
	return math.Sqrt(float64(v.X*v.X + v.Y*v.Y))
}

func signOf(x int) int {
	if x == 0 {
		return 0
	} else if x < 0 {
		return -1
	} else {
		return 1
	}
}

func (v Vec2i) Add(u Vec2i) Vec2i {
	return Vec2i{
		X: v.X + u.X,
		Y: v.Y + u.Y,
	}
}

func (v Vec2i) Minus(u Vec2i) Vec2i {
	return Vec2i{
		X: v.X - u.X,
		Y: v.Y - u.Y,
	}
}

func (v Vec2i) Mul(u Vec2i) Vec2i {
	return Vec2i{
		X: v.X*u.X - v.Y*u.Y,
		Y: v.X*u.Y + v.Y*u.X,
	}
}

func (v Vec2i) MulI(i int) Vec2i {
	return NewVec2i(i*v.X, i*v.Y)
}

func (v Vec2i) Sign() Vec2i {
	return NewVec2i(signOf(v.X), signOf(v.Y))
}

func (v Vec2i) RotateLeft() Vec2i {
	return v.Mul(RIGHT)
}

func (v Vec2i) RotateRight() Vec2i {
	return v.Mul(LEFT)
}
