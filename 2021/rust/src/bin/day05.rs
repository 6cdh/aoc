use aoc2021::readlines;
use aoc2021::run;

use scan_fmt::scan_fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn from(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

#[derive(Debug)]
struct Line {
    p1: Point,
    p2: Point,
}

impl Line {
    fn from(str: &str) -> Option<Self> {
        if let Ok(points) = scan_fmt!(str, "{},{} -> {},{}", i32, i32, i32, i32) {
            Some(Self {
                p1: Point::from(points.0, points.1),
                p2: Point::from(points.2, points.3),
            })
        } else {
            None
        }
    }

    fn is_hv(&self) -> bool {
        self.p1.x == self.p2.x || self.p1.y == self.p2.y
    }

    fn is_diag(&self) -> bool {
        (self.p1.x - self.p2.x).abs() == (self.p1.y - self.p2.y).abs()
    }
}

fn read() -> Vec<Line> {
    readlines()
        .iter()
        .flat_map(|line| Line::from(line))
        .collect()
}

fn solve<T>(lines: &[Line], filter_lines: T) -> usize
where
    T: FnMut(&&Line) -> bool,
{
    let mut points = vec![vec![0; 1000]; 1000];
    for l in lines.iter().filter(filter_lines) {
        let dx = (l.p2.x - l.p1.x).signum();
        let dy = (l.p2.y - l.p1.y).signum();
        let mut p = l.p1;
        while p != l.p2 {
            points[p.x as usize][p.y as usize] += 1;
            p.x += dx;
            p.y += dy;
        }
        points[p.x as usize][p.y as usize] += 1;
    }

    points.iter().flatten().filter(|v| **v > 1).count()
}

fn main() {
    run!({
        let input = read();
        (
            solve(&input, |line| line.is_hv()),
            solve(&input, |line| line.is_hv() || line.is_diag()),
        )
    });
}
