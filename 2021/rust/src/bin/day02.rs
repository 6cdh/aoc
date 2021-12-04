use aoc2021::readlines;
use aoc2021::run;

enum Dir {
    Forward,
    Up,
    Down,
}

impl Dir {
    fn from(str: &str) -> Option<Dir> {
        match str {
            "forward" => Some(Dir::Forward),
            "up" => Some(Dir::Up),
            "down" => Some(Dir::Down),
            _ => None,
        }
    }
}

struct Command {
    dir: Dir,
    dist: usize,
}

impl Command {
    fn from(str: &str) -> Option<Self> {
        if let Ok(cmd) = scan_fmt::scan_fmt!(str, "{} {}", String, usize) {
            Some(Command {
                dir: Dir::from(&cmd.0)?,
                dist: cmd.1,
            })
        } else {
            None
        }
    }
}

struct Pos {
    x: usize,
    y: usize,
}

fn part1(cmds: &[Command]) -> usize {
    let mut pos = Pos { x: 0, y: 0 };

    for cmd in cmds {
        match cmd.dir {
            Dir::Up => pos.y -= cmd.dist,
            Dir::Down => pos.y += cmd.dist,
            Dir::Forward => pos.x += cmd.dist,
        }
    }

    pos.x * pos.y
}

fn part2(cmds: &[Command]) -> usize {
    let mut pos = Pos { x: 0, y: 0 };
    let mut aim = 0;

    for cmd in cmds {
        match cmd.dir {
            Dir::Up => aim -= cmd.dist,
            Dir::Down => aim += cmd.dist,
            Dir::Forward => {
                pos.x += cmd.dist;
                pos.y += aim * cmd.dist;
            }
        }
    }

    pos.x * pos.y
}

fn read() -> Vec<Command> {
    readlines()
        .iter()
        .flat_map(|line| Command::from(line))
        .collect()
}

fn main() {
    run!({
        let input = read();
        (part1(&input), part2(&input))
    });
}
