use aoc2021::readlines;
use aoc2021::run;
use aoc2021::tonumbers;

struct Board(Vec<Vec<i32>>);

impl Board {
    const MARKED: i32 = -1;

    fn from(lines6: &[String]) -> Self {
        Self(
            lines6
                .iter()
                .skip(1)
                .map(|line| tonumbers(line, ' '))
                .collect(),
        )
    }

    fn mark(&mut self, picked: i32) {
        self.0.iter_mut().flatten().for_each(|v| {
            if *v == picked {
                *v = -1;
            }
        });
    }

    fn win(&self) -> bool {
        self.0
            .iter()
            .any(|row| row.iter().all(|v| *v == Board::MARKED))
            || (0..5).any(|i| self.0.iter().map(|row| row[i]).all(|v| v == Board::MARKED))
    }

    fn score(&self, picked: i32) -> i32 {
        self.0
            .iter()
            .flatten()
            .filter(|v| **v != Board::MARKED)
            .sum::<i32>()
            * picked
    }
}

type Input = (Vec<i32>, Vec<Board>);

fn read() -> Input {
    let lines = readlines();
    let numbers = tonumbers(&lines[0], ',');
    let boards = lines[1..].chunks(6).map(Board::from).collect();
    (numbers, boards)
}

fn part1(input: &mut Input) -> (i32, i32) {
    let boards = &mut input.1;
    let mut first_win = 0;
    let mut last_win = 0;

    let mut wined = vec![false; boards.len()];
    let mut wined_cnt = 0;

    for &picked in &input.0 {
        for i in 0..boards.len() {
            if wined[i] {
                continue;
            }
            boards[i].mark(picked);
            let b = &boards[i];

            if b.win() {
                wined_cnt += 1;
                if wined_cnt == 1 {
                    first_win = b.score(picked);
                } else if wined_cnt == boards.len() {
                    last_win = b.score(picked);
                }
                wined[i] = true;
            }
        }
    }
    (first_win, last_win)
}

fn main() {
    run!({
        let mut input = read();
        part1(&mut input)
    });
}
