use aoc2021::read_numbers;
use aoc2021::readlines;
use aoc2021::run;

fn read() -> Vec<usize> {
    read_numbers(&readlines()[0], ',')
        .into_iter()
        .map(|v| v as usize)
        .collect()
}

fn solve(fish: &[usize], days: usize) -> usize {
    let mut fish_cnt = [0; 9];
    for f in fish {
        fish_cnt[*f] += 1;
    }
    for _ in 0..days {
        fish_cnt.rotate_left(1);
        fish_cnt[6] += fish_cnt[8];
    }
    fish_cnt.iter().sum()
}

fn main() {
    run!({
        let input = read();
        (solve(&input, 80), solve(&input, 256))
    });
}
