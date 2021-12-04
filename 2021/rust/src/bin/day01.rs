use aoc2021::readlines;
use aoc2021::run;

fn part1(depths: &[i32]) -> usize {
    depths.windows(2).filter(|w| w[0] < w[1]).count()
}

fn part2(depths: &[i32]) -> usize {
    depths.windows(4).filter(|w| w[0] < w[3]).count()
}

fn read() -> Vec<i32> {
    readlines()
        .iter()
        .flat_map(|line| line.parse::<i32>())
        .collect()
}

fn main() {
    run!({
        let input = read();
        (part1(&input), part2(&input))
    });
}
