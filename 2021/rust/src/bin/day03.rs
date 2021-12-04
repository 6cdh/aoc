use aoc2021::readlines;
use aoc2021::run;

fn read() -> Vec<String> {
    readlines()
        .into_iter()
        .filter(|line| !line.trim().is_empty())
        .collect()
}

fn string2decimal(str: &str) -> u32 {
    u32::from_str_radix(str, 2).unwrap()
}

fn count0(vec: &[&str], nth: usize) -> usize {
    vec.iter().filter(|bin| bin.as_bytes()[nth] == b'0').count()
}

fn construct_binary<T>(vec: &[&str], is_zero: T) -> String
where
    T: Fn(usize, usize) -> bool,
{
    (0..vec[0].len())
        .map(|i| {
            let cnt0 = count0(vec, i);
            let cnt1 = vec.len() - cnt0;
            if is_zero(cnt0, cnt1) {
                '0'
            } else {
                '1'
            }
        })
        .collect()
}

fn part1(vec: &[&str]) -> u32 {
    let gamma_rate = string2decimal(&construct_binary(vec, |cnt0, cnt1| cnt0 > cnt1));
    let epsilon_rate = (1 << vec[0].len()) - 1 - gamma_rate;
    gamma_rate * epsilon_rate
}

fn filter_binary<'a, T>(vec: &[&'a str], is_zero: T) -> &'a str
where
    T: Fn(usize, usize) -> bool,
{
    let mut vec = vec.to_vec();
    (0..vec[0].len()).fold(&mut vec, |acc, i| {
        if acc.len() == 1 {
            return acc;
        }
        let cnt0 = count0(acc, i);
        let cnt1 = acc.len() - cnt0;
        let bit = if is_zero(cnt0, cnt1) { b'0' } else { b'1' };
        acc.retain(|bin| bin.as_bytes()[i] == bit);
        acc
    })[0]
}

fn part2(vec: &[&str]) -> u32 {
    let oxygen_rate = string2decimal(filter_binary(vec, |cnt0, cnt1| cnt0 > cnt1));
    let co2_rate = string2decimal(filter_binary(vec, |cnt0, cnt1| cnt0 <= cnt1));
    oxygen_rate * co2_rate
}

fn main() {
    run!({
        let input = read();
        let ref_vec: Vec<&str> = input.iter().map(|str| str.as_str()).collect();
        (part1(&ref_vec), part2(&ref_vec))
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_part1() {
        let case = [
            "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000",
            "11001", "00010", "01010",
        ];
        assert_eq!(part1(&case), 198);
    }

    #[test]
    fn test_part2() {
        let case = [
            "00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000",
            "11001", "00010", "01010",
        ];
        assert_eq!(part2(&case), 230);
    }
}
