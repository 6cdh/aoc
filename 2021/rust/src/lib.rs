use std::io::{self, BufRead};

pub fn readlines() -> Vec<String> {
    let stdin = io::stdin();
    stdin.lock().lines().map(|line| line.unwrap()).collect()
}

pub fn read_numbers(str: &str, dim: char) -> Vec<i32> {
    str.split(dim).flat_map(|s| s.parse::<i32>()).collect()
}

#[macro_export]
macro_rules! run {
    ($e:block) => {
        use colored::*;
        use std::time::Instant;
        let s = Instant::now();
        let ans = $e;
        let e = Instant::now();
        let ans = format!("{:?}", ans).green();
        println!("result: {} in {:.2?}", ans, e - s);
    };
}
