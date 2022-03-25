fn part1(readings: &[i32]) -> usize {
    readings
        .windows(2)
        .filter(|w| match w {
            [a, b] => a < b,
            _ => false,
        })
        .count()
}

fn part2(readings: &[i32]) -> usize {
    part1(
        readings
            .windows(3)
            .map(|w| w.iter().sum())
            .collect::<Vec<i32>>()
            .as_slice()
    )
}

#[cfg(test)]
mod tests {

    const EXAMPLE : &[i32] = &[199, 200, 208, 210, 200, 207, 240, 269, 260, 263];

    #[test]
    fn part1() {
        let count = super::part1(&EXAMPLE);

        assert_eq!(count, 7);
    }

    #[test]
    fn part2() {
        let count = super::part2(&EXAMPLE);

        assert_eq!(count, 5);
    }
}


fn parse(input: &str) -> Vec<i32> {
    input.lines().filter_map(|line| line.parse().ok()).collect()
}

pub fn run() {
    let input = parse(include_str!("./day01.input"));

    println!("day01 - part1: {}", part1(&input));
    println!("day01 - part2: {}", part2(&input));
}
