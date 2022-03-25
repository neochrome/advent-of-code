enum Command {
    Forward(i32),
    Down(i32),
    Up(i32),
}

#[derive(Debug, PartialEq)]
struct Position {
    x: i32,
    y: i32,
}
const START_POSITION: Position = Position { x: 0, y: 0 };

fn drive1(start_position: Position, commands: &[Command]) -> Position {
    commands
        .iter()
        .fold(start_position, |Position { x, y }, cmd| match cmd {
            Command::Forward(n) => Position { x: x + n, y },
            Command::Up(n) => Position { x, y: y - n },
            Command::Down(n) => Position { x, y: y + n },
        })
}

fn part1(commands: &[Command]) -> i32 {
    let Position { x, y } = drive1(START_POSITION, commands);
    return x * y;
}

fn drive2(start_position: Position, start_aim: i32, commands: &[Command]) -> (Position, i32) {
    commands
        .iter()
        .fold((start_position, start_aim), |(pos, aim), cmd| match cmd {
            Command::Forward(n) => (
                Position {
                    x: pos.x + n,
                    y: pos.y + aim * n,
                },
                aim,
            ),
            Command::Up(n) => (pos, aim - n),
            Command::Down(n) => (pos, aim + n),
        })
}

fn part2(commands: &[Command]) -> i32 {
    let (Position { x, y }, _) = drive2(START_POSITION, 0, commands);
    return x * y;
}

fn parse_command(command_text: &str) -> Command {
    match command_text.split_once(' ') {
        Some((cmd, dist)) => match (cmd, dist.parse::<i32>()) {
            ("forward", Ok(n)) => Command::Forward(n),
            ("up", Ok(n)) => Command::Up(n),
            ("down", Ok(n)) => Command::Down(n),
            _ => panic!("bad input: {}", command_text),
        },
        _ => panic!("bad input: {}", command_text),
    }
}

fn parse(input: &str) -> Vec<Command> {
    input
        .lines()
        .filter(|s| !s.is_empty())
        .map(parse_command)
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::day02::*;

    mod drive1 {
        use crate::day02::*;
        #[test]
        fn no_commands() {
            let pos = drive1(START_POSITION, &[]);
            assert_eq!(pos, START_POSITION);
        }

        #[test]
        fn forward() {
            let pos = drive1(Position { x: 0, y: 10 }, &[Command::Forward(5)]);
            assert_eq!(pos, Position { x: 5, y: 10 });
        }

        #[test]
        fn up() {
            let pos = drive1(Position { x: 3, y: 5 }, &[Command::Up(3)]);
            assert_eq!(pos, Position { x: 3, y: 2 });
        }

        #[test]
        fn down() {
            let pos = drive1(Position { x: 3, y: 5 }, &[Command::Down(3)]);
            assert_eq!(pos, Position { x: 3, y: 8 });
        }
    }

    mod drive2 {
        use crate::day02::*;
        #[test]
        fn no_commands() {
            let (pos, aim) = drive2(START_POSITION, 0, &[]);
            assert_eq!(pos, START_POSITION);
            assert_eq!(aim, 0);
        }

        #[test]
        fn forward_no_aim() {
            let (pos, aim) = drive2(Position { x: 0, y: 10 }, 0, &[Command::Forward(5)]);
            assert_eq!(pos, Position { x: 5, y: 10 });
            assert_eq!(aim, 0);
        }

        #[test]
        fn forward_aiming_down() {
            let (pos, aim) = drive2(Position { x: 0, y: 0 }, 5, &[Command::Forward(5)]);
            assert_eq!(pos, Position { x: 5, y: 25 });
            assert_eq!(aim, 5);
        }

        #[test]
        fn forward_aiming_up() {
            let (pos, aim) = drive2(Position { x: 10, y: 100 }, -3, &[Command::Forward(5)]);
            assert_eq!(pos, Position { x: 15, y: 85 });
            assert_eq!(aim, -3);
        }

        #[test]
        fn up() {
            let (pos, aim) = drive2(Position { x: 3, y: 5 }, 0, &[Command::Up(3)]);
            assert_eq!(pos, Position { x: 3, y: 5 });
            assert_eq!(aim, -3);
        }

        #[test]
        fn down() {
            let (pos, aim) = drive2(Position { x: 3, y: 5 }, 0, &[Command::Down(3)]);
            assert_eq!(pos, Position { x: 3, y: 5 });
            assert_eq!(aim, 3);
        }
    }

    const EXAMPLE: &str = "
forward 5
down 5
forward 8
up 3
down 8
forward 2
";

    #[test]
    fn part1_example() {
        let result = part1(&parse(EXAMPLE));
        assert_eq!(result, 150);
    }

    #[test]
    fn part2_example() {
        let result = part2(&parse(EXAMPLE));
        assert_eq!(result, 900);
    }
}

pub fn run() {
    let input = parse(include_str!("./day02.input"));

    println!("day02 - part1: {}", part1(&input));
    println!("day02 - part2: {}", part2(&input));
}
