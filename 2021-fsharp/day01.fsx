let example = [
  199
  200
  208
  210
  200
  207
  240
  269
  260
  263
]

let countIncrements : int list -> int =
  let rec count n = function
    | a :: b :: rest when b > a -> count (n + 1) (b :: rest)
    | _ :: rest -> count n rest
    | [] -> n
  count 0

let part1 = countIncrements >> printfn "Part1: %A"

let sumsOfThree : int list -> int list =
  let rec calc sums = function
    | a :: b :: c :: rest -> calc (a+b+c::sums) (b::c::rest)
    | [] | [_] | [_;_] -> sums |> List.rev
  calc []

let part2 = sumsOfThree >> countIncrements >> printfn "Part2: %A"

open System.IO
let measurements =
  File.ReadAllLines "./day01.input"
  |> List.ofArray
  |> List.map int

part1 measurements
part2 measurements
