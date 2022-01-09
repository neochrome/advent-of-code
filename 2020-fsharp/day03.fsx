#load "./lib.fsx"
open Lib

let example = [|
  "..##......."
  "#...#...#.."
  ".#....#..#."
  "..#.#...#.#"
  ".#...##..#."
  "..#.##....."
  ".#.#.#....#"
  ".#........#"
  "#.##...#..."
  "#...##....#"
  ".#..#...#.#"
|]


let traverse (map : string array) (sx,sy) =
  let rec step x y trees =
    if y >= map.Length then
      trees
    else
      let trees' =
        let row = map.[y]
        if row.[x % row.Length] = '#' then
          trees + 1
        else
          trees
      step (x + sx) (y + sy) trees'
  step 0 0 0

traverse example (3,1) |> Assert.must_be 7

open System.IO
let map = File.ReadAllLines "./day03.input"

traverse map (3,1) |> printfn "Part1: %A"

[
  1,1
  3,1
  5,1
  7,1
  1,2
]
|> List.map (traverse map)
|> List.reduce (*)
|> printfn "Part2: %A"
