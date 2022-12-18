#load "./lib.fsx"
open Lib

let example = [16;1;2;0;4;2;7;1;2;14]

let solve cost initial_positions =
  let min = initial_positions |> List.min
  let max = initial_positions |> List.max

  let starting_distance = initial_positions |> List.map (fun p -> min - p)

  let move p = starting_distance |> List.map ((+) p >> abs >> cost) |> List.sum
  [ min..max ]
  |> List.map (fun p -> p, move p)
  |> List.minBy snd
  |> snd

let part1 = solve id
let part2 = solve (fun distance -> distance * (distance + 1) / 2)

example |> part1 |> Assert.must_be 37
example |> part2 |> Assert.must_be 168

open System.IO
let input =
  File.ReadAllLines "./day07.input"
  |> Array.toList
  |> List.map (String.split ',' >> List.map int)
  |> List.concat

input |> part1 |> printfn "part1: %A"
input |> part2 |> printfn "part2: %A"
