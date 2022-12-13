#load "./lib.fsx"
open Lib

(*                                 0 1 2 3 4 5 6 7 8  sum
Initial state: 3,4,3,1,2             1 1 2 1            5
After  1 day:  2,3,2,0,1           1 1 2 1              5
After  2 days: 1,2,1,6,0,8         1 2 1       1   1    6
After  3 days: 0,1,0,5,6,7,8       2 1       1 1 1 1    7
After  4 days: 6,0,6,4,5,6,7,8,8   1       1 1 3 1 2    9
After  5 days: 5,6,5,3,4,5,6,7,7,8       1 1 3 2 2 1   10
*)
let example = [ 3; 4; 3; 1; 2 ]

let simulate (days : int) (init : int list) : int64 =
  let rec tick day (daily : int64 list) =
    if day = days then
      List.sum daily
    else
      match daily with
      | [] -> 0
      | ready_to_breed :: daily' ->
        (daily' @ [ready_to_breed])
        |> List.mapi (fun day fishes -> fishes + if day = 6 then ready_to_breed else 0)
        |> tick (day + 1)
  let daily = Array.create 9 0L
  for fishes in init do
    daily[fishes] <- daily[fishes] + 1L
  daily |> Array.toList |> tick 0

let part1 = simulate 80
let part2 = simulate 256

example |> part1 |> Assert.must_be 5934
example |> part2 |> Assert.must_be 26984457539L

open System.IO
let input = File.ReadAllLines "./day06.input" |> Array.toList |> List.map (String.split ',' >> List.map int) |> List.concat

input |> part1 |> printfn "part1: %A"
input |> part2 |> printfn "part2: %A"
