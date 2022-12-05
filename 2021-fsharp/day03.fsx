#load "./lib.fsx"
open Lib

let example = List.map BitArray.from_string <| [
  "00100"
  "11110"
  "10110"
  "10111"
  "10101"
  "01111"
  "00111"
  "11100"
  "10000"
  "11001"
  "00010"
  "01010"
] 


let most_common_bits =
  List.map (Array.map (function false -> -1 | true -> +1))
  >> List.reduce (Array.map2 (+))
  >> Array.map (fun n -> n >= 0)

let least_common_bits = most_common_bits >> BitArray.invert

let part1 =
  most_common_bits
  >> (fun bits ->
    let gamma = bits |> BitArray.value_of
    let epsilon = bits |> BitArray.invert |> BitArray.value_of
    gamma * epsilon
  )

let part2 report =
  let search (pattern_fn : BitArray.t list -> BitArray.t) =
    let rec aux bit_pos = function
      | [r] -> r
      | report' ->
        let pattern = pattern_fn report'
        aux (bit_pos + 1) (List.filter (fun r -> r[bit_pos] = pattern[bit_pos]) report')
    aux 0 >> BitArray.value_of
  let generator = search most_common_bits report
  let scrubber = search least_common_bits report
  generator * scrubber


example |> part1 |> Assert.must_be 198
example |> part2 |> Assert.must_be 230

open System.IO
let report =
  File.ReadAllLines "./day03.input"
  |> List.ofArray
  |> List.map BitArray.from_string

report |> part1 |> printfn "part1: %A"
report |> part2 |> printfn "part2: %A"
