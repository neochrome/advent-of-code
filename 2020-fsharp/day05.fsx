#load "./lib.fsx"
open Lib

let back (lo, hi) = (hi - lo) / 2 + lo + 1, hi
let front (lo, hi) = lo, hi - 1 - (hi - lo) / 2
let left = front
let right = back

let find_seat (input : string) =
  let rec decode rows cols = function
    | [] -> fst rows, fst cols
    | 'F' :: input' -> decode (front rows) cols input'
    | 'B' :: input' -> decode (back rows) cols input'
    | 'L' :: input' -> decode rows (left cols) input'
    | 'R' :: input' -> decode rows (right cols) input'
    | _ -> failwith "oh noes"
  input.ToCharArray() |> List.ofArray |> decode (0,127) (0,7)

let id_of (row, col) = row * 8 + col

"FBFBBFFRLR" |> find_seat |> id_of |> Assert.must_be 357
"FFFBBBFRRR" |> find_seat |> id_of |> Assert.must_be 119
"BBFFBBFRLL" |> find_seat |> id_of |> Assert.must_be 820
"BFFFBBFRRR" |> find_seat |> id_of |> Assert.must_be 567

open System.IO
let seat_ids = File.ReadAllLines "./day05.input" |> Array.map (find_seat >> id_of) |> Set.ofArray

let max_id = Set.maxElement seat_ids
printfn "Part1: %A" max_id

let min_id = Set.minElement seat_ids

let has_prev id = Set.contains (id - 1) seat_ids
let has_next id = Set.contains (id + 1) seat_ids
let is_missing id = not (Set.contains id seat_ids)

seq { min_id + 1 .. max_id - 1 }
|> Seq.find (fun id -> has_prev id && has_next id && is_missing id)
|> printfn "Part2: %A"
