#load "./lib.fsx"
open Lib

type Map = int array array
type Point = int * int
let up : Point = -1,0
let down : Point = 1,0
let left : Point = 0,-1
let right : Point = 0,1
let (++) (ar,ac) (br,bc) = ar+br,ac+bc

let parse_line = String.chars >> Seq.map (string >> int)

let pad map : Map =
  let pad_with = 9
  let padding = map |> Seq.head |> Seq.map (fun _ -> pad_with)
  let vpad = [ yield padding; yield! map; yield padding]
  let padded = [|
    for row in vpad ->
      [|
        yield pad_with
        for col in row -> col
        yield pad_with
      |]
  |]
  padded

let parse (input : string seq) : Map = input |> Seq.map parse_line |> pad

let example =
  [
    "2199943210"
    "3987894921"
    "9856789892"
    "8767896789"
    "9899965678"
  ] |> parse

let find_low_points (map : Map) : Point list =
  [
    for r in 1..(map.Length - 2) do
      for c in 1..(map.[r].Length - 2) do
        let p = map.[r].[c]
        let neighbors = [
          map.[r-1].[c]
          map.[r+1].[c]
          map.[r].[c-1]
          map.[r].[c+1]
        ]
        if neighbors |> List.forall ((<) p) then
          r,c
  ]

let measure_basin (map : Map) (start : Point) : int =
  let rec measure p ext =
    let r,c = p
    if map.[r].[c] = 9 || Set.contains p ext  then
      ext
    else
      ext
      |> Set.add p
      |> measure (p ++ up)
      |> measure (p ++ down)
      |> measure (p ++ left)
      |> measure (p ++ right)

  Set.empty
  |> measure start
  |> Set.count
 
measure_basin example (1,2) |> Assert.must_be 3

let part1 map =
  map
  |> find_low_points
  |> List.sumBy (fun (r,c) -> map.[r].[c] + 1)

let part2 map =
  map
  |> find_low_points
  |> Seq.map (measure_basin map)
  |> Seq.sortDescending
  |> Seq.take 3
  |> Seq.reduce (*)

example |> part1 |> Assert.must_be 15
example |> part2 |> Assert.must_be 1134

open System.IO
let map = 
  File.ReadAllLines "./day09.input"
  |> parse

map |> part1 |> printfn "part1: %A"
map |> part2 |> printfn "part2: %A"
