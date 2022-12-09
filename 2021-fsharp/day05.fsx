#load "./lib.fsx"
open Lib

type Pos = int * int
type Line = Pos * Pos
type Grid = Map<Pos, int>

let parse_line : string -> Line = function
  | ParseRegex "(\d+),(\d+) -> (\d+),(\d+)" [Integer x1; Integer y1; Integer x2; Integer y2] ->
    ((x1,y1), (x2,y2))
  | str -> failwith "bad input"

let example = Seq.map parse_line [
  "0,9 -> 5,9"
  "8,0 -> 0,8"
  "9,4 -> 3,4"
  "2,2 -> 2,1"
  "7,0 -> 7,4"
  "6,4 -> 2,0"
  "0,9 -> 2,9"
  "3,4 -> 1,4"
  "0,0 -> 8,8"
  "5,5 -> 8,2"
]

let rasterize (l: Line) : Pos seq =
  let b, e = l
  let x1,y1 = b
  let x2,y2 = e
  let dx = if x1 < x2 then 1 else if x1 > x2 then -1 else 0
  let dy = if y1 < y2 then 1 else if y1 > y2 then -1 else 0
  Seq.unfold (function
    | None -> None
    | Some p ->
      if p = e then
        Some (p, None)
      else
        let x,y = p
        let p' = x + dx, y + dy
        Some (p, Some p')
  ) (Some b)

((2,2),(0,0)) |> rasterize |> Seq.toList |> Assert.must_be [(2,2); (1,1); (0,0)]

let h_or_v (((x1,y1),(x2,y2)):Line) = x1 = x2 || y1 = y2

let plot (g: Grid) (p: Pos) : Grid =
  g |> Map.change p (Option.map ((+) 1) >> Option.orElse (Some 1))

let draw (g: Grid) (l: Line) =
  rasterize l
  |> Seq.fold plot g

let overlaps : Grid -> int = Map.filter (fun _ c -> c > 1) >> Map.count

let part1 : Line seq -> int = Seq.filter h_or_v >> Seq.fold draw (Grid []) >> overlaps

example |> part1 |> Assert.must_be 5

let part2 : Line seq -> int = Seq.fold draw (Grid []) >> overlaps

example |> part2 |> Assert.must_be 12

open System.IO
let input = File.ReadAllLines "./day05.input" |> Seq.map parse_line
input |> part1 |> printfn "part1: %A"
input |> part2 |> printfn "part2: %A"
