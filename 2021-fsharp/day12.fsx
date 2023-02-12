#load "./lib.fsx"
open Lib

type Pos = int * int
type Grid = Set<Pos>
type Fold = Horizontal of int | Vertical of int

let parse_dots : string seq -> Pos seq =
  Seq.choose (function ParseRegex "(\d+),(\d+)" [Integer x; Integer y] -> Some(Pos(x,y)) | _ -> None)

let parse_folds : string seq -> Fold seq =
  Seq.choose(function
    | ParseRegex "fold along y=(\d+)" [Integer y] -> Some(Horizontal y)
    | ParseRegex "fold along x=(\d+)" [Integer x] -> Some(Vertical x)
    | _ -> None
  )

let parse (lines : string seq) =
  parse_dots lines |> Set.ofSeq, parse_folds lines


let example = (parse << (String.split '\n')) """
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
"""

let dump (grid: Grid) =
  let w = grid |> Set.toSeq |> Seq.map fst |> Seq.max
  let h = grid |> Set.toSeq |> Seq.map snd |> Seq.max
  for y = 0 to h do
    for x = 0 to w do
      if grid.Contains (x,y) then
        printf "#"
      else
        printf "."
    printfn ""

let fold (grid: Grid) = function
  | Horizontal at ->
    let top,bottom = Set.partition (snd >> ((>) at)) grid
    bottom
    |> Set.map (fun (x,y) -> x, 2*at-y)
    |> Set.union top
  | Vertical at ->
    let left,right = Set.partition (fst >> ((>) at)) grid
    right
    |> Set.map (fun (x,y) -> 2*at-x, y)
    |> Set.union left

example
|> fst
|> fold <| Horizontal 7
|> Set.count
|> Assert.must_be 17

example
|> fst
|> fold <| Horizontal 7
|> fold <| Vertical 5
|> Set.count
|> Assert.must_be 16


let part1 (grid : Grid, folds : Fold seq) =
  folds
  |> Seq.take 1
  |> Seq.fold fold grid
  |> Set.count

let part2 (grid: Grid, folds: Fold seq) =
  folds
  |> Seq.fold fold grid
  |> dump

example |> part1 |> Assert.must_be 17

open System.IO
let input = File.ReadAllLines "./day13.input" |> parse

input |> part1 |> printfn "part1: %A"
input |> part2
// ####...##..##..#..#...##..##...##..#..#
// #.......#.#..#.#..#....#.#..#.#..#.#..#
// ###.....#.#..#.####....#.#....#..#.####
// #.......#.####.#..#....#.#.##.####.#..#
// #....#..#.#..#.#..#.#..#.#..#.#..#.#..#
// #.....##..#..#.#..#..##...###.#..#.#..#
// FJAHJGAH
