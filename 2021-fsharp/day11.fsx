#load "./lib.fsx"
open Lib

type Pos = int * int // row / col
type Grid = Map<Pos, int>

let parse: string seq -> Grid =
    Seq.mapi (fun r -> String.chars >> Seq.map (string >> int) >> Seq.mapi (fun c e -> (r, c), e))
    >> Seq.concat
    >> Map.ofSeq

let inc = (+) 1
let (++) (r1, c1) (r2, c2) = r1 + r2, c1 + c2

let step (grid: Grid) : int * Grid =
    let surrounding = [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ]
    let intensify = Map.map (fun _ -> inc)
    let settle = Map.map (fun _ energy -> if energy > 9 then 0 else energy)

    let flash_single (p : Pos) : Grid -> Grid =
        List.foldBack (fun dir -> Map.change (p ++ dir) (Option.map inc)) surrounding

    let rec flash_all (flashed, grid) : Set<Pos> * Grid =
        let (flashed', grid') =
          grid
          |> Map.toSeq
          |> Seq.filter (function (pos, energy) -> not (Set.contains pos flashed) && energy > 9)
          |> Seq.fold (fun (flashed', grid') (pos, _) ->
              Set.add pos flashed', flash_single pos grid'
          ) (flashed, grid)
        if flashed' = flashed then
          (flashed, grid)
        else
          flash_all (flashed', grid')
    
    let flashed, grid' = flash_all (Set[], intensify grid)
    Set.count flashed, settle grid'


[
"11111"
"19991"
"19191"
"19991"
"11111"
] |> parse |> step |> fst |> Assert.must_be 9

let example =
  [
    "5483143223"
    "2745854711"
    "5264556173"
    "6141336146"
    "6357385478"
    "4167524645"
    "2176841721"
    "6882881134"
    "4846848554"
    "5283751526"
  ] |> parse

let part1 =
  let rec loop n flashes grid =
    if n = 0 then
      flashes
    else
      let flashes',grid' = step grid
      loop (n - 1) (flashes + flashes') grid'
  loop 100 0


let part2 =
  let rec loop n grid =
    let flashes, grid' = step grid
    if flashes = Map.count grid' then
      n
    else
      loop (n + 1) grid'
  loop 1


example |> part1 |> Assert.must_be 1656
example |> part2 |> Assert.must_be 195

open System.IO
let grid =
  File.ReadAllLines "./day11.input"
  |> parse

grid |> part1 |> printfn "part1: %A"
grid |> part2 |> printfn "part2: %A"
