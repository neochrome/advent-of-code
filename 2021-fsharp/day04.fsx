#load "./lib.fsx"
open Lib

module Board =
  type Position = int * int
  type t = Set<int> list

  let mark (n: int) (board: t) : t =
    board |> List.map (Set.remove n)

  let score (board: t) : int option =
    if board |> List.contains Set.empty then
      board
      |> List.except Set.empty
      |> List.reduce Set.union
      |> Set.fold (+) 0
      |> Some
    else
      None

  let build (numbers : int seq) : t =
    if Seq.length numbers <> 25 then
      failwith "must be exactly 25 numbers on a board"

    let rows = numbers |> Seq.chunkBySize 5 |> Seq.map Set
    let cols = numbers |> Seq.chunkBySize 5 |> Seq.transpose |> Seq.map Set
    seq {
      yield! rows
      yield! cols
    } |> Seq.toList


let example = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"

let parse (input : string seq) =
  let numbers = Seq.head input |> String.split ',' |> List.map int 
  let boards =
    input
    |> Seq.skip 1
    |> Seq.chunkBySize 6
    |> Seq.map (
      Seq.map (
        String.split ' '
        >> Seq.filter (String.length >> (<>) 0)
        >> Seq.map int
      ) >> Seq.concat
    )
    |> Seq.map Board.build
    |> Seq.toList
  numbers, boards


let rec part1 (numbers, boards) =
  match numbers with
  | num :: numbers' ->
    let boards' = boards |> List.map (Board.mark num)
    match boards' |> List.choose Board.score with
    | [score] -> score * num
    | _ -> part1 (numbers', boards')
  | [] -> failwith "no one wins"


let rec part2 (numbers, boards) =
  match numbers with
  | num :: numbers' ->
    let no_wins =
      boards
      |> List.map (Board.mark num)
      |> List.filter (Board.score >> Option.isNone)
    match no_wins with
    | [] -> failwith "no single last board"
    | [board] ->
      numbers'
      |> List.scan (fun (board', _) num' ->
        let board'' = board' |> Board.mark num'
        let score'' = board'' |> Board.score |> Option.map ((*) num')
        board'', score''
      ) (board, None)
      |> List.pick (snd)
    | boards' -> part2 (numbers', boards')
  | [] -> failwith "no one wins"


example
|> String.split '\n'
|> parse
|> part1
|> Assert.must_be 4512

example
|> String.split '\n'
|> parse
|> part2
|> Assert.must_be 1924

open System.IO
let input = File.ReadAllLines "./day04.input" |> parse
input |> part1 |> printfn "part1: %A"
input |> part2 |> printfn "part2: %A"
