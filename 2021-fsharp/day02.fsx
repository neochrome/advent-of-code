#load "./lib.fsx"
open Lib

let example = [
  "forward 5"
  "down 5"
  "forward 8"
  "up 3"
  "down 8"
  "forward 2"
]

type command =
  | Forward of int
  | Down of int
  | Up of int

type position = {
  x : int
  y : int
  aim: int
}

let start_pos = { x = 0; y = 0; aim = 0 }

let parse = function
   
  | ParseRegex "forward (\d+)" [Integer n] -> Forward n
    
  | ParseRegex "down (\d+)" [Integer n] -> Down n
    
  | ParseRegex "up (\d+)" [Integer n] -> Up n
  | str -> failwith (sprintf "unparseable %A" str)
   
   
let part1 p = function
  | Forward n -> { p with x = p.x + n }
  | Down n -> { p with y = p.y + n }
  | Up n -> { p with y = p.y - n }
  
let part2 p = function
  | Up n -> { p with aim = p.aim - n }
  | Down n -> { p with aim = p.aim + n }
  | Forward n -> { p with x = p.x + n; y = p.y + p.aim * n }


let drive strategy = List.fold strategy start_pos >> fun p -> p.x * p.y

example
|> List.map parse
|> drive part1
|> Assert.must_be 150

example
|> List.map parse
|> drive part2
|> Assert.must_be 900

open System.IO
let commands =
  File.ReadAllLines "./day02.input"
  |> List.ofArray
  |> List.map parse

commands |> drive part1 |> printfn "part1: %A"
commands |> drive part2 |> printfn "part2: %A"
