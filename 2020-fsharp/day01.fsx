#load "./lib.fsx"
open Lib

let example = [
  1721
  979
  366
  299
  675
  1456
]

let is2020 = List.reduce (+) >> (=) 2020

example
|> List.combinations 2
|> List.find is2020
|> List.reduce (*)
|> Assert.must_be 514579

open System.IO
File.ReadAllLines "./day01.input"
|> List.ofArray
|> List.map int
|> List.combinations 2
|> List.find is2020
|> List.reduce (*)
|> printfn "Part1: %d"

File.ReadAllLines "./day01.input"
|> List.ofArray
|> List.map int
|> List.combinations 3
|> List.find is2020
|> List.reduce (*)
|> printfn "Part2: %d"
