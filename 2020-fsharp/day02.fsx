#load "./lib.fsx"
open Lib

let example = [
  "1-3 a: abcde"
  "1-3 b: cdefg"
  "2-9 c: ccccccccc"
]

let parse = function
  | ParseRegex "(\d+)-(\d+) (\w): (\w+)" [Integer f; Integer t; Char c; String s] ->
    f,t,c,s
  | str -> failwith (sprintf "unparseable %A" str)

let count ch = String.filter ((=) ch) >> String.length
let part1 (min,max,ch,str) =
  let c = count ch str
  c >= min && c <= max

example
|> List.map parse
|> List.filter part1
|> List.length
|> Assert.must_be 2

open System.IO
File.ReadAllLines "./day02.input"
|> List.ofArray
|> List.map parse
|> List.filter part1
|> List.length
|> printfn "Part1: %A"

let part2 (p1 : int, p2 : int, ch : char, str : string) =
  (str.[p1 - 1] = ch) <> (str.[p2 - 1] = ch)

example
|> List.map parse
|> List.filter part2
|> List.length
|> Assert.must_be 1

File.ReadAllLines "./day02.input"
|> List.ofArray
|> List.map parse
|> List.filter part2
|> List.length
|> printfn "Part2: %A"
