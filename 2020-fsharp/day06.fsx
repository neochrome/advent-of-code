#load "./lib.fsx"
open Lib

let example = String.split '\n' "abc

a
b
c

ab
ac

a
a
a
a

b"

type answers = char Set
type group = answers list
let as_groups : string list -> group list = List.chunkBy ((<>) "") >> List.map (List.map (String.chars >> Set.ofArray))
let count_in_group how = List.reduce how >> Set.count
let any = Set.union
let all = Set.intersect

let part1 = List.map (count_in_group any) >> List.sum
let part2 = List.map (count_in_group all) >> List.sum

example |> as_groups |> part1 |> Assert.must_be 11
example |> as_groups |> part2 |> Assert.must_be 6

open System.IO
let groups = File.ReadAllLines "./day06.input" |> List.ofArray |> as_groups

groups |> part1 |> printfn "Part1: %A"
groups |> part2 |> printfn "Part2: %A"
