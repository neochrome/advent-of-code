#load "./lib.fsx"
open Lib

let example = [
    "light red bags contain 1 bright white bag, 2 muted yellow bags."
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    "bright white bags contain 1 shiny gold bag."
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
    "faded blue bags contain no other bags."
    "dotted black bags contain no other bags."
  ]

let parse_contained = function ParseRegex "(\d+) (\w+ \w+)" [Integer n; String s] -> Some (s,n) | _ -> None

let parse (line : string) =
  match line.Split(" bags contain ") |> List.ofArray with
  | color :: contained :: [] ->
    let bags = contained.Split("bag") |> Array.choose parse_contained |> List.ofArray
    color, bags
  | _ -> failwith "not worthy"


let part1 bags =
  let rec bags_in (bag : string) : string list =
    let bags' = bags |> Map.find bag |> List.map (fst)
    let bags'' = [bag] :: List.map bags_in bags'
    List.concat bags''
  bags
  |> Map.keys
  |> Seq.map (bags_in)
  |> Seq.filter (List.contains "shiny gold")
  |> Seq.map (List.head)
  |> Seq.except ["shiny gold"]
  |> Seq.distinct
  |> Seq.length


let part2 bags =
  let rec count_in bag =
    bags
    |> Map.find bag
    |> List.sumBy (fun (bag', n) -> n * (1 + count_in bag'))
  count_in "shiny gold"

let example_bags = example |> List.map parse |> Map.ofList
part1 example_bags |> Assert.must_be 4
part2 example_bags |> Assert.must_be 32

open System.IO
let bags = File.ReadAllLines "./day07.input" |> List.ofArray |> List.map parse |> Map.ofList
part1 bags |> printfn "Part1: %A"
part2 bags |> printfn "Part2: %A"
