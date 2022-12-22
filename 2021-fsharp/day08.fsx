#load "./lib.fsx"
open Lib

let example_simple = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

let example_full =
    [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce" ]


let parse = function
  | ParseRegex "(.*) \| (.*)" [String wires; String digits] ->
    String.split ' ' wires, String.split ' ' digits
  | _ -> failwith "bad input"

let simple = function
  | 2 | 3 | 4 | 7 -> true
  | _ -> false

let part1 =
  List.map (snd >> List.map String.length >> List.filter simple >> List.length) >> List.sum

example_full |> List.map parse |> part1 |> Assert.must_be 26

// D         L round #
//---------------------
// 0|abc efg|6 1:2:
// 1|  c  f |2 1: :
// 2|a cde g|5 1:2:
// 3|a cd fg|5 1:2:
// 4| bcd f |4 1: :
// 5|ab d fg|5 1:2:
// 6|ab defg|6 1:2:
// 7|a c  f |3 1: :3
// 8|abcdefg|7 1: :
// 9|abcd fg|6 1:2:
//--------------------
//#1|8687497| all
//    ^  ^^  -> b,e,f
//#2|6x45xx6| L=5,6   
//     ^^    -> c,d
//#3|1xxxxx0| L=3
//   ^     ^ -> a,g
//
//  aaaa
// b    c
// b    c
//  dddd
// e    f
// e    f
//  gggg

let analyse wires =
  let freq =
    List.map String.chars
    >> List.fold (
      Seq.fold (fun chars ch -> 
        Map.change ch (function None -> Some (1) | Some (count) -> Some (count + 1)) chars
      )
    ) Map.empty<char, int>

  let of_length (lengths : Set<int>) = List.filter (String.length >> lengths.Contains)
  let except (unmapped : Set<char>) (count: Set<int>) = Map.filter (fun w c -> unmapped.Contains w && count.Contains c)
  let wired (mapping : Map<int,char>) =
    Map.toList >> List.choose (fun (from, count) -> mapping.TryFind count |> Option.map (fun to' -> from, to'))

  let unmapped = Set(String.chars "abcdefg")
  let mapped = []

  let mapped = wires |> freq |> wired (Map [3,'e'; 6,'b'; 9,'f'])
  let unmapped = mapped |> List.fold (fun unmapped' (from,_) -> unmapped' |> Set.remove from) unmapped

  let mapped2 =
    wires
    |> of_length (Set [5;6])
    |> freq
    |> except unmapped (Set [4;5])
    |> wired (Map [4,'c'; 5,'d'])
  let unmapped = mapped2 |> List.fold (fun unmapped' (from,_) -> unmapped' |> Set.remove from) unmapped
  
  let mapped3 =
    wires
    |> of_length (Set [3])
    |> freq
    |> except unmapped (Set [0;1])
    |> wired (Map [1,'a'])
  let unmapped = mapped3 |> List.fold (fun unmapped' (from,_) -> unmapped' |> Set.remove from) unmapped

  let mapped4 = [unmapped |> Set.toSeq |> Seq.head, 'g']

  let mapping = mapped @ mapped2 @ mapped3 @ mapped4 |> Map.ofList
  fun w -> mapping.[w]


let digits = [|
  "abcefg"
  "cf"
  "acdeg"
  "acdfg"
  "bcdf"
  "abdfg"
  "abdefg"
  "acf"
  "abcdefg"
  "abcdfg"
|]

let digit_lookup = 
  let table = digits |> Array.map (String.chars >> Set)
  fun d -> Array.findIndex ((=) d) table

let part2 =
  List.map (fun (wires, digits) ->
    let mapping = analyse wires
    digits
    |> List.map (String.chars >> Seq.map mapping >> Set >> digit_lookup)
    |> List.rev
    |> List.fold (fun (p, s) d -> p * 10, s + d * p) (1, 0)
    |> snd
  ) >> List.sum

[example_simple] |> List.map parse |> part2 |> Assert.must_be 5353

// example_full |> List.map parse |> part2 |> Assert.must_be 61229



open System.IO
let input =
  File.ReadAllLines "./day08.input"
  |> Array.toList |> List.map parse

input |> part1 |> printfn "part1: %A"
// input |> part2 |> printfn "part2: %A"
