#load "./lib.fsx"
open Lib

type ParseResult =
  | Valid
  | Incomplete of char list
  | Corrupted of char

let is_opening = function
  | '[' | '(' | '{' | '<' -> true
  | _ -> false

let closing = function
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | '<' -> '>'
  | p -> failwith (sprintf "invalid opening %A" p)

let corruption_score = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | p -> failwith (sprintf "invalid closing %A" p)

let completion_score =
  let score = function
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | p -> failwith (sprintf "invalid closing %A" p)
  Seq.fold (fun s ch -> s * 5L + score ch) 0L

[
  "}}]])})]", 288957
  ")}>]})", 5566
  "}}>}>))))", 1480781
  "]]}}]}]}>", 995444
  "])}>", 294
]
|> Seq.map (fun (str, score) -> str |> String.chars |> Array.toList |> completion_score, score)
|> Seq.iter (fun (a,e) -> a |> Assert.must_be e)



let parse (line : string) : ParseResult =
  let rec balance idx expected =
    if idx = line.Length then
      if expected = [] then
        Valid
      else
        Incomplete expected
    else
      match line.[idx], expected with
      | '[' | '(' | '{' | '<' as p, _ -> balance (idx + 1) (closing p :: expected)
      | p, cl :: expected' when p = cl -> balance (idx + 1) expected'
      | p, _ -> Corrupted p
  balance 0 []

"()" |> parse |> Assert.must_be Valid
"(]" |> parse |> Assert.must_be (Corrupted ']')
">" |> parse |> Assert.must_be (Corrupted '>')
"(" |> parse |> Assert.must_be (Incomplete [')'])
"(<[{}" |> parse |> Assert.must_be (Incomplete [']';'>';')'])
"[({(<(())[]>[[{[]{<()<>>" |> parse |> Assert.must_be (Incomplete ("}}]])})]" |> String.chars |> Array.toList))

let example =
  [
    "[({(<(())[]>[[{[]{<()<>>"
    "[(()[<>])]({[<{<<[]>>("
    "{([(<{}[<>[]}>{[]{[(<()>"
    "(((({<>}<{<{<>}{[]{[]{}"
    "[[<[([]))<([[{}[[()]]]"
    "[{[{({}]{}}([{[{{{}}([]"
    "{<[[]]>}<{[{[{[]{()[[[]"
    "[<(<(<(<{}))><([]([]()"
    "<{([([[(<>()){}]>(<<{{"
    "<{([{{}}[<[[[<>{}]]]>[]]"
  ] |> Seq.map parse

let part1 = 
  Seq.choose (function Corrupted ch -> Some ch | _ -> None)
  >> Seq.map corruption_score
  >> Seq.sum

let part2 =
  Seq.choose (function Incomplete exp -> Some exp | _ -> None)
  >> Seq.map completion_score
  >> Seq.sort
  >> Seq.toArray
  >> fun scores -> scores.[scores.Length / 2]

example |> part1 |> Assert.must_be 26397
example |> part2 |> Assert.must_be 288957

open System.IO
let parsed = 
  File.ReadAllLines "./day10.input"
  |> Seq.map parse

parsed |> part1 |> printfn "part1: %A"
parsed |> part2 |> printfn "part2: %A"
