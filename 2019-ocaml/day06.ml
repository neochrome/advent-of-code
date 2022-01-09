#use "./lib.ml";;

let parse =
  String.split_on_char ')'
  >> function
    | [parent;child] -> parent,child
    | _ -> failwith "invalid line"

module StringGraph = Graph.Make(String)

let part1 = StringGraph.fold (fun acc d _ -> acc + d) 0 "COM"

let _part1_example =
  [
    "COM)B";
    "B)C";
    "C)D";
    "D)E";
    "E)F";
    "B)G";
    "G)H";
    "D)I";
    "E)J";
    "J)K";
    "K)L";
  ]
  |> List.map parse
  |> StringGraph.of_list
  |> part1
  |> fun sum -> assert (sum = 42)

module StringSet = Set.Make(String)

let part2 graph =
  let path_to dst =
    graph
    |> StringGraph.path_to ((=) dst) "COM"
    |> StringSet.of_list
    |> StringSet.remove dst
  in
  let you = path_to "YOU" in
  let san = path_to "SAN" in
  let common = StringSet.inter you san in
  (* <COM-B-C-D> (I) <SAN> *)
  (* <COM-B-C-D> (E-J-K) <YOU> *)
  StringSet.(
    common
    |> diff (union you san)
    |> cardinal
  )

let _part2_example =
  [
    "COM)B";
    "B)C";
    "C)D";
    "D)E";
    "E)F";
    "B)G";
    "G)H";
    "D)I";
    "E)J";
    "J)K";
    "K)L";
    "K)YOU";
    "I)SAN";
  ]
  |> List.map parse
  |> StringGraph.of_list
  |> part2
  |> fun dist -> assert (dist = 4)

let () =
  File.open_in "./day06.input" (fun ch ->
    let graph =
      Seq.of_lines ch
      |> Seq.map parse
      |> List.of_seq
      |> StringGraph.of_list
    in
    let () = part1 graph |> Printf.printf "part1: %d\n%!" in
    let () = part2 graph |> Printf.printf "part2: %d\n%!" in
    ()
  )
