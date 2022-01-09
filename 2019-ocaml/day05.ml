#use "./lib.ml";;
#use "./intcode.ml";;

let () =
  File.open_in "./day05.input" (fun ch ->
    let program = Seq.of_lines ch
      |> List.of_seq |> List.hd
      |> String.split_on_char ',' |> List.map int_of_string
      |> Array.of_list
    in
    let () =
      let out = Intcode.(execute program [1] |> output |> List.hd) in
      out |> Printf.printf "part1: %d\n%!"
    in
    let () =
      let out = Intcode.(execute program [5] |> output |> List.hd) in
      out |> Printf.printf "part2: %d\n%!"
    in
    ()
  )
