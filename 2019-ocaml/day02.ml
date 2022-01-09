#use "./lib.ml";;
#use "./intcode.ml";;

let run input noun verb =
  let input' = Array.copy input in
  let () = input'.(1) <- noun
  and () = input'.(2) <- verb
  in Intcode.(execute input' [] |> mem)

let rec search input noun verb =
  let res = run input noun verb in
  if res.(0) = 19690720 then (noun,verb)
  else
    match (noun,verb) with
    | 99,99 -> failwith "not able to find solution"
    | n,99 -> search input (n+1) 0
    | n,v -> search input n (v+1)

let () =
  File.open_in "./day02.input" (fun ch ->
    let input =
      Seq.of_lines ch
      |> Seq.map (String.split_on_char ',')
      |> List.of_seq
      |> List.flatten
      |> List.map int_of_string
      |> Array.of_list
    in
    let () =
      run input 12 02 |> fun res -> Printf.printf "part1: %d\n%!" res.(0)
    in
    let () =
      search input 0 0 |> fun (noun,verb) -> Printf.printf "part2: 100 * %d + %d = %d\n%!" noun verb (100*noun+verb)
    in
    ()
  )
