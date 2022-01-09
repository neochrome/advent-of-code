#use "./lib.ml";;

module Intcode = struct
  let execute program input =
    let mem = Array.copy program in
    let read addr = mem.(addr) in
    let pos addr = read addr in
    let imm addr = addr in
    let write addr value = mem.(pos addr) <- value in

    let rec step pc input output =
      let ins = read pc in
      let arg n =
        let mode = function
          | 0 -> pos (pc+n) | 1 -> imm (pc+n)
          | m -> failwith (Printf.sprintf "invalid mode: %d" m)
        in
        match n with
        | 1 -> mode (ins / 100 mod 10)
        | 2 -> mode (ins / 1000 mod 10)
        | 3 -> mode (ins / 10000 mod 10)
        | n -> failwith (Printf.sprintf "invalid argument position: %d" n)
      in
      let op_code = ins mod 100 in
      if op_code = 99 then List.rev output
      else
        match op_code with
        | 1 -> (read (arg 1)) + (read (arg 2)) |> write (pc+3); step (pc+4) input output
        | 2 -> (read (arg 1)) * (read (arg 2)) |> write (pc+3); step (pc+4) input output
        | 3 ->
          begin match input with
          | [] -> failwith "no more input available"
          | v::input' ->
            let () = write (pc+1) v in
            step (pc+2) input' output
          end
        | 4 -> read (arg 1) :: output |> step (pc+2) input
        | 5 -> step (if read (arg 1) <> 0 then read (arg 2) else pc+3) input output
        | 6 -> step (if read (arg 1) = 0 then read (arg 2) else pc+3) input output
        | 7 ->
          let () = (if read (arg 1) < read (arg 2) then 1 else 0) |> write (pc+3) in
          step (pc+4) input output
        | 8 ->
          let () = (if read (arg 1) = read (arg 2) then 1 else 0) |> write (pc+3) in
          step (pc+4) input output
        | o -> failwith (Printf.sprintf "invalid opcode: %d" o)
    in
    mem, step 0 input []

end

let _test =
  let mem, out = Intcode.execute [|1002;4;3;4;33|] [] in
  let () = assert (mem = [|1002;4;3;4;99|]) in

  let mem, out = Intcode.execute [|3;9;8;9;10;9;4;9;99;-1;8|] [8] in
  let () = assert (out = [1]) in
  ()

let () =
  File.open_in "./day05.input" (fun ch ->
    let program = Seq.of_lines ch
      |> List.of_seq |> List.hd
      |> String.split_on_char ',' |> List.map int_of_string
      |> Array.of_list
    in
    let () =
      let mem, out = Intcode.execute program [1] in
      out |> List.rev |> List.hd |> Printf.printf "part1: %d\n%!"
    in
    let () =
      let mem, out = Intcode.execute program [5] in
      out |> List.rev |> List.hd |> Printf.printf "part2: %d\n%!"
    in
    ()
  )
