#use "./lib.ml";;
#use "./intcode.ml";;

let amp program input phase = Intcode.execute program [phase;input] |> Intcode.output |> List.hd

let circuit program = List.fold_left (amp program) 0

let _tests =
  let () = circuit [|3;15;3;16;1002;16;10;16;1;16;15;15;4;15;99;0;0|] [4;3;2;1;0] |> fun res -> assert (res = 43210) in
  let () = circuit [|3;23;3;24;1002;24;10;24;1002;23;-1;23;101;5;23;23;1;24;23;23;4;23;99;0;0|] [0;1;2;3;4] |> fun res -> assert (res = 54321) in
  let () = circuit [|3;31;3;32;1002;32;10;32;1001;31;-2;31;1007;31;0;33;1002;33;7;33;1;33;31;31;1;32;31;31;4;31;99;0;0;0|] [1;0;4;3;2] |> fun res -> assert (res = 65210) in
  ()

let part1 program =
  List.permutations [0;1;2;3;4]
  |> List.fold_left (fun signal phases -> circuit program phases |> max signal) 0

module Circuit = struct
  type t = Intcode.t list

  let amp program phase = Intcode.initialize program [phase]

  let initialize program phases = List.map (amp program) phases

  let output = List.rev >> List.hd >> Intcode.output >> List.hd

  let step input amps =
    let rec each updated_amps input = function
      | [] -> updated_amps |> List.rev
      | amp::amps' ->
        Intcode.to_string amp |> print_endline;
        let amp' = Intcode.(amp |> push_input input) in
        Intcode.to_string amp' |> print_endline;
        begin match Intcode.(amp' |> execute_until_blocked |> pop_output) with
        | None, _ -> failwith "no output!"
        | Some output, amp' ->
            Printf.printf "%d\n" output;
            each (amp'::updated_amps) output amps'
        end
    in each [] input amps

  let is_complete amps = List.for_all Intcode.is_halted amps

  (* let rec execute_until_complete amps = *)
  (*   if is_complete amps then amps *)
  (*   else *)
  (*     let amps' = step amps in *)
  (*     match output amps' with *)
  (*     | None -> "no output!" *)
  (*     | Some value -> *)
  (*  *)
  (*   |> execute_until_complete *)

end


let () =
  let c = Circuit.initialize [|3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5|] [9;8;7;6;5] in
  c |> Circuit.step 0 |> Circuit.output |> Printf.printf "%d\n"
  (* c |> Circuit.step |> Circuit.step |> Circuit.output |> Printf.printf "%d\n" *)


let () =
  File.open_in "./day07.input" (fun ch ->
    let program = Seq.of_lines ch
      |> List.of_seq |> List.hd
      |> String.split_on_char ',' |> List.map int_of_string
      |> Array.of_list
    in
    let () = part1 program |> Printf.printf "part1: %d\n%!" in
    ()
  )
