module Intcode = struct
  type status =
    | Running
    | Blocked
    | Halted

  let string_of_status = function
    | Running -> "running"
    | Blocked -> "blocked"
    | Halted -> "halted"

  type t = {
    mem : int array;
    pc : int;
    input : int list;
    output : int list;
    status : status;
  }

  let output state = state.output
  let peek_output state =
    match state.output with
    | [] -> None
    | last::_ -> Some last

  let pop_output state =
    match state.output with
      | [] -> None, state
      | last::output -> Some last, { state with output }

  let push_input value state = { state with input = state.input @ [value] }
  let mem state = state.mem
  let is_halted state = state.status = Halted
  let is_running state = state.status = Running
  let is_blocked state = state.status = Blocked

  let step ({ mem; pc; input; output; status } as state) =
    match status with
    | Halted -> state
    | Running | Blocked ->
      let read addr = mem.(addr) in
      let pos addr = read addr in
      let imm addr = addr in
      let write addr value = mem.(pos addr) <- value in
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
      let running pc = { state with pc; status = Running } in
      let halted = { state with status = Halted } in
      let blocked = { state with status = Blocked } in
      let op_code = ins mod 100 in
      if op_code = 99 then halted
      else
        match op_code with
        | 1 -> (read (arg 1)) + (read (arg 2)) |> write (pc+3); running (pc+4)
        | 2 -> (read (arg 1)) * (read (arg 2)) |> write (pc+3); running (pc+4)
        | 3 ->
          begin match input with
          | [] -> blocked
          | v::input -> write (pc+1) v; { (running (pc+2)) with input }
          end
        | 4 -> { (running (pc+2)) with output = read (arg 1)::output }
        | 5 -> running (if read (arg 1) <> 0 then read (arg 2) else pc+3)
        | 6 -> running (if read (arg 1) = 0 then read (arg 2) else pc+3)
        | 7 -> (if read (arg 1) < read (arg 2) then 1 else 0) |> write (pc+3); running (pc+4)
        | 8 -> (if read (arg 1) = read (arg 2) then 1 else 0) |> write (pc+3); running (pc+4)
        | o -> failwith (Printf.sprintf "invalid opcode: %d" o)

  let initialize program input =
    { mem = Array.copy program; pc = 0; input; output = []; status = Running }

  let rec execute_until_blocked state =
    if not (is_running state) then state
    else state |> step |> execute_until_blocked

  let execute program input =
    let rec iter state =
      if is_halted state then state
      else state |> step |> iter
    in initialize program input |> iter

  let to_string { mem; status; pc; input; output; } =
    let mem = mem |> Array.to_list |> List.map string_of_int |> String.concat "," in
    let input = input |> List.map string_of_int |> String.concat "," in
    let output = output |> List.map string_of_int |> String.concat "," in
    Printf.sprintf "%s: pc=%d, input: [%s], output: [%s]\n%s" (string_of_status status) pc input output mem

  let tests =
    let _day02_ =
      let () = assert (execute [|1;0;0;0;99|] [] |> mem = [|2;0;0;0;99|]) in
      let () = assert (execute [|2;3;0;3;99|] [] |> mem = [|2;3;0;6;99|]) in
      let () = assert (execute [|2;4;4;5;99;0|] [] |> mem = [|2;4;4;5;99;9801|]) in
      let () = assert (execute [|1;1;1;4;99;5;6;0;99|] [] |> mem = [|30;1;1;4;2;5;6;0;99|]) in
      ()
    in
    let _day05_ =
      let mem = execute [|1002;4;3;4;33|] [] |> mem in
      let () = assert (mem = [|1002;4;3;4;99|]) in

      let out = execute [|3;9;8;9;10;9;4;9;99;-1;8|] [8] |> output in
      let () = assert (out = [1]) in
      ()
    in
    ()

end
