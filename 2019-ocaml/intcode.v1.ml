module Intcode = struct
  type src =
    | Imm of int
    | Pos of int

  let src_of_int =
    function
    | 0 -> fun p -> Pos p
    | 1 -> fun v -> Imm v
    | m -> failwith (Printf.sprintf "unsupported mode: %d" m)

  type dst =
    | Pos of int

  type ins =
    | ADD of src * src * dst
    | MUL of src * src * dst
    | INP of dst
    | OUT of src
    | JIT of src * src
    | JIF of src * src
    | LES of src * src * dst
    | EQU of src * src * dst
    | HLT

  let ins_size =
    function
    | ADD _ | MUL _ -> 4
    | INP _ | OUT _ -> 2
    | JIT _ | JIF _ -> 3
    | LES _ | EQU _ -> 4
    | HLT -> 1

  let ins_of_int opcode =
    let m = opcode / 100 in
    let src = Array.map src_of_int [|m mod 10; m / 10 mod 10; m / 100 mod 10|] in
    let dst a = Pos a in
    match opcode mod 100 with
    | 99 -> fun _pc _mem -> HLT
    | 1 -> fun pc mem -> ADD (src.(0) mem.(pc+1), src.(1) mem.(pc+2), dst mem.(pc+3))
    | 2 -> fun pc mem -> MUL (src.(0) mem.(pc+1), src.(1) mem.(pc+2), dst mem.(pc+3))
    | 3 -> fun pc mem -> INP (dst mem.(pc+1))
    | 4 -> fun pc mem -> OUT (src.(0) mem.(pc+1))
    | 5 -> fun pc mem -> JIT (src.(0) mem.(pc+1), src.(1) mem.(pc+2))
    | 6 -> fun pc mem -> JIF (src.(0) mem.(pc+1), src.(1) mem.(pc+2))
    | 7 -> fun pc mem -> LES (src.(0) mem.(pc+1), src.(1) mem.(pc+2), dst mem.(pc+3))
    | 8 -> fun pc mem -> EQU (src.(0) mem.(pc+1), src.(1) mem.(pc+2), dst mem.(pc+3))
    | o -> failwith (Printf.sprintf "unsupported opcode: %d" o)

  let execute program input =
    let mem = Array.copy program in
    let read = function
      | Imm v -> v
      | Pos p -> mem.(p)
    in
    let write (Pos p) v = mem.(p) <- v in
    let rec step pc input output =
      let ins = ins_of_int mem.(pc) pc mem in
      let pc' = pc + (ins_size ins) in
      match ins with
      | HLT -> mem,output
      | ADD (arg1,arg2,dst) ->
        let () = (read arg1) + (read arg2) |> write dst in
        step pc' input output
      | MUL (arg1,arg2,dst) ->
        let () = (read arg1) * (read arg2) |> write dst in
        step pc' input output
      | INP dst ->
        begin match input with
        | [] -> failwith "no more input available"
        | v::input' ->
          let () = write dst v in
          step pc' input' output
        end
      | OUT src ->
        let v = read src in
        let output' = v :: output in
        step pc' input output'
      | LES (arg1,arg2,dst) ->
        let () = (if (read arg1) < (read arg2) then 1 else 0) |> write dst in
        step pc' input output
      | EQU (arg1,arg2,dst) ->
        let () = (if (read arg1) = (read arg2) then 1 else 0) |> write dst in
        step pc' input output
      | JIT (cmp,dst) ->
        if (read cmp) <> 0 then
          step (read dst) input output
        else
          step pc' input output
      | JIF(cmp,dst) ->
        if (read cmp) = 0 then
          step (read dst) input output
        else
          step pc' input output
    in
    let mem',output = step 0 input [] in
    mem',List.rev output

end
