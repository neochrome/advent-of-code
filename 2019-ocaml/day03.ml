#use "./lib.ml";;

let parse_segment segment = Scanf.sscanf segment "%c%d" (fun dir dist -> (dir,dist))

let parse path =
  path
  |> String.split_on_char ','
  |> List.map parse_segment

module Cell = struct
  type t = int * int * int
  let compare (ax,ay,_) (bx,by,_) =
    match Int.compare ax bx with
    | 0 -> Int.compare ay by
    | n -> n
end
module Cells = Set.Make(Cell)

let follow path =
  let rec walk (x,y,s) cells (dx,dy) =
    function
    | 0 -> (x,y,s),cells
    | dist -> let cell = (x+dx,y+dy,s+1) in walk cell (Cells.add cell cells) (dx,dy) (dist-1)
  in
  List.fold_left (fun (cell,cells) (dir,dist) ->
    match dir with
    | 'U' -> walk cell cells (0,1) dist
    | 'R' -> walk cell cells (1,0) dist
    | 'D' -> walk cell cells (0,-1) dist
    | 'L' -> walk cell cells (-1,0) dist
    | _ -> failwith "bad direction"
  ) ((0,0,0),Cells.empty) path
  |> fun (_,cells) -> cells

let distance (x,y) = (abs x) + (abs y)

let intersections_of wire1_path wire2_path =
  let wire1 = parse wire1_path |> follow in
  let wire2 = parse wire2_path |> follow in
  Cells.(
    inter wire1 wire2
    |> remove (0,0,0)
    |> elements
  )

let part1 = List.map (fun (x,y,_) -> distance (x,y)) >> List.min

let _part1_examples =
  assert (intersections_of "R8,U5,L5,D3" "U7,R6,D4,L4" |> part1 = 6);
  assert (intersections_of "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" |> part1 = 159);
  assert (intersections_of "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |> part1 = 135)

let part2 = List.map (fun (_,_,s) -> s) >> List.min

(* let _part2_examples = *)
(*   assert (intersections_of "R8,U5,L5,D3" "U7,R6,D4,L4" |> part2 = 30); *)
(*   assert (intersections_of "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" |> part2 = 610); *)
(*   assert (intersections_of "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |> part2 = 410) *)

let () =
  File.open_in "./day03.input" (fun ch ->
    let wire1,wire2 =
      Seq.of_lines ch
      |> List.of_seq
      |> function
        | [w1;w2] -> w1,w2
        | _ -> failwith "bad input"
    in
    let intersections = intersections_of wire1 wire2 in
    let () = part1 intersections |> Printf.printf "part1: %d\n%!" in
    let () = part2 intersections |> Printf.printf "part2: %d\n%!" in
    ()
  )
