let bind f g x = g (f x)
let (>>) = bind

module Option = struct

  let none = None
  let some x = Some x

  let is_none = function None -> true | _ -> false
  let is_some = function None -> false | _ -> true

  let value = function
    | None -> raise (Invalid_argument "Must not be None")
    | Some x -> x

  let value_default default = function
    | None -> default
    | Some x -> x

  let map f = function
    | None -> None
    | Some x -> f x |> some

  let orSome y = function
    | None -> some y
    | x -> x

end

module Range = struct

  let rec iter b e f =
    if b > e then ()
    else begin f (b); iter (b + 1) e f end

  let rec fold b e f a =
    if b > e then a
    else fold (b + 1) e f (f a b)

end

module File = struct

  let open_in filename fn =
    let ch = open_in filename in
    try
      let res = fn ch in
      close_in ch;
      res
    with e ->
      close_in ch;
      raise e

end

module Seq = struct
  include Seq

  let of_lines ch =
    let rec next () =
      try Cons (input_line ch, next)
      with End_of_file -> Nil
    in next
end

module String = struct
  include String

  let to_chars s =
    let rec build n l =
      if n = 0 then l
      else build (n - 1) (s.[n - 1] :: l)
    in
    build (length s) []

  let from_chars chars =
    init (List.length chars) (fun i -> List.nth chars i)

  let fold f a s =
    let last = length s in
    let rec build n a =
      if n = last then a
      else build (n + 1) (f a s.[n])
    in build 0 a

end

module List = struct
  include List

  let unique_pairs xs =
    let rec take acc = function
      | [] -> acc
      | curr :: next ->
        let acc' = next |> List.map (fun x -> curr, x) in
        take (acc' @ acc) next
    in take [] xs

  let reject f = filter (fun x -> not (f x))

  let min =
    let rec search = function
      | [] -> failwith "min requires a non-empty list"
      | [x] -> x
      | x :: xs -> Stdlib.min x (search xs)
    in search

  let max =
    let rec search = function
      | [] -> failwith "a non-empty list is required"
      | [x] -> x
      | x :: xs -> Stdlib.max x (search xs)
    in search

  let minmax cmp xs =
    let min a b = if cmp a b < 0 then a else b in
    let max a b = if cmp b a < 0 then a else b in
    let rec search mi ma = function
      | [] -> mi,ma
      | x :: xs -> search (min mi x) (max ma x) xs
    in match xs with
    | [] -> failwith "a non-empty list is required"
    | x :: xs -> search x x xs

  let minf f xs =
    let minf a b = if f a < f b then a else b in
    let rec search mx = function
      | [] -> mx
      | x :: xs -> search (minf mx x) xs
    in
    match xs with
    | [] -> failwith "a non-empty list is required"
    | x :: xs -> search x xs

  let maxf f xs =
    let maxf a b = if f a > f b then a else b in
    let rec search mx = function
      | [] -> mx
      | x :: xs -> search (maxf mx x) xs
    in
    match xs with
    | [] -> failwith "a non-empty list is required"
    | x :: xs -> search x xs

  let remove x = filter ((<>) x)

  let filter_map f xs =
    xs
    |> map f
    |> filter Option.is_some
    |> map Option.value

  let filter_mapi f xs =
    xs
    |> mapi f
    |> filter Option.is_some
    |> map Option.value

  let rec skip n = function
    | [] -> []
    | xs when n <= 0 -> xs
    | _ :: xs -> skip (n - 1) xs

  let take n xs =
    let rec build xs' n = function
      | [] -> xs'
      | _ when n <= 0 -> xs'
      | x :: xs -> build (x :: xs') (n - 1) xs
    in build [] n xs |> rev

  let chunk f xs =
    let add x (p,xs) = p,x::xs in
    let sorted (p,xs) = p,List.rev xs in
    let rec build chunks chunk = function
      | [] -> sorted chunk :: chunks |> List.rev
      | x :: xs ->
        let p = f x in
        if p = fst chunk
        then build chunks (chunk |> add x) xs
        else build (sorted chunk :: chunks) (p,[x]) xs
    in
    match xs with
    | [] -> []
    | x :: xs -> build [] (f x, [x]) xs

  let rec combine k lst =
    if k = 0 then [[]]
    else
      let rec inner =
        function
        | [] -> []
        | x::xs -> map (fun z -> x::z) (combine (k - 1) xs)::inner xs
      in concat (inner lst)

  let rec permutations =
    function
    | [] -> []
    | x::[] -> [[x]]
    | l ->
      fold_left (fun acc x ->
        acc @ map (fun p -> x::p) (permutations (remove x l))
      ) [] l

end

module Map = struct
  module type S = sig
    include Map.S
    val maxf: (key -> 'a -> 'b) -> 'a t -> key * 'a
    val find_default : key -> 'a -> 'a t -> 'a
    val keys : 'a t -> key list
  end

  module type OrderedType = Map.OrderedType

  module Make (Ord: Map.OrderedType) : S with type key = Ord.t = struct
    module Map = Map.Make(Ord)
    include Map

    let maxf f m =
      fold (fun key value acc ->
        let a' = f key value in
        match acc with
        | Some (a,_) when a' <= a -> acc
        | _ -> Some (a', (key,value))
      ) m None
      |> function
        | None -> failwith "no values"
        | Some (_,max) -> max

    let find_default k d m =
      match find_opt k m with
      | None -> d
      | Some x -> x

    let keys m = m |> bindings |> List.rev_map fst |> List.rev
  end
end

module Array = struct
  include Array

  let index_of_opt arr needle =
    let rec search i =
      if length arr - i < length needle then None
      else if check i 0 then Some i
      else search (i + 1)
    and check i j =
      if j = length needle then true
      else if arr.(i) <> needle.(j) then false
      else check (i + 1) (j + 1)
    in search 0

  let rindex_of_opt arr needle =
    let nend = length needle - 1 in
    let rec search i =
      if i < nend then None
      else if check i nend then Some (i - nend)
      else search (i - 1)
    and check i j =
      if j < 0 then true
      else if arr.(i) <> needle.(j) then false
      else check (i - 1) (j - 1)
    in search (length arr - 1)

end

module Graph = struct
  module type NodeType = sig
    include Map.OrderedType
  end
  module type S = sig
    type 'a t
    type node
    val of_list : (node * node) list -> node t
    val fold : ('a -> int -> node -> 'a) -> 'a -> node -> node t -> 'a
    val path_to : (node -> bool) -> node -> node t -> node list
  end
  module Make(Node: NodeType) : S with type node = Node.t = struct
    module M = Map.Make(Node)
    type 'a t = Node.t list M.t
    type node = Node.t

    let add_node p c =
      M.update p (function
        | None -> Some c
        | Some children -> Some (children @ c)
      )

    let of_list =
      List.fold_left (fun g (p,c) ->
        g |> add_node p [c] |> add_node c []
      ) M.empty

    let fold f seed start graph =
      let rec visit d a node =
        let children = M.find node graph in
        List.fold_left (visit (d+1)) (f a d node) children
      in visit 0 seed start

    let path_to f start graph =
      let children_of node = M.find node graph in
      let rec search node =
        if f node then [node]
        else
          match each (children_of node) with
          | [] -> []
          | path -> node::path
      and each = function
        | [] -> []
        | node::siblings ->
          begin match search node with
          | [] -> each siblings
          | path -> path
          end
      in search start

  end
end

