module Assert =

  let must_be expected actual =
    if actual <> expected then
      printf "expected: %A\n     got: %A" expected actual
      exit 1
    else ()

let (|Integer|_|) (str : string) = try Some(int str) with _ -> None
let (|Char|_|) (str : string) = try Some(char str) with _ -> None
let (|String|_|) (str : string) = Some(str)
let (|ParseRegex|_|) regex str =
   let m = System.Text.RegularExpressions.Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None


module List =

  let rec combinations n l =
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

  let chunkBy p =
    let rec build chunks chunk = function
    | [] when chunk = [] -> chunks |> List.rev
    | [] -> List.rev chunk :: chunks |> List.rev
    | x :: xs when p x -> build chunks (x :: chunk) xs
    | x :: xs when chunk = [] -> build chunks chunk xs
    | x :: xs -> build (List.rev chunk :: chunks) [] xs
    build [] []


module String =

  let split by (str : string) = str.Split([|by|]) |> List.ofArray
  let chars (str : string) = str.ToCharArray()
