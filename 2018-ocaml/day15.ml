#use "./ext.ml";;

type space =
  | Wall
  | Empty
  | Unit

type unit = { x : int; y : int; hp : int }

let distance { x=sx;y=sy } { x=tx;y=ty } = abs (sx - tx) + abs (sy - ty)


