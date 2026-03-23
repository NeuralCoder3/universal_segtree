(*

- range query
- base type
- combine operation (associative)
- point update

- optional: range update (lazy, needs update type, combine, apply)

*)


module type Base = sig
  type t
  val default : t (* identity for combine -- neutral element *)
  val combine : t -> t -> t (* associative *)
  val apply : t -> t -> t
end

module type Updater = sig
  type base_t
  type update
  val compose : update -> update -> update
  (* also on composed leafs *)
  val apply_update : base_t -> update -> base_t
end

module type Optional_Updater = sig
  type t 
  val base : (module Base with type t = t) 
  val updater : (module Updater with type base_t = t) option
end



  (* type tree = Leaf of (int*B.t) | Node of ((int*int*int)*B.t*tree*tree)

  let get_value = function
    | Leaf (_, v) -> v
    | Node ((_, _, _), v, _, _) -> v

  let rec init (min,max) =
    if min = max then Leaf (min, B.default)
    else
      let mid = (min + max) / 2 in
      let sub_tree = init (min, mid) in
      let sub_tree' = init (mid+1, max) in
      let value = B.combine (get_value sub_tree) (get_value sub_tree') in
      Node ((min, mid, max), value, sub_tree, sub_tree') *)

  (*
      we use key 0..n-1 
      one could annotate keys manually
      the index can be computed on the fly
  *)


(* 
  use int key for now 
  ordered, mid point
*)
module SegmentTree (U:Optional_Updater) = struct

  module B = (val U.base : Base with type t = U.t)

  type tree = Leaf of B.t | Node of (B.t * tree * tree)

  let get_value = function
    | Leaf v -> v
    | Node (v, _, _) -> v

  let rec init n =
    if n = 1 then Leaf B.default
    else
      let sub_tree = init (n/2) in
      let sub_tree' = 
        if n - n / 2 = n / 2 then sub_tree else
        init (n - n/2) 
      in
      let value = B.combine (get_value sub_tree) (get_value sub_tree') in
      assert (value = B.default);
      Node (value, sub_tree, sub_tree')

  let rec query tree i j =
    match tree with
    | Leaf v -> (assert (i = 0 && j = 0); v)
    | Node (v, left, right) ->
      let mid = (j - i + 1) / 2 in
      if j < mid then query left i j
      else if i >= mid then query right (i - mid) (j - mid)
      else B.combine (query left i (mid-1)) (query right 0 (j - mid))

  let rec update tree i v =
    match tree with
    | Leaf _ -> (assert (i = 0); Leaf v)
    | Node (_, left, right) ->
      let mid = (i + 1) / 2 in
      if i < mid then
        let left' = update left i v in
        let value = B.combine (get_value left') (get_value right) in
        Node (value, left', right)
      else
        let right' = update right (i - mid) v in
        let value = B.combine (get_value left) (get_value right') in
        Node (value, left, right')

  (* let rec range_update tree i j v =
    match tree with *)

end