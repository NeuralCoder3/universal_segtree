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

module DummyUpdater (B:Base) = struct
  type base_t = B.t
  type update = unit
  let compose () () = ()
  (* let apply_update v () = failwith "no range updates supported" *)
  let apply_update v () = v
end

(* 
  use int key for now 
  ordered, mid point
*)
module SegmentTree (B:Base) (U:Updater with type base_t = B.t) = struct

  type tree = Leaf of B.t | Node of (B.t * U.update option * tree * tree)

  let get_value = function
    | Leaf v -> v
    | Node (v, _, _, _) -> v

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
      Node (value, None, sub_tree, sub_tree')

  (* TODO: do I need to propagate down? *)
  let rec query tree i j =
    match tree with
    | Leaf v -> (assert (i = 0 && j = 0); v)
    | Node (v, u, left, right) ->
      let mid = (j - i + 1) / 2 in
      let value =
        if j < mid then query left i j
        else if i >= mid then query right (i - mid) (j - mid)
        else B.combine (query left i (mid-1)) (query right 0 (j - mid))
      in
      match u with
      | None -> value
      | Some u -> U.apply_update value u

  (* point update *)
  let rec update tree i f =
    match tree with
    | Leaf v -> (assert (i = 0); Leaf (f v))
    | Node (_, None, left, right) ->
      let mid = (i + 1) / 2 in
      if i < mid then
        let left' = update left i f in
        let value = B.combine (get_value left') (get_value right) in
        Node (value, None, left', right)
      else
        let right' = update right (i - mid) f in
        let value = B.combine (get_value left) (get_value right') in
        Node (value, None, left, right')
    | Node (_, _, left, right) ->
        failwith "not implemented: point update on tree with pending range update"

end