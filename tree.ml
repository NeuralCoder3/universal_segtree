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
  val empty : update (* None, identity for compose, apply *)
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

  type tree = Leaf of B.t | Node of (B.t * U.update * tree * tree)
  type segment_tree = { size : int; tree : tree }

  let get_value = function
    | Leaf v -> v
    | Node (v, _, _, _) -> v

  let rec init' n =
    if n = 1 then Leaf B.default
    else
      let sub_tree = init' (n/2) in
      let sub_tree' = 
        if n - n / 2 = n / 2 then sub_tree else
        init' (n - n/2) 
      in
      let value = B.combine (get_value sub_tree) (get_value sub_tree') in
      assert (value = B.default);
      Node (value, U.empty, sub_tree, sub_tree')

  let init n = { size = n; tree = init' n }

  (*
      either give size and subtract from query, or keep as is and track both bounds
  *)

  (* TODO: do I need to propagate down? *)
  (* let rec query' lower upper tree i j =
    match tree with
    | Leaf v -> (assert (lower = upper && j = 0); v)
    | Node (v, update, left, right) ->
      let mid = (upper - lower + 1) / 2 in
      let value =
        if j < mid then query' lower mid left i j
        else if i >= mid then query' mid upper right i j
        else 
          B.combine 
            (query' lower mid left i (mid-1))
            (query' mid upper right mid j)
      in
      U.apply_update value update *)

let query tree i j = query' 0 tree.size tree.tree i j

  let apply_update tree u =
    match tree with
    | Leaf v -> Leaf (U.apply_update v u)
    | Node (v, u', left, right) ->
      (* the older update is father down as we never place an update under another one *)
      let u'' = U.compose u' u in
      (* do we want updated value already? *)
      (* probably not, otherwise if we change the update, we need to reapply it *)
      (* e.g. if quering below it *)
      (* let value = U.apply_update v u in *)
      Node (v, u'', left, right)

  (* point update *)
  let rec update' lower upper tree i f =
    match tree with
    | Leaf v -> (assert (i = 0); Leaf (f v))
    | Node (_, u, left, right) ->
      (* if pending update, propagate downward *)
      (* never place update under other update *)
      let (left', right') = (apply_update left u, apply_update right u) in
      let mid = (upper - lower + 1) / 2 in
      if i < mid then
        let left'' = update' lower mid left i f in
        let value = B.combine (get_value left'') (get_value right') in
        Node (value, U.empty, left'', right')
      else
        let right'' = update' mid upper right (i - mid) f in
        let value = B.combine (get_value left') (get_value right'') in
        Node (value, U.empty, left', right'')

  let update tree i f = { tree = update' 0 tree.size tree.tree i f; size = tree.size }

  let rec range_update' lower upper tree i j u =
    match tree with
    | Leaf v -> (assert (i = 0 && j = 0); Leaf (U.apply_update v u))

    | Node (_, u', left, right) ->
      let (left', right') = (apply_update left u', apply_update right u') in
      let mid = (upper - lower + 1) / 2 in
      if j < mid then
        let left'' = range_update' lower mid left i j u in
        let value = B.combine (get_value left'') (get_value right') in
        Node (value, U.empty, left'', right')
      else if i >= mid then
        let right'' = range_update' mid upper right (i - mid) (j - mid) u in
        let value = B.combine (get_value left') (get_value right'') in
        Node (value, U.empty, left', right'')
      else
        let left'' = range_update' lower mid left i (mid-1) u in
        let right'' = range_update' mid upper right 0 (j - mid) u in
        let value = B.combine (get_value left'') (get_value right'') in
        Node (value, U.empty, left'', right'')

end