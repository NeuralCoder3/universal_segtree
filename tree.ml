(*

- range query
- base type
- combine operation (associative)
- point update

- optional: range update (lazy, needs update type, combine, apply)

TODO:
- insert (based on red-black trees)
- compare OCaml segment tree

fenwick tree for invertible operation

*)


module type Base = sig
  type t
  val default : t (* identity for combine -- neutral element *)
  val combine : t -> t -> t (* associative *)
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

  (* type tree = Leaf of int*B.t | Node of ((B.t * U.update) * (int*int) * tree * tree) *)
  type tree = Leaf of { key: int; value: B.t } | Node of {
    value: B.t; 
    update: U.update; 
    lower: int;
    mid: int;
    upper: int;
    left: tree; 
    right: tree
  }
  type segment_tree = { size : int; tree : tree }

  let get_value = function
    | Leaf n -> n.value
    | Node n -> U.apply_update n.value n.update

  let rec init lower upper =
    if lower + 1 = upper then Leaf { key = lower; value = B.default }
    else
      let mid = (lower + upper) / 2 in
      let left = init lower mid in
      let right = init mid upper in
      let value = B.combine (get_value left) (get_value right) in
      (* assert (value = B.default); *)
      Node { value; update = U.empty; lower; mid; upper; left; right }

(*
  either directly propagate updates down, or collect them
*)
(*
  all out range => id
  all in range => value
  else => combine

  can leave i j same
*)
let query tree i j =
  let rec aux pending tree i j =
    assert (i <= j);
    match tree with
    | Leaf n -> (assert (i+1 = j && i = n.key); U.apply_update n.value pending)
    | Node n ->
      (assert (i >= n.lower && j <= n.upper));
      (* lower update is older => apply first *)
      let update = U.compose n.update pending in
      if j <= n.mid then aux update n.left i j
      else if i >= n.mid then aux update n.right i j
      else (
        (* can query whole node *)
        if i = n.lower && j = n.upper then get_value tree
        else B.combine (aux update n.left i n.mid) (aux update n.right n.mid j)
      )
  in
  aux U.empty tree i j

  let apply_update tree u =
    match tree with
    | Leaf n -> Leaf { n with value = U.apply_update n.value u }
    | Node n ->
      (* the older update is father down as we never place an update under another one *)
      let u' = U.compose n.update u in
      (* do we want updated value already? *)
      (* probably not, otherwise if we change the update, we need to reapply it *)
      (* e.g. if quering below it *)
      (* let value = U.apply_update v u in *)
      Node { n with update = u' }

  (* point update *)
  let rec point_update tree i f =
    match tree with
    | Leaf n -> (assert (i = n.key); Leaf { n with value = f n.value })
    | Node n ->
      (* if pending update, propagate downward *)
      (* never place update under other update *)
      let (left', right') = (apply_update n.left n.update, apply_update n.right n.update) in
      if i < n.mid then
        let left'' = point_update left' i f in
        let value = B.combine (get_value left'') (get_value right') in
        Node { n with value; update = U.empty; left = left''; right = right' }
      else
        let right'' = point_update right' i f in
        let value = B.combine (get_value left') (get_value right'') in
        Node { n with value; update = U.empty; left = left'; right = right'' }

  let rec range_update tree i j u =
    match tree with
    | Leaf n -> 
      (assert (i = n.key && j = i+1); Leaf { n with value = U.apply_update n.value u })
    | Node n ->
      (assert (i >= n.lower && j <= n.upper));
      if i = n.lower && j = n.upper then apply_update tree u
      else
        let (left', right') = (apply_update n.left n.update, apply_update n.right n.update) in
        let (left'', right'') = 
           if j <= n.mid then (range_update left' i j u, right')
           else if i >= n.mid then (left', range_update right' i j u)
           else (
             let left'' = range_update left' i n.mid u in
             let right'' = range_update right' n.mid j u in
             (left'', right'')
           )
        in
        let value = B.combine (get_value left'') (get_value right'') in
        Node { n with value; update = U.empty; left = left''; right = right'' }

  let show show_key show_update show_value tree =
    let rec aux level tree =
      match tree with
      | Leaf n -> Printf.sprintf "%sLeaf(key=%s, value=%s)" (String.make (2*level) ' ') (show_key n.key) (show_value n.value)
      | Node n ->
        let left_str = aux (level + 1) n.left in
        let right_str = aux (level + 1) n.right in
        Printf.sprintf "%sNode(value=%s, update=%s)\n%s\n%s" (String.make (2*level) ' ') (show_value n.value) (show_update n.update) left_str right_str
    in
    aux 0 tree

  

end







(* Tests *)





(* with type t = int *)
module SumPointBase : Base = struct
  type t = int
  let default = 0
  let combine = ( + )
end

(* 
It has to be known that SumBase and SumUpdate are compatible, i.e. SumUpdate.base_t = SumBase.t
*)
type sum_t = { sum: int; count: int }
module SumBase : Base with type t = sum_t = struct
  (* for single elements, sum is the value *)
  type t = sum_t
  let default = { sum = 0; count = 1 }
  let combine = fun a b -> { sum = a.sum + b.sum; count = a.count + b.count }
end

(* Outside to expose interface to user 
  could also be handled by exposing a subtype (module type between updater and implementation)
*)
type sum_updates = 
  | SetValue of int
  | AddValue of int
  | NoUpdate
module SumUpdater : Updater with type base_t = sum_t with type update = sum_updates = struct
  type base_t = sum_t
  type update = sum_updates

  let empty = NoUpdate
  let compose old_update new_update =
    match (old_update, new_update) with
    | (NoUpdate, u) | (u, NoUpdate) -> u
    | (_, SetValue v) -> SetValue v
    | (SetValue v, AddValue delta) -> SetValue (v + delta)
    | (AddValue v1, AddValue v2) -> AddValue (v1 + v2)

  let apply_update base update =
    match update with
    | NoUpdate -> base
    | SetValue v -> { base with sum = v * base.count }
    | AddValue delta -> { base with sum = base.sum + delta * base.count }

end


let show_sum_update = function
  | NoUpdate -> "-"
  | SetValue v -> Printf.sprintf "%d" v
  | AddValue delta -> Printf.sprintf "+%d" delta

let show_sum { sum; count } = Printf.sprintf "(Sum=%d, Count=%d)" sum count

let () = 
  let module ST = SegmentTree(SumBase)(SumUpdater) in
  (* let n = 1024 in *)
  let n = 8 in
  let tree = ST.init 0 n in
  Printf.printf "%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
  Printf.printf "Initial sum of [0, 1024): %d\n%!" (ST.query tree 0 n).sum;
  Printf.printf "\n\n\n%!";
  let tree = ST.point_update tree 5 (fun v -> { v with sum = 10 }) in
  Printf.printf "%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
  Printf.printf "After point update at index 5: %d\n%!" (ST.query tree 0 n).sum;
  Printf.printf "\n\n\n%!";
  let tree = ST.range_update tree 1 4 (AddValue 2) in
  Printf.printf "%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
  (* Printf.printf "After range update [1, 4): %d\n" (ST.query tree 0 n).sum; *)
  Printf.printf "\n\n\n%!";
  ()
  
(* 
stress test for lazy update:
update root (whole tree) n time +1, query each point individually
ideal runtime: O(n + n log n) = O(n log n)
without combined updates: O(n log n) for each query, total O(n^2 log n)
without lazy updates: O(n) for each update + O(log n) for each query, total O(n^2)
*)

let () =
  let module ST = SegmentTree(SumBase)(SumUpdater) in
  (* let n = 1024 in *)
  let n = 1_000_000 in
  (* let n = 1_000_000_0 in *)
  (*
  10^6  1.65s / 1.32s (ocaml), 0.18s (ocamlopt -O3), 1.25 (ocamlc)
  10^9 18.47s
  *)
  let tree = ST.init 0 n in

  (* let tree' = tree in
  let tree' = ST.range_update tree' 0 n (AddValue 1) in
  Printf.printf "Sum after one update: %d\n%!" (ST.query tree' 0 n).sum;
  Printf.printf "Point Sum after one update: %d\n%!" (ST.query tree' 0 1).sum;
  let tree' = ST.range_update tree' 0 n (AddValue 1) in
  Printf.printf "Sum after two updates: %d\n%!" (ST.query tree' 0 n).sum;
  Printf.printf "Point Sum after two updates: %d\n%!" (ST.query tree' 0 1).sum; *)

  Printf.printf "Starting stress test with %d updates...\n%!" n;
  let init_time = Sys.time () in
  let tree = List.fold_left (fun t _ -> ST.range_update t 0 n (AddValue 1)) tree (List.init n (fun _ -> ())) in
  let total_sum = ST.query tree 0 n in
  let sums = List.init n (fun i -> (ST.query tree i (i+1)).sum) in
  let end_time = Sys.time () in
  Printf.printf "Stress test completed in %.2f seconds.\n%!" (end_time -. init_time);
  Printf.printf "Total sum correct? %b\n%!" (total_sum.sum = n * n);
  Printf.printf "Point sums correct? %b\n%!" (List.for_all ((=) n) sums);
  (* Printf.printf "Total sum after %d updates: %d (expected: %d)\n%!" n total_sum.sum (n * n); *)
  (* Printf.printf "Sums after %d updates: %s\n%!" n (String.concat ", " (List.map string_of_int sums)); *)
  ()

(* TODO: query whole range is missing update *)