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

module type Key = sig
  type t
  val le: t -> t -> bool
end


(* 
    lower update is older => apply first
    maintained by not placing an update under an existing one (always propagate the old one before)
*)
module SegmentTree (K:Key) (B:Base) (U:Updater with type base_t = B.t) = struct

  type tree = Empty | Node of {
    value: B.t; 
    update: U.update; (* should be empty for leaf nodes (TODO: check) *)
    left: tree; (* empty for leaf *)
    right: tree;

    lower: K.t;
    upper: K.t; (* inclusive, = lower for leaf *)
  }

  (* helper *)
  let get_value = function
    | Empty -> 
      failwith "Empty node has no value"
      (* B.default  *)
      (* TODO: fail instead? *)
    | Node n -> U.apply_update n.value n.update

  let init = Empty


let query tree i j =
  assert (K.le i j);
  let rec aux pending tree =
    match tree with
    | Empty -> failwith "Empty node has no value"
      (* B.default *)
    | Node n ->
      let update = U.compose n.update pending in
      (* completely in range *)
      if K.le i n.lower && K.le n.upper j then U.apply_update n.value update
      (* completely out of range *)
      (* TODO: this is the only place where we need the default element (and could avoid it) *)
      (* do not apply update as we need the neutral element for combine (if it fully falls in left/right) *)
      else if K.le n.upper i || K.le j n.lower then B.default
      (* partially in range *)
      else B.combine (aux update n.left) (aux update n.right)
  in
  aux U.empty tree

  (* helper *)
  let apply_update tree u =
    match tree with
    | Empty -> failwith "Empty node cannot be updated"
    | Node n ->
      let u' = U.compose n.update u in
      (* we do not want the updated value
        otherwise, we would need to reapply it (e.g. if querying below it)
      *)
      Node { n with update = u' }

  (* point update with arbitrary function *)
  let rec point_update tree i f =
    match tree with
    | Empty -> failwith "Empty node cannot be updated"
    | Node n ->
      (* if out of range, return node *)
      if K.le i n.lower || K.le n.upper i then tree else
      (* if single node, update else fail *)
      if n.lower = n.upper then 
        (if i = n.lower then 
          let new_value = f (U.apply_update n.value n.update) in
          Node { n with value = new_value; update = U.empty }
        else failwith "Index not found")
      else
      (* if pending update, propagate downward *)
      (* never place update under other update *)
      let (left', right') = (apply_update n.left n.update, apply_update n.right n.update) in
      (* propagate update down *)
      Node { n with update = U.empty; left = point_update left' i f; right = point_update right' i f }

  let rec range_update tree i j u =
    match tree with
    | Empty -> Empty
    | Node n ->
      (* fully outside do nothing *)
      if K.le n.upper i || K.le j n.lower then tree
      (* fully inside *)
      else if K.le i n.lower && K.le n.upper j then apply_update tree u
      else
      (* recurse *)
        let (left', right') = (apply_update n.left n.update, apply_update n.right n.update) in
        let (left'', right'') = (range_update left' i j u, range_update right' i j u) in
        let value = B.combine (get_value left'') (get_value right'') in
        Node { n with value; update = U.empty; left = left''; right = right'' }

  let show show_key show_update show_value tree =
    let rec aux level tree =
      match tree with
      | Empty -> Printf.sprintf "%sEmpty\n" (String.make (2*level) ' ')
      | Node n ->
        let left_str = aux (level + 1) n.left in
        let right_str = aux (level + 1) n.right in
        Printf.sprintf "%sNode(key=[%s, %s], value=%s, update=%s)\n%s\n%s" 
          (String.make (2*level) ' ') 
          (show_key n.lower) 
          (show_key n.upper) 
          (show_value n.value) 
          (show_update n.update) 
          left_str right_str
    in
    aux 0 tree

  

end







(* Tests *)





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

module IntKey : Key with type t = int = struct
  type t = int
  let le a b = a <= b
end


let show_sum_update = function
  | NoUpdate -> "-"
  | SetValue v -> Printf.sprintf "%d" v
  | AddValue delta -> Printf.sprintf "+%d" delta

let show_sum { sum; count } = Printf.sprintf "(Sum=%d, Count=%d)" sum count

let () = 
  let module ST = SegmentTree(IntKey)(SumBase)(SumUpdater) in
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
  (*
  10^6  1.65s
  10^7 18.47s
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