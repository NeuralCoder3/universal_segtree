open Segtree

(* 
It has to be known that SumBase and SumUpdate are compatible, i.e. SumUpdate.base_t = SumBase.t
*)
type sum_t = { sum: int; count: int }

(* Outside to expose interface to user 
  could also be handled by exposing a subtype (module type between updater and implementation)
*)
type sum_updates = 
  | SetValue of int
  | AddValue of int

module SumBase : Base with type t = sum_t = struct
  (* for single elements, sum is the value *)
  type t = sum_t
  let combine = fun a b -> { sum = a.sum + b.sum; count = a.count + b.count }
end

module SumUpdater : Updater with type base_t = sum_t with type update = sum_updates = struct
  type base_t = sum_t
  type update = sum_updates

  let compose old_update new_update =
    match (old_update, new_update) with
    | (_, SetValue v) -> SetValue v
    | (SetValue v, AddValue delta) -> SetValue (v + delta)
    | (AddValue v1, AddValue v2) -> AddValue (v1 + v2)

  let apply_update base update =
    match update with
    | SetValue v -> { base with sum = v * base.count }
    | AddValue delta -> { base with sum = base.sum + delta * base.count }
end


let show_sum_update = function
  | SetValue v -> Printf.sprintf "%d" v
  | AddValue delta -> Printf.sprintf "+%d" delta

let show_sum { sum; count } = Printf.sprintf "(Sum=%d, Count=%d)" sum count

let sum_init = { sum = 0; count = 1 }
