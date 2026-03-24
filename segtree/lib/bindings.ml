open Ctypes
open Segtree_sum.Sum_domain

module Stubs (I : Cstubs_inverted.INTERNAL) = struct

  module ST = Segtree.SegTree(Segtree.IntKey)(Segtree_sum.Sum_domain.SumBase)(Segtree_sum.Sum_domain.SumUpdater)

  (* sum base type structure in c *)
  type query_result
  let query_result : query_result structure typ = structure "query_result"
  let q_sum = field query_result "sum" int64_t
  let q_count = field query_result "count" int64_t
  let () = seal query_result  (* "seal" finalizes the memory layout *)
  

  let () =
    (* pinned GC root ( void* ) *)
    I.internal "st_init" (void @-> returning (ptr void)) (fun () ->
      let tree = ST.init in
      Root.create tree
    );

    (* insert takes a void*, gets the tree, and returns a new void* root *)
    I.internal "st_insert" (ptr void @-> int @-> int @-> int @-> returning (ptr void)) 
    (fun tree_ptr k sum count ->
      let tree = Root.get tree_ptr in
      let new_tree = ST.insert tree k { sum; count } in
      Root.create new_tree
    );

    (* query result as struct by value *)
    I.internal "st_query" (ptr void @-> int @-> int @-> returning query_result) 
    (fun tree_ptr i j ->
      let tree = Root.get tree_ptr in
      let result = ST.query tree i j in
      
      (* allocate a C-struct and fill its fields *)
      let ret = make query_result in
      setf ret q_sum (Int64.of_int result.sum);
      setf ret q_count (Int64.of_int result.count);
      ret
    );

    I.internal "st_range_add" (ptr void @-> int @-> int @-> int @-> returning (ptr void)) 
    (fun tree_ptr i j delta ->
      let tree = Root.get tree_ptr in
      let new_tree = ST.range_update tree i j (AddValue delta) in
      Root.create new_tree
    );

    I.internal "st_range_set" (ptr void @-> int @-> int @-> int @-> returning (ptr void)) 
    (fun tree_ptr i j v ->
      let tree = Root.get tree_ptr in
      let new_tree = ST.range_update tree i j (SetValue v) in
      Root.create new_tree
    );

    (* ffi-way to free ocaml memory (used for intermediate trees) *)
    I.internal "st_free" (ptr void @-> returning void) (fun tree_ptr ->
      Root.release tree_ptr
    );

    ()
  
end