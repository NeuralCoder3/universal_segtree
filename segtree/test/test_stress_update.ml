open Segtree
(* 
stress test for lazy update:
update root (whole tree) n time +1, query each point individually
ideal runtime: O(n + n log n) = O(n log n)
without combined updates: O(n log n) for each query, total O(n^2 log n)
without lazy updates: O(n) for each update + O(log n) for each query, total O(n^2)
*)
let test_stress () =
  let open Segtree_sum.Sum_domain in
  let module ST = SegTree(IntKey)(SumBase)(SumUpdater) in
  let n = 1_000_000 in
  let tree = ST.init in
  let tree = List.fold_left (fun t i -> ST.insert t i sum_init) tree (List.init n Fun.id) in

  Printf.printf "Starting stress test with %d updates...\n%!" n;
  let init_time = Sys.time () in
  let tree = List.fold_left (fun t _ -> ST.range_update t 0 n (AddValue 1)) tree (List.init n (fun _ -> ())) in
  let total_sum = ST.query tree 0 n in
  let sums = List.init n (fun i -> (ST.query tree i i).sum) in
  let point_sum_correct = List.for_all ((=) n) sums in
  let end_time = Sys.time () in
  Printf.printf "Stress test completed in %.2f seconds.\n%!" (end_time -. init_time);
  Printf.printf "Total sum correct? %b\n%!" (total_sum.sum = n * n);
  if total_sum.sum <> n * n then exit 1;
  Printf.printf "Point sums correct? %b\n%!" point_sum_correct;
  if not point_sum_correct then exit 1;
  ()

let () = test_stress ()
