open Segtree
let test () = 
  let open Segtree_sum.Sum_domain in
  let module ST = SegTree(IntKey)(SumBase)(SumUpdater) in
  (* let n = 1024 in *)
  let n = 8 in
  let tree = ST.init in
  let tree = List.fold_left (fun t i -> ST.insert t i sum_init) tree (List.init n Fun.id) in
  Printf.printf "%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
  Printf.printf "Initial sum of [0, n): %d\n%!" (ST.query tree 0 n).sum;
  Printf.printf "\n\n\n%!";
  let tree = ST.point_update tree 5 (fun v -> { v with sum = 10 }) in
  Printf.printf "%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
  Printf.printf "After point update at index 5: %d\n%!" (ST.query tree 0 n).sum;
  Printf.printf "\n\n\n%!";
  let tree = ST.range_update tree 1 4 (AddValue 2) in
  Printf.printf "%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
  Printf.printf "After range update [1, 4): %d\n" (ST.query tree 0 n).sum;
  Printf.printf "\n\n\n%!";
  ()

let () = test ()
