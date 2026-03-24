open Segtree
(* 
lazy init:
1,2,5 with 1
sum = 3
+3 on all
sum = 12
insert 3,4 with 1
sum = 14 
*)
let test () =
  let open Segtree_sum.Sum_domain in
  let module ST = SegTree(IntKey)(SumBase)(SumUpdater) in
  let base_value = { sum = 1; count = 1 } in
  let tree = ST.init in
  let tree = ST.insert tree 1 base_value in
  let tree = ST.insert tree 2 base_value in
  let tree = ST.insert tree 5 base_value in
  Printf.printf "Initial tree:\n%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
  let s1 = ST.query tree 0 6 in
  Printf.printf "Initial sum of [0, n): %d\n%!" s1.sum;
  Printf.printf "\n\n\n%!";
  if s1.sum <> 3 then exit 1;
  let tree = ST.range_update tree 0 6 (AddValue 3) in
  Printf.printf "After adding 3 to all:\n%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
  let s2 = ST.query tree 0 6 in
  Printf.printf "Sum of [0, n) after adding 3: %d\n%!" s2.sum;
  Printf.printf "\n\n\n%!";
  if s2.sum <> 12 then exit 1;
  let tree = ST.insert tree 3 base_value in
  let tree = ST.insert tree 4 base_value in
  Printf.printf "After inserting 3 and 4:\n%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
  let s3 = ST.query tree 0 6 in
  Printf.printf "Sum of [0, n) after inserting 3 and 4: %d\n%!" s3.sum;
  Printf.printf "\n\n\n%!";
  if s3.sum <> 14 then exit 1;
  ()

let () = test ()
