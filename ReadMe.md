# Most General Segment Tree

A segment tree implementation over 
- an arbitrary base type B, and 
- key type K with an 
- operation C on B, and
- (optional) range updates U

such that:

- C:B×B→B is associative
- K is ordered (supports comparisons -- all types in OCaml do (except functions))
- there is an A:B×U→B that applies updates to (accumulated) values, and
- a function G:U×U→U that combines updates to one O(1) operation

The segment tree supports:
- range queries
- arbitrary point updates
- defined range updates (in U via A)
- a debug print
- insertion of new keys (not just updates to existing keys)


The addition of general range updates is already an advantage over "normal" segment trees that either only support queries with point updates, or one hard-coded lazy range update.

Furthermore, insertions are seldom together with lazy updates.
Most notably, we provide:
- worst-case O(log n) operations (not possible with scape-goat trees)
- a purely functional implementation (and thus persistent data structure)
- no need for a neutral element for the operation (simplified implementations need these)
- no invertible operations (a fenwick-tree implementation is possible if the operation is invertible)
- demonstration of an FFI usage in C and Python
- demo test cases (see below)

## Other Segtree Implementations (Rust)
- [ac_library::lazysegtree](https://docs.rs/ac-library-rs/latest/ac_library/lazysegtree/struct.LazySegtree.html) ([documentation](https://atcoder.github.io/ac-library/production/document_en/lazysegtree.html))
    - they have the same categorical base (also more nicely written up)
    - no insert, not functional, needs neutral element (monoid structure)
- [seg_tree](https://docs.rs/seg-tree/latest/seg_tree/)
    - TODO
- [segtri](https://docs.rs/segtri/latest/segtri/)
    - TODO


## Demo/Test Cases

First, we define our domain.
We want to support 
- sum operations
- on integer keys
- with Add and Set (range) updates

```ocaml
(* for single elements, sum is the value, count is 1 *)
type sum_t = { sum: int; count: int }
type sum_updates = 
  | SetValue of int
  | AddValue of int
```
Note: without range updates, the count would not be needed.

For these types, we define our modules to instantiate the segment tree:

```ocaml
module IntKey : Key with type t = int = struct
  type t = int
  let le a b = a <= b
end

module SumBase : Base with type t = sum_t = struct
  type t = sum_t
  let combine = fun a b -> { sum = a.sum + b.sum; count = a.count + b.count }
end
```

The update module is more interesting. 
Setting a range of n values, leaves their count unchanged, but sets the sum to n*value.
Adding a value to a range of n values, adds n*value to the sum, and leaves the count unchanged.
For composition, set overrides everything, and add adds up.
```ocaml
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
```

### Testing Insert into Updates

Updates are lazy and cling to the outermost root.
However, if we insert into an updates range, the new value should not be effected by the update.
Example:
```
Insert 1,2,5 with value 1
Range 0..6 sums up to 3
Adding 3 on 0..6 results in 1,2,5 having value 4
Range 0..6 sums up to 12
Inserting 3,4 with value 1
Range 0..6 sums up to 14
```

```ocaml
let module ST = SegmentTree(IntKey)(SumBase)(SumUpdater) in
let base_value = { sum = 1; count = 1 } in
let tree = ST.init in
let tree = ST.insert tree 1 base_value in
let tree = ST.insert tree 2 base_value in
let tree = ST.insert tree 5 base_value in
Printf.printf "Initial tree:\n%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
Printf.printf "Initial sum of [0, n): %d\n%!" (ST.query tree 0 6).sum;
(* expect 3 *)
Printf.printf "\n\n\n%!";
let tree = ST.range_update tree 0 6 (AddValue 3) in
Printf.printf "After adding 3 to all:\n%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
Printf.printf "Sum of [0, n) after adding 3: %d\n%!" (ST.query tree 0 6).sum;
(* expect 12 *)
Printf.printf "\n\n\n%!";
let tree = ST.insert tree 3 base_value in
let tree = ST.insert tree 4 base_value in
Printf.printf "After inserting 3 and 4:\n%s\n%!" (ST.show string_of_int show_sum_update show_sum tree);
Printf.printf "Sum of [0, n) after inserting 3 and 4: %d\n%!" (ST.query tree 0 6).sum;
(* expect 14 *)
Printf.printf "\n\n\n%!"
``` 

### Stress Test for Lazy Updates

If we perform an update, we need to combine it with existing updates.
Otherwise, we accumulate costs at a node that could propagate downwards.

Example:
- Have a tree with n nodes
- Update the root n times with +1
- Query node 1 to n

If updates are not combined (no update structure), we have n O(1) operations at the root.
Quering a node propagates all updates down, resulting in O(n) time at the leaf.
This happens for every node, resulting in O(n^2 log n) time for the queries.
If one does not implement lazy updates correctly, and just pushes updates down, we arrive at a total cost of O(n^2).

```ocaml
let module ST = SegmentTree(IntKey)(SumBase)(SumUpdater) in
let n = 1_000_000 in
let tree = ST.init in
let tree = List.fold_left (fun t i -> ST.insert t i sum_init) tree (List.init n Fun.id) in
let tree = List.fold_left (fun t _ -> ST.range_update t 0 n (AddValue 1)) tree (List.init n (fun _ -> ())) in
let total_sum = ST.query tree 0 n in
let sums = List.init n (fun i -> (ST.query tree i i).sum) in
```

#### Performance

The code above takes roughly `0.98s` on my machine, compiled with `ocamlopt -O3` on OCaml 5.3.0.

(0.85s in 5.1.1+flambda2 with imperative overwriting of the tree)

## Invariants of the Implementation

When we update/modify a node, we push existing updates downward if needed. This way, older updates are always lower in the tree (and need to be applied first).

Updates are only pushes as far down as needed.
If a path is traversed, updates may only spread to the side of the path.
Therefore, every operation touches at most O(log n) nodes, incurring O(log n) time and space costs.


