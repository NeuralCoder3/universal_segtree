module type Base = sig
  type t
  val combine : t -> t -> t (* associative *)
end

module type Key = sig type t end

module type Updater = sig
  type base_t
  type update
  val compose : update -> update -> update
  (* also on composed nodes *)
  val apply_update : base_t -> update -> base_t
end

module DummyUpdater (B:Base) = struct
  type base_t = B.t
  type update = unit
  let compose () () = ()
  let apply_update v () = v
end

module IntKey : Key with type t = int = struct
  type t = int
end

module OptionUpdater (U: Updater) = struct
  type update = U.update option
  let empty = None
  let compose old_update new_update =
    match (old_update, new_update) with
    | (None, u) | (u, None) -> u
    | (Some u1, Some u2) -> Some (U.compose u1 u2)
  let apply_update base = function
    | None -> base
    | Some u -> U.apply_update base u
end

module SegTree (K:Key) (B:Base) (BU:Updater with type base_t = B.t) : sig
  type tree
  val init : tree
  val query : tree -> K.t -> K.t -> B.t
  val point_update : tree -> K.t -> (B.t -> B.t) -> tree
  val range_update : tree -> K.t -> K.t -> BU.update -> tree
  val show : (K.t -> string) -> (BU.update -> string) -> (B.t -> string) -> tree -> string 
  val insert : tree -> K.t -> B.t -> tree
end = struct

  module U = OptionUpdater(BU)

  (* split node to allow aliasing it in matching *)
  type node = {
      value: B.t; 
      (* 
          lower update is older => apply first
          maintained by not placing an update under an existing one (always propagate the old one before)
      *)
      update: U.update;
      (* empty for leaf (both are always simultanously empty) *)
      left: tree; 
      right: tree;

      lower: K.t;
      upper: K.t; (* inclusive, = lower for leaf *)

      height : int;
  } and tree = 
    | Empty 
    | Node of node

  let get_value = function
    | Empty -> None
    | Node n -> Some (U.apply_update n.value n.update)

  let init = Empty

  let combine' a b = match (a, b) with
    | (None, None) -> failwith "Both nodes are empty, cannot combine"
    | (Some x, None) | (None, Some x) -> x
    | (Some x, Some y) -> B.combine x y

  let query tree i j =
    assert (i <= j);
    let rec aux pending tree =
      match tree with
      | Empty -> failwith "Empty node has no value"
        (* B.default *)
      | Node n ->
        let update = U.compose n.update pending in
        (* completely in range *)
        if i <= n.lower && n.upper <= j then Some (U.apply_update n.value update)
        (* completely out of range *)
        else if n.upper < i || j < n.lower then None
        (* partially in range *)
        else Some (combine' (aux update n.left) (aux update n.right))
    in
    Option.get (aux U.empty tree)

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
      if i < n.lower || i > n.upper then tree else
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
      let (left'', right'') = (point_update left' i f, point_update right' i f) in
      (* let value = B.combine (get_value left'') (get_value right'') in *)
      let value = combine' (get_value left'') (get_value right'') in
      (* propagate update down *)
      Node { n with update = U.empty; left = left''; right = right''; value }

  let rec range_update tree i j u =
    match tree with
    | Empty -> Empty
    | Node n ->
      (* fully outside do nothing *)
      if n.upper < i || j < n.lower then tree
      (* fully inside *)
      else if i <= n.lower && n.upper <= j then apply_update tree (Some u)
      else
      (* recurse *)
        let (left', right') = (apply_update n.left n.update, apply_update n.right n.update) in
        let (left'', right'') = (range_update left' i j u, range_update right' i j u) in
        let value = combine' (get_value left'') (get_value right'') in
        Node { n with value; update = U.empty; left = left''; right = right'' }

  let show show_key show_update show_value tree =
    let show_option_update = function
      | None -> "-"
      | Some u -> show_update u
    in
    let rec aux level tree =
      match tree with
      | Empty -> ""
      | Node n ->
        let left_str = aux (level + 1) n.left in
        let right_str = aux (level + 1) n.right in
        Printf.sprintf "%sNode(key=[%s, %s], value=%s, update=%s)\n%s%s" 
          (String.make (2*level) ' ') 
          (show_key n.lower) 
          (show_key n.upper) 
          (show_value n.value) 
          (show_option_update n.update) 
          left_str right_str
    in
    aux 0 tree


  let get_lower = function Empty -> failwith "Empty node has no lower bound" | Node n -> n.lower
  let get_upper = function Empty -> failwith "Empty node has no upper bound" | Node n -> n.upper
  let get_height = function Empty -> 0 | Node n -> n.height

  let make_inner left right =
    Node { 
      height = 1 + max (get_height left) (get_height right);
      lower = min (get_lower left) (get_lower right);
      upper = max (get_upper left) (get_upper right); 
      value = combine' (get_value left) (get_value right);
      left; 
      right; 
      update = U.empty 
    }

  let push_down tree =
    match tree with
    | Empty -> Empty
    | Node n ->
      if n.update = U.empty then 
        tree
      else
        if n.left = Empty && n.right = Empty then
          (* we should not arrive at a leaf here *)
          (* assert false *)
          (* or bake in update *)
          Node { n with value = U.apply_update n.value n.update; update = U.empty }
          (* or leave as is *)
          (* tree *)
        else
          let left' = apply_update n.left n.update in
          let right' = apply_update n.right n.update in
          Node { n with update = U.empty; left = left'; right = right' }

  let balance left right =
    let hl = get_height left in
    let hr = get_height right in
    if hl > hr + 1 then
      (* rebalance left before rotating *)
      match push_down left with
      | Node l_node ->
          if get_height l_node.left >= get_height l_node.right then
            (* single right rotation *)
            make_inner l_node.left (make_inner l_node.right right)
          else
            (* double right rotation *)
            (match push_down l_node.right with
            | Node lr_node ->
                make_inner (make_inner l_node.left lr_node.left) (make_inner lr_node.right right)
            | Empty -> failwith "impossible AVL state")
      | Empty -> failwith "impossible AVL state"
    else if hr > hl + 1 then
      (* rebalance right before rotating *)
      match push_down right with
      | Node r_node ->
          if get_height r_node.right >= get_height r_node.left then
            (* single left rotation *)
            make_inner (make_inner left r_node.left) r_node.right
          else
            (* double left rotation *)
            (match push_down r_node.left with
            | Node rl_node ->
                make_inner (make_inner left rl_node.left) (make_inner rl_node.right r_node.right)
            | Empty -> failwith "impossible AVL state")
      | Empty -> failwith "impossible AVL state"
    else
      (* already balanced *)
      make_inner left right

  let insert t x v =
    let rec ins tree =
      match push_down tree with
      | Empty -> 
          Node { height = 1; lower = x; upper = x; value = v; left = Empty; right = Empty; update = U.empty }
          
      | Node n ->
          if n.left = Empty && n.right = Empty then
            (* leaf case *)
            let new_leaf = Node { height = 1; lower = x; upper = x; value = v; left = Empty; right = Empty; update = U.empty } in
            
            if x < n.lower then
              make_inner new_leaf (Node n)
            else if x > n.lower then
              make_inner (Node n) new_leaf
            else
              (* duplicate, overwrite *)
              Node { n with value = v }
              
          else
            let left_upper = get_upper n.left in
            if x <= left_upper then 
              balance (ins n.left) n.right
            else 
              balance n.left (ins n.right)
    in
    ins t

end
