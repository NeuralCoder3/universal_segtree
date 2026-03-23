open Tree

module ST = SegmentTree(IntKey)(SumBase)(SumUpdater)
let st_init () = ST.init
let st_insert tree k sum count = ST.insert k { sum; count } tree
let st_query tree i j = ST.query tree i j
let st_range_add tree i j delta = ST.range_update tree i j (AddValue delta)
let st_range_set tree i j v = ST.range_update tree i j (SetValue v)
let () =
  Callback.register "st_init" st_init;
  Callback.register "st_insert" st_insert;
  Callback.register "st_query" st_query;
  Callback.register "st_range_add" st_range_add;
  Callback.register "st_range_set" st_range_set