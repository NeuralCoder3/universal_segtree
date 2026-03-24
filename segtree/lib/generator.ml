let () =
  (* This tells the generator to read the Stubs module from bindings.ml *)
  let stubs = (module Bindings.Stubs : Cstubs_inverted.BINDINGS) in

  let fd_ml = open_out "bridge_stubs.ml" in
  let fmt_ml = Format.formatter_of_out_channel fd_ml in
  Cstubs_inverted.write_ml fmt_ml ~prefix:"caml_" stubs;
  close_out fd_ml;

  let fd_h = open_out "bridge.h" in
  let fmt_h = Format.formatter_of_out_channel fd_h in
  (* needed for custom type *)
  Format.fprintf fmt_h "#include <stdint.h>\n\n";
  Format.fprintf fmt_h "struct query_result {\n";
  Format.fprintf fmt_h "    int64_t sum;\n";
  Format.fprintf fmt_h "    int64_t count;\n";
  Format.fprintf fmt_h "};\n\n";
  Cstubs_inverted.write_c_header fmt_h ~prefix:"caml_" stubs;
  close_out fd_h;

  let fd_c = open_out "bridge.c" in
  let fmt_c = Format.formatter_of_out_channel fd_c in
  Format.fprintf fmt_c "#include \"bridge.h\"\n";
  Cstubs_inverted.write_c fmt_c ~prefix:"caml_" stubs;
  close_out fd_c