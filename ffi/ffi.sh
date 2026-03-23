ocamlopt -output-obj -o tree_st_lib.o tree.ml wrapper.ml
gcc -c -I $(ocamlc -where) tree_ffi.c
gcc -o my_app main.c tree_ffi.o tree_st_lib.o -L $(ocamlc -where) -lasmrun -lm -ldl



ocamlopt -output-obj -o tree_st_lib.o tree.ml tree_ffi.ml
gcc -O3 -o stress_test -I $(ocamlc -where) -L $(ocamlc -where) tree.c tree_ffi.c tree_st_lib.o -lasmrun -lm -ldl


# .so (for Rust, Java, Python)
gcc -O3 -fPIC -I $(ocamlc -where) -c tree_ffi.c -o tree_ffi.o
ocamlopt -output-complete-obj -o libtree_st.so tree.ml tree_ffi_wrapper.ml tree_ffi.o
gcc -O3 -o stress_test tree.c -I $(ocamlc -where) -L. -ltree_st -Wl,-rpath,.