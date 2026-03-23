CC = gcc
CFLAGS = -O3 -Wall -I $(OCAML_DIR)
OCAMLOPT = ocamlopt
OCAMLC = ocamlc
OCAML_DIR = $(shell $(OCAMLC) -where)

TARGET = stress_test
SHARED_LIB = libtree_st.so

all: $(TARGET)

tree_ffi.o: tree_ffi.c tree_ffi.h
	$(CC) $(CFLAGS) -fPIC -c $< -o $@

# bundle to single so
$(SHARED_LIB): tree.ml tree_ffi_wrapper.ml tree_ffi.o
	$(OCAMLOPT) -output-complete-obj -o $@ $^

# compile main and link
$(TARGET): tree.c $(SHARED_LIB)
	$(CC) $(CFLAGS) -o $@ $< -L. -ltree_st -Wl,-rpath,.

clean:
	rm -f *.o *.cm[iox] $(SHARED_LIB) $(TARGET)

.PHONY: all clean