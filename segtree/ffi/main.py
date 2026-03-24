import ctypes
import time
import os
import sys

lib_path = os.path.abspath("./_build/default/lib/ffi_exports.so")

print("1. Loading Library...")
# RTLD_GLOBAL is often strictly required for the OCaml GC to load correctly!
mylib = ctypes.CDLL(lib_path, mode=ctypes.RTLD_GLOBAL)

print("2. Initializing OCaml Runtime...")
mylib.init_ocaml_runtime.restype = None
mylib.init_ocaml_runtime()

# FFI Signatures
void_p = ctypes.c_void_p
c_int = ctypes.c_int

class QueryResult(ctypes.Structure):
    _fields_ = [
        ("sum", ctypes.c_int64),
        ("count", ctypes.c_int64)
    ]

mylib.st_init.argtypes = []
mylib.st_init.restype = void_p

mylib.st_insert.argtypes = [void_p, c_int, c_int, c_int]
mylib.st_insert.restype = void_p

mylib.st_range_add.argtypes = [void_p, c_int, c_int, c_int]
mylib.st_range_add.restype = void_p

mylib.st_query.argtypes = [void_p, c_int, c_int]
mylib.st_query.restype = QueryResult

mylib.st_free.argtypes = [void_p]
mylib.st_free.restype = None

def test_stress():
    n = 1_000_000  # <--- drastically reduced to test for OCaml exceptions!
    
    print("3. Initializing empty tree...")
    tree = mylib.st_init()
    
    print(f"4. Running {n} inserts...")
    for i in range(n):
        new_tree = mylib.st_insert(tree, i, 0, 1)
        mylib.st_free(tree)
        tree = new_tree

    print("5. Running range additions...")
    for _ in range(n):
        new_tree = mylib.st_range_add(tree, 0, n, 1)
        mylib.st_free(tree)
        tree = new_tree

    print("6. Querying totals...")
    # It now returns the struct directly!
    total_res = mylib.st_query(tree, 0, n) 
    print(f"Total Sum: {total_res.sum}, Total Count: {total_res.count}")
    
    print("7. Verifying point sums...")
    point_sums_correct = True
    for i in range(n):
        res = mylib.st_query(tree, i, i)
        
        if res.sum != n:
            point_sums_correct = False
            break
            
    # Notice: We completely removed mylib.st_free() from the query steps!

    print(f"Done! Total sum correct? {total_res.sum == n * n}")
    print(f"Point sums correct? {point_sums_correct}")

if __name__ == "__main__":
    test_stress()