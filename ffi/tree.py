import ctypes
import time
from tree_ffi import lib

def main():
    argv = (ctypes.c_char_p * 2)(b"python_script", None)
    lib.st_init_system(argv)

    # initial tree wrapped in a pointer to note it to the GC
    tree = ctypes.c_void_p(lib.st_init())
    
    # register location for the GC
    lib.caml_register_global_root(ctypes.byref(tree))

    n = 1_000_000

    print(f"Building tree with {n} elements...")
    for i in range(n):
        # registeted memory location gets new pointer
        tree.value = lib.st_insert(tree, i, 0, 1)

    print(f"Starting stress test with {n} range updates...")
    start_time = time.time()

    for i in range(n):
        tree.value = lib.st_range_add(tree, 0, n, 1)

    total_sum = lib.st_query(tree, 0, n)

    # check single nodes
    all_correct = True
    for i in range(n):
        node_sum = lib.st_query(tree, i, i)
        if node_sum.sum != n:
            # print(f"Node {i} has incorrect sum: {node_sum.sum} (Expected: {n})")
            all_correct = False
            break

    end_time = time.time()
    time_spent = end_time - start_time
    print(f"Stress test completed in {time_spent:.2f} seconds.")

    expected_sum = n * n
    
    print(f"Total sum: {total_sum.sum} (Expected: {expected_sum})")
    print(f"Total sum correct? {str(total_sum.sum == expected_sum).lower()}")
    print(f"All nodes correct? {str(all_correct).lower()}")

    lib.caml_remove_global_root(ctypes.byref(tree))

if __name__ == "__main__":
    main()