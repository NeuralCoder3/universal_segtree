#include <stdio.h>
#include <time.h>
#include <inttypes.h>  // Added for PRId64
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include "tree_ffi.h"

int main(int argc, char **argv) {
    st_init_system(argv);
    value tree = st_init();
    caml_register_global_root(&tree);

    // 10^6 1.00s
    // 10^8 149.75s
    int n = 1000000;

    printf("Building tree with %d elements...\n", n);
    for (int i = 0; i < n; i++) {
        tree = st_insert(tree, i, 0, 1);
    }

    printf("Starting stress test with %d range updates...\n", n);
    clock_t start_time = clock();

    for (int i = 0; i < n; i++) {
        tree = st_range_add(tree, 0, n, 1);
    }

    c_sum_t total_sum = st_query(tree, 0, n);

    // check each single node
    bool all_correct = true;
    for (int i = 0; i < n; i++) {
        c_sum_t node_sum = st_query(tree, i, i);
        if (node_sum.sum != n) {
            // printf("Node %d has incorrect sum: %" PRId64 " (Expected: %" PRId64 ")\n", i, node_sum.sum, (int64_t)n);
            all_correct = false;
            break;
        }
    }
    
    clock_t end_time = clock();
    double time_spent = (double)(end_time - start_time) / CLOCKS_PER_SEC;
    printf("Stress test completed in %.2f seconds.\n", time_spent);

    int64_t expected_sum = (int64_t)n * n; 
    // Print using PRId64 format specifier for cross-platform int64_t printing
    printf("Total sum: %" PRId64 " (Expected: %" PRId64 ")\n", total_sum.sum, expected_sum);
    printf("Total sum correct? %s\n", total_sum.sum == expected_sum ? "true" : "false");

    printf("All nodes correct? %s\n", all_correct ? "true" : "false");

    caml_remove_global_root(&tree);
    return 0;
}