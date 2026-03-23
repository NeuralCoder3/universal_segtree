#ifndef TREE_FFI_H
#define TREE_FFI_H

#include <caml/mlvalues.h>
#include <stdint.h>

typedef struct {
    int64_t sum;
    int64_t count;
} c_sum_t;

void st_init_system(char **argv);

value st_init(void);
value st_insert(value tree, int k, int64_t sum, int64_t count);
c_sum_t st_query(value tree, int i, int j);
value st_range_add(value tree, int i, int j, int64_t delta);
value st_range_set(value tree, int i, int j, int64_t v);

#endif // TREE_FFI_H