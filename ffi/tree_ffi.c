#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <stdint.h>
#include "tree_ffi.h"

void st_init_system(char **argv) {
    caml_startup(argv);
}

value st_init(void) {
    CAMLparam0();
    static const value * closure = NULL;
    if (closure == NULL) closure = caml_named_value("st_init");
    
    CAMLreturn(caml_callback(*closure, Val_unit));
}

value st_insert(value tree, int k, int64_t sum, int64_t count) {
    CAMLparam1(tree);
    static const value * closure = NULL;
    if (closure == NULL) closure = caml_named_value("st_insert");
    
    value args[4] = { tree, Val_int(k), Val_long((intnat)sum), Val_long((intnat)count) };
    CAMLreturn(caml_callbackN(*closure, 4, args));
}

c_sum_t st_query(value tree, int i, int j) {
    CAMLparam1(tree);
    CAMLlocal1(res);
    static const value * closure = NULL;
    if (closure == NULL) closure = caml_named_value("st_query");
    
    res = caml_callback3(*closure, tree, Val_int(i), Val_int(j));
    
    c_sum_t out;
    out.sum = (int64_t)Long_val(Field(res, 0));
    out.count = (int64_t)Long_val(Field(res, 1));
    
    CAMLreturnT(c_sum_t, out);
}

value st_range_add(value tree, int i, int j, int64_t delta) {
    CAMLparam1(tree);
    static const value * closure = NULL;
    if (closure == NULL) closure = caml_named_value("st_range_add");
    
    value args[4] = { tree, Val_int(i), Val_int(j), Val_long((intnat)delta) };
    CAMLreturn(caml_callbackN(*closure, 4, args));
}

value st_range_set(value tree, int i, int j, int64_t v) {
    CAMLparam1(tree);
    static const value * closure = NULL;
    if (closure == NULL) closure = caml_named_value("st_range_set");
    
    value args[4] = { tree, Val_int(i), Val_int(j), Val_long((intnat)v) };
    CAMLreturn(caml_callbackN(*closure, 4, args));
}