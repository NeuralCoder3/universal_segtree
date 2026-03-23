/*
avoid having caml functions on top-level
converts functional tree-interface to imperative
*/

typedef struct {
    value root;
} st_wrapper_t;

st_wrapper_t* st_create() {
    st_wrapper_t* wrapper = malloc(sizeof(st_wrapper_t));
    wrapper->root = st_init();
    caml_register_global_root(&(wrapper->root)); 
    return wrapper;
}

void st_free(st_wrapper_t* wrapper) {
    caml_remove_global_root(&(wrapper->root));
    free(wrapper);
}

void st_insert_wrapped(st_wrapper_t* wrapper, int k, int sum, int count) {
    wrapper->root = st_insert(wrapper->root, k, sum, count);
}