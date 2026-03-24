#include <caml/mlvalues.h>
#include <caml/callback.h>

void init_ocaml_runtime(void) {
    char *dummy_argv[] = { "ocaml_ffi", NULL };
    caml_startup(dummy_argv);
}