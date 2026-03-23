import ctypes
import sys
import os

lib_path = os.path.abspath('./libtree_st.so')
try:
    lib = ctypes.CDLL(lib_path)
except OSError as e:
    print(f"Error loading library: {e}\nDid you compile libtree_st.so?")
    sys.exit(1)

# base type structure to match OCaml's 'sum_t' type
class CSumT(ctypes.Structure):
    _fields_ = [
        ("sum", ctypes.c_int64),
        ("count", ctypes.c_int64)
    ]

# define library functions
# ocaml values are opaque pointers => void* (c_void_p)
lib.st_init_system.argtypes = [ctypes.POINTER(ctypes.c_char_p)]
lib.st_init_system.restype = None

lib.st_init.argtypes = []
lib.st_init.restype = ctypes.c_void_p

lib.st_insert.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.c_int64, ctypes.c_int64]
lib.st_insert.restype = ctypes.c_void_p

lib.st_range_add.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.c_int, ctypes.c_int64]
lib.st_range_add.restype = ctypes.c_void_p

lib.st_query.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.c_int]
lib.st_query.restype = CSumT

# GC root functions
lib.caml_register_global_root.argtypes = [ctypes.POINTER(ctypes.c_void_p)]
lib.caml_register_global_root.restype = None

lib.caml_remove_global_root.argtypes = [ctypes.POINTER(ctypes.c_void_p)]
lib.caml_remove_global_root.restype = None
