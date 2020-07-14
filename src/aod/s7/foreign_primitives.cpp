/**
 * This file was auto-generated with org-babel tangle.
 * See foreign_types.org for more info
 * I would suggest to not edit this by hand.
 **/

#include "s7.h"

namespace aod {
     namespace s7 {
          namespace foreign {
               
               // ------------------------------ bool ------------------------------
               
               void free_bool(void* raw_data) {
                    bool* data = (bool*) raw_data;
                    delete data;
               }
               
               int tag_bool(s7_scheme *sc) {
                    s7_pointer res = s7_eval_c_string(sc, "(*foreign* 'type-bool)");
                    if(s7_is_integer(res)){
               	  return s7_integer(res);
                    }
                    s7_error(sc,                               /* s7 is declared in xen.h, defined in xen.c */
               	   s7_make_symbol(sc, "foreign-error"),
               	   s7_cons(sc, s7_make_string(sc, "type-bool not registered"), s7_nil(sc)));
                    return -1;
               }
               
               s7_pointer make_bool(s7_scheme *sc, s7_pointer args) {
                    bool* data = new bool;
                    *data = (bool) s7_boolean(sc, s7_car(args));
                    int type = tag_bool(sc);
                    s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
                    return obj;
               }
               
               s7_pointer ref_bool(s7_scheme *sc, s7_pointer args) {
                    bool* data = (bool*) s7_c_object_value(s7_car(args));
               
                    return s7_make_boolean(sc, *data);
               }
               
               s7_pointer set_bool(s7_scheme *sc, s7_pointer args) {
                    // 2 args: (block-set! (ref) value)
                    if (s7_list_length(sc, args) != 2) {
               	  return (s7_wrong_number_of_args_error(sc,
               						"set! for bool takes 2 arguments: ~S", args));
                    }
                    bool* data = (bool*) s7_c_object_value(s7_car(args));
                    s7_pointer s7_new_value = s7_cadr(args);
                    bool new_value = s7_boolean(sc, s7_new_value);
                    *data = new_value;
               
                    return (s7_new_value);
               }
               
               void bind_bool(s7_scheme* sc, s7_pointer env) {
                    /* s7_pointer env = s7_inlet(sc, s7_nil(sc)); */
                    /* s7_gc_protect(sc, env); */
               
                    // --- bool ----
                    s7_int type = s7_make_c_type(sc, "<bool>");
                    s7_define(sc, env, s7_make_symbol(sc, "type-bool"),
               	       s7_make_integer(sc, type));
                    s7_define(sc, env, s7_make_symbol(sc, "new-bool"),
               	       s7_make_function(sc, "bool", make_bool, 1, 0, false,
               				"creates a heap allocated bool (c-object)"));
                    s7_c_type_set_ref(sc, type, ref_bool);
                    s7_c_type_set_set(sc, type, set_bool);
                    s7_c_type_set_free(sc, type, free_bool);
               }
               
               // ! ---------------------------- bool ------------------------------
               

               
               // ------------------------------ int ------------------------------
               
               void free_int(void* raw_data) {
                    int* data = (int*) raw_data;
                    delete data;
               }
               
               int tag_int(s7_scheme *sc) {
                    s7_pointer res = s7_eval_c_string(sc, "(*foreign* 'type-int)");
                    if(s7_is_integer(res)){
               	  return s7_integer(res);
                    }
                    s7_error(sc,                               /* s7 is declared in xen.h, defined in xen.c */
               	   s7_make_symbol(sc, "foreign-error"),
               	   s7_cons(sc, s7_make_string(sc, "type-int not registered"), s7_nil(sc)));
                    return -1;
               }
               
               s7_pointer make_int(s7_scheme *sc, s7_pointer args) {
                    int* data = new int;
                    *data = (int) s7_number_to_integer(sc, s7_car(args));
                    int type = tag_int(sc);
                    s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
                    return obj;
               }
               
               s7_pointer ref_int(s7_scheme *sc, s7_pointer args) {
                    int* data = (int*) s7_c_object_value(s7_car(args));
               
                    return s7_make_integer(sc, *data);
               }
               
               s7_pointer set_int(s7_scheme *sc, s7_pointer args) {
                    // 2 args: (block-set! (ref) value)
                    if (s7_list_length(sc, args) != 2) {
               	  return (s7_wrong_number_of_args_error(sc,
               						"set! for int takes 2 arguments: ~S", args));
                    }
                    int* data = (int*) s7_c_object_value(s7_car(args));
                    s7_pointer s7_new_value = s7_cadr(args);
                    int new_value = s7_number_to_integer(sc, s7_new_value);
                    *data = new_value;
               
                    return (s7_new_value);
               }
               
               void bind_int(s7_scheme* sc, s7_pointer env) {
                    /* s7_pointer env = s7_inlet(sc, s7_nil(sc)); */
                    /* s7_gc_protect(sc, env); */
               
                    // --- bool ----
                    s7_int type = s7_make_c_type(sc, "<int>");
                    s7_define(sc, env, s7_make_symbol(sc, "type-int"),
               	       s7_make_integer(sc, type));
                    s7_define(sc, env, s7_make_symbol(sc, "new-int"),
               	       s7_make_function(sc, "int", make_int, 1, 0, false,
               				"creates a heap allocated int (c-object)"));
                    s7_c_type_set_ref(sc, type, ref_int);
                    s7_c_type_set_set(sc, type, set_int);
                    s7_c_type_set_free(sc, type, free_int);
               }
               
               // ! ---------------------------- int ------------------------------
               

                 // note: s7_make_real gets a double argument, not float. hope it gets automatically casted
               
               // ------------------------------ float ------------------------------
               
               void free_float(void* raw_data) {
                    float* data = (float*) raw_data;
                    delete data;
               }
               
               int tag_float(s7_scheme *sc) {
                    s7_pointer res = s7_eval_c_string(sc, "(*foreign* 'type-float)");
                    if(s7_is_integer(res)){
               	  return s7_integer(res);
                    }
                    s7_error(sc,                               /* s7 is declared in xen.h, defined in xen.c */
               	   s7_make_symbol(sc, "foreign-error"),
               	   s7_cons(sc, s7_make_string(sc, "type-float not registered"), s7_nil(sc)));
                    return -1;
               }
               
               s7_pointer make_float(s7_scheme *sc, s7_pointer args) {
                    float* data = new float;
                    *data = (float) s7_number_to_real(sc, s7_car(args));
                    int type = tag_float(sc);
                    s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
                    return obj;
               }
               
               s7_pointer ref_float(s7_scheme *sc, s7_pointer args) {
                    float* data = (float*) s7_c_object_value(s7_car(args));
               
                    return s7_make_real(sc, *data);
               }
               
               s7_pointer set_float(s7_scheme *sc, s7_pointer args) {
                    // 2 args: (block-set! (ref) value)
                    if (s7_list_length(sc, args) != 2) {
               	  return (s7_wrong_number_of_args_error(sc,
               						"set! for float takes 2 arguments: ~S", args));
                    }
                    float* data = (float*) s7_c_object_value(s7_car(args));
                    s7_pointer s7_new_value = s7_cadr(args);
                    float new_value = s7_number_to_real(sc, s7_new_value);
                    *data = new_value;
               
                    return (s7_new_value);
               }
               
               void bind_float(s7_scheme* sc, s7_pointer env) {
                    /* s7_pointer env = s7_inlet(sc, s7_nil(sc)); */
                    /* s7_gc_protect(sc, env); */
               
                    // --- bool ----
                    s7_int type = s7_make_c_type(sc, "<float>");
                    s7_define(sc, env, s7_make_symbol(sc, "type-float"),
               	       s7_make_integer(sc, type));
                    s7_define(sc, env, s7_make_symbol(sc, "new-float"),
               	       s7_make_function(sc, "float", make_float, 1, 0, false,
               				"creates a heap allocated float (c-object)"));
                    s7_c_type_set_ref(sc, type, ref_float);
                    s7_c_type_set_set(sc, type, set_float);
                    s7_c_type_set_free(sc, type, free_float);
               }
               
               // ! ---------------------------- float ------------------------------
               

               void bind_primitives(s7_scheme *sc, s7_pointer env) {
                    // either passing s7_curlet or s7_nil works..
                    // ..ugh still don't know what happens with environments
                    // s7_pointer env = s7_inlet(sc, s7_nil(sc));
                    // s7_gc_protect(sc, env);

                    // the bindings
                    bind_bool(sc, env);
                    bind_int(sc, env);
                    bind_float(sc, env);

                    s7_define(sc, s7_curlet(sc), s7_make_symbol(sc, "*foreign*"),
                              s7_sublet(sc, s7_nil(sc), s7_let_to_list(sc, env)));

                    s7_define_variable(sc, "aod.c.foreign", s7_let_to_list(sc, env));
               }
          } // foreign
     } // s7
} // aod
