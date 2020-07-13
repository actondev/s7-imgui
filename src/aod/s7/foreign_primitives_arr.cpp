/**
 * This file was auto-generated with org-babel tangle.
 * See foreign_types.org for more info
 * I would suggest to not edit this by hand.
 **/

#include "s7.h"
#include <stdio.h> // fprintf etc
#include "foreign_primitives.hpp" // needed when I access (*arr 1 '&) to get a c-object
#include "foreign_primitives_arr.hpp"

namespace aod {
     namespace s7 {
          namespace foreign {
               
               // ------------------------------ bool-arr ------------------------------
               
               void free_bool_arr(void *raw_data) {
                    bool* data = (bool*) raw_data;
                    delete[] data;
               }
               
               int tag_bool_arr(s7_scheme* sc){
                    s7_pointer res = s7_eval_c_string(sc, "(*foreign* 'type-bool[])");
                    if(s7_is_integer(res)){
               	  return s7_integer(res);
                    }
                    return -1;
               }
               
               
               s7_pointer make_bool_arr(s7_scheme *sc, s7_pointer args) {
                    int len = s7_integer(s7_car(args));
                    if (len == 0) {
               	  return (s7_wrong_number_of_args_error(sc,
               						"bool_arr creating needs 1 positive argument for its length: ~S", args));
                    }
                    /* fprintf(stderr, "making bool[] of length %d\n", len); */
                    bool* data = new bool[len]{}; // {} is for default initialization. eg false for bool, 0 for numbers
               
                    int type = tag_bool_arr(sc);
                    s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
               
                    return obj;
               }
               
               s7_pointer ref_bool_arr(s7_scheme *sc, s7_pointer args) {
                    bool* arr = (bool*) s7_c_object_value(s7_car(args));
                    int args_length = s7_list_length(sc, args);
                    int index = s7_integer(s7_cadr(args));
                    if (args_length == 2) {
               	  return s7_make_boolean(sc, arr[index]);
                    } else if (args_length == 3) {
               	  // we return the reference
               	  bool* data = &arr[index];
               	  int type = aod::s7::foreign::tag_bool(sc);
               	  s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
               	  return obj;
                    }
                    else {
               	  return (s7_wrong_number_of_args_error(sc,
               						"bool_arr-ref needs 2 or 3 arguments~S", args));
                    }
               }
               
               s7_pointer set_bool_arr(s7_scheme *sc, s7_pointer args) {
                    // 3 args: (block-set! data index value)
                    if (s7_list_length(sc, args) != 3)
               	  return (s7_wrong_number_of_args_error(sc,
               						"bool_arr-set! takes 3 arguments: ~S", args));
               
                    bool* arr = (bool*) s7_c_object_value(s7_car(args));
                    int index = s7_integer(s7_cadr(args));
               
                    bool new_value = s7_boolean(sc, s7_caddr(args));
               
                    arr[index] = new_value;
               
                    return (s7_cadr(args));
               }
               
               void bind_bool_arr(s7_scheme* sc, s7_pointer env) {
                    /* s7_pointer env = s7_inlet(sc, s7_nil(sc)); */
                    /* s7_gc_protect(sc, env); */
               
                    // --- bool ----
                    s7_int type = s7_make_c_type(sc, "<bool-arr>");
                    s7_define(sc, env, s7_make_symbol(sc, "type-bool[]"),
               	       s7_make_integer(sc, type));
                    s7_define(sc, env, s7_make_symbol(sc, "new-bool[]"),
               	       s7_make_function(sc, "new-bool[]", make_bool_arr, 1, 0, false,
               				"creates a heap allocated bool[] (c-object)"));
                    s7_c_type_set_ref(sc, type, ref_bool_arr);
                    s7_c_type_set_set(sc, type, set_bool_arr);
                    s7_c_type_set_free(sc, type, free_bool_arr);
               }
               
               // ! ---------------------------- bool-arr ------------------------------
               

               
               // ------------------------------ int-arr ------------------------------
               
               void free_int_arr(void *raw_data) {
                    int* data = (int*) raw_data;
                    delete[] data;
               }
               
               int tag_int_arr(s7_scheme* sc){
                    s7_pointer res = s7_eval_c_string(sc, "(*foreign* 'type-int[])");
                    if(s7_is_integer(res)){
               	  return s7_integer(res);
                    }
                    return -1;
               }
               
               
               s7_pointer make_int_arr(s7_scheme *sc, s7_pointer args) {
                    int len = s7_integer(s7_car(args));
                    if (len == 0) {
               	  return (s7_wrong_number_of_args_error(sc,
               						"int_arr creating needs 1 positive argument for its length: ~S", args));
                    }
                    /* fprintf(stderr, "making int[] of length %d\n", len); */
                    int* data = new int[len]{}; // {} is for default initialization. eg false for bool, 0 for numbers
               
                    int type = tag_int_arr(sc);
                    s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
               
                    return obj;
               }
               
               s7_pointer ref_int_arr(s7_scheme *sc, s7_pointer args) {
                    int* arr = (int*) s7_c_object_value(s7_car(args));
                    int args_length = s7_list_length(sc, args);
                    int index = s7_integer(s7_cadr(args));
                    if (args_length == 2) {
               	  return s7_make_integer(sc, arr[index]);
                    } else if (args_length == 3) {
               	  // we return the reference
               	  int* data = &arr[index];
               	  int type = aod::s7::foreign::tag_int(sc);
               	  s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
               	  return obj;
                    }
                    else {
               	  return (s7_wrong_number_of_args_error(sc,
               						"int_arr-ref needs 2 or 3 arguments~S", args));
                    }
               }
               
               s7_pointer set_int_arr(s7_scheme *sc, s7_pointer args) {
                    // 3 args: (block-set! data index value)
                    if (s7_list_length(sc, args) != 3)
               	  return (s7_wrong_number_of_args_error(sc,
               						"int_arr-set! takes 3 arguments: ~S", args));
               
                    int* arr = (int*) s7_c_object_value(s7_car(args));
                    int index = s7_integer(s7_cadr(args));
               
                    int new_value = s7_number_to_integer(sc, s7_caddr(args));
               
                    arr[index] = new_value;
               
                    return (s7_cadr(args));
               }
               
               void bind_int_arr(s7_scheme* sc, s7_pointer env) {
                    /* s7_pointer env = s7_inlet(sc, s7_nil(sc)); */
                    /* s7_gc_protect(sc, env); */
               
                    // --- bool ----
                    s7_int type = s7_make_c_type(sc, "<int-arr>");
                    s7_define(sc, env, s7_make_symbol(sc, "type-int[]"),
               	       s7_make_integer(sc, type));
                    s7_define(sc, env, s7_make_symbol(sc, "new-int[]"),
               	       s7_make_function(sc, "new-int[]", make_int_arr, 1, 0, false,
               				"creates a heap allocated int[] (c-object)"));
                    s7_c_type_set_ref(sc, type, ref_int_arr);
                    s7_c_type_set_set(sc, type, set_int_arr);
                    s7_c_type_set_free(sc, type, free_int_arr);
               }
               
               // ! ---------------------------- int-arr ------------------------------
               

                 // note: s7_make_real gets a double argument, not float. hope it gets automatically casted
               
               // ------------------------------ float-arr ------------------------------
               
               void free_float_arr(void *raw_data) {
                    float* data = (float*) raw_data;
                    delete[] data;
               }
               
               int tag_float_arr(s7_scheme* sc){
                    s7_pointer res = s7_eval_c_string(sc, "(*foreign* 'type-float[])");
                    if(s7_is_integer(res)){
               	  return s7_integer(res);
                    }
                    return -1;
               }
               
               
               s7_pointer make_float_arr(s7_scheme *sc, s7_pointer args) {
                    int len = s7_integer(s7_car(args));
                    if (len == 0) {
               	  return (s7_wrong_number_of_args_error(sc,
               						"float_arr creating needs 1 positive argument for its length: ~S", args));
                    }
                    /* fprintf(stderr, "making float[] of length %d\n", len); */
                    float* data = new float[len]{}; // {} is for default initialization. eg false for bool, 0 for numbers
               
                    int type = tag_float_arr(sc);
                    s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
               
                    return obj;
               }
               
               s7_pointer ref_float_arr(s7_scheme *sc, s7_pointer args) {
                    float* arr = (float*) s7_c_object_value(s7_car(args));
                    int args_length = s7_list_length(sc, args);
                    int index = s7_integer(s7_cadr(args));
                    if (args_length == 2) {
               	  return s7_make_real(sc, arr[index]);
                    } else if (args_length == 3) {
               	  // we return the reference
               	  float* data = &arr[index];
               	  int type = aod::s7::foreign::tag_float(sc);
               	  s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
               	  return obj;
                    }
                    else {
               	  return (s7_wrong_number_of_args_error(sc,
               						"float_arr-ref needs 2 or 3 arguments~S", args));
                    }
               }
               
               s7_pointer set_float_arr(s7_scheme *sc, s7_pointer args) {
                    // 3 args: (block-set! data index value)
                    if (s7_list_length(sc, args) != 3)
               	  return (s7_wrong_number_of_args_error(sc,
               						"float_arr-set! takes 3 arguments: ~S", args));
               
                    float* arr = (float*) s7_c_object_value(s7_car(args));
                    int index = s7_integer(s7_cadr(args));
               
                    float new_value = s7_number_to_real(sc, s7_caddr(args));
               
                    arr[index] = new_value;
               
                    return (s7_cadr(args));
               }
               
               void bind_float_arr(s7_scheme* sc, s7_pointer env) {
                    /* s7_pointer env = s7_inlet(sc, s7_nil(sc)); */
                    /* s7_gc_protect(sc, env); */
               
                    // --- bool ----
                    s7_int type = s7_make_c_type(sc, "<float-arr>");
                    s7_define(sc, env, s7_make_symbol(sc, "type-float[]"),
               	       s7_make_integer(sc, type));
                    s7_define(sc, env, s7_make_symbol(sc, "new-float[]"),
               	       s7_make_function(sc, "new-float[]", make_float_arr, 1, 0, false,
               				"creates a heap allocated float[] (c-object)"));
                    s7_c_type_set_ref(sc, type, ref_float_arr);
                    s7_c_type_set_set(sc, type, set_float_arr);
                    s7_c_type_set_free(sc, type, free_float_arr);
               }
               
               // ! ---------------------------- float-arr ------------------------------
               

                 /**
                    (define *int-arr ((*foreign* 'new-int[]) 3))
                    (*int-arr 0) => 0
                    (set! (*int-arr 0 1))
                    (*int-arr 1) => 1
               
                    ;; second argument 'ref (or 3rd for the ref function) means give me the reference, not the value
                    (define *int-1 (*int-arr 1 'ref))
                    (*int-1) => 1
                    (set! (*int-1) 2)
                    (*int-1) => 2
               
                    (*int-arr 1) => 2
               
                 **/
               void bind_primitives_arr(s7_scheme *sc, s7_pointer env) {
                    // either passing s7_curlet or s7_nil works..
                    // ..ugh still don't know what happens with environments
                    // s7_pointer env = s7_inlet(sc, s7_nil(sc));
                    // s7_gc_protect(sc, env);

                    // the bindings
                    bind_bool_arr(sc, env);
                    bind_int_arr(sc, env);
                    bind_float_arr(sc, env);

                    s7_define(sc, s7_curlet(sc), s7_make_symbol(sc, "*foreign*"),
                              s7_sublet(sc, s7_nil(sc), s7_let_to_list(sc, env)));

                    s7_define_variable(sc, "aod.foreign", s7_let_to_list(sc, env));
               }
          } // foreign
     } // s7
} // aod

