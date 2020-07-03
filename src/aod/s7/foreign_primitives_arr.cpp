/**
 * This file was auto-generated with org-babel tangle.
 * See foreign_types.org for more info
 * I would suggest to not edit this by hand.
 **/

#include "s7.h"
#include "foreign_primitives_arr.hpp"

namespace aod {
     namespace s7 {
	  
	  // ------------------------------ bool-arr ------------------------------
	  
	  /**
	  defined in the header
	  
	  typedef struct {
	  	size_t size;
	  	bool* elements;
	  } bool_arr;
	   **/
	  
	  void free_bool_arr(void *raw_data) {
	        bool* data = (bool*) raw_data;
	        bool *elements = data->elements;
	        delete[] elements;
	        delete data;
	  }
	  
	  s7_pointer make_bool_arr(s7_scheme *sc, s7_pointer args) {
	  	int len = s7_list_length(sc, args);
	  	if (len == 0) {
	  		return (s7_wrong_number_of_args_error(sc,
	  				"bool_arr creating needs >0 arguments: ~S", args));
	  	}
	  	s7_pointer p = args;
	  	bool_arr* data = new bool_arr;
	  	data->size = len;
	  	data->elements = new bool[len];
	  	for (int i = 0; i < len; i++) {
	  		data->elements[i] = (bool) s7_boolean(sc, s7_car(p));
	  		p = s7_cdr(p);
	  	}
	  	
	  	int type = s7_integer(s7_eval_c_string(sc, "(*foreign* 'type-bool-arr)"));
	  
	  	s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
	  
	  	return obj;
	  }
	  
	  s7_pointer ref_bool_arr(s7_scheme *sc, s7_pointer args) {
	  	bool_arr* data = (bool_arr*) s7_c_object_value(s7_car(args));
	  	int index = s7_integer(s7_cadr(args));
	  	if (index >= data->size) {
	  		return (s7_out_of_range_error(sc, "float-arr-ref", 2, s7_cadr(args),
	  				"should be less than float-arr length"));
	  	}
	  
	  	return s7_make_boolean(sc, data->elements[index]);
	  }
	  
	  s7_pointer set_bool_arr(s7_scheme *sc, s7_pointer args) {
	  	// 3 args: (block-set! data index value)
	  	if (s7_list_length(sc, args) != 3)
	  		return (s7_wrong_number_of_args_error(sc,
	  				"float-set! takes 3 arguments: ~S", args));
	  
	  	bool_arr* data = (bool_arr*) s7_c_object_value(s7_car(args));
	  	int index = s7_integer(s7_cadr(args));
	  	if (index >= data->size) {
	  		return (s7_out_of_range_error(sc, "bool-arr-set!", 2, s7_cadr(args),
	  				"Index should be less than bool-arr length"));
	  	}
	  
	  	bool new_value = s7_boolean(sc, s7_caddr(args));
	  
	  	data->elements[index] = new_value;
	  
	  	return (s7_cadr(args));
	  }
	  
	  void bind_bool_arr(s7_scheme* sc, s7_pointer env) {
	       /* s7_pointer env = s7_inlet(sc, s7_nil(sc)); */
	       /* s7_gc_protect(sc, env); */
	  
	       // --- bool ----
	       s7_int type = s7_make_bool(sc, "<bool-arr>");
	       s7_define(sc, env, s7_make_symbol(sc, "type-bool-arr"),
	  	       s7_make_integer(sc, type_bool));
	       s7_define(sc, env, s7_make_symbol(sc, "new-bool-arr"),
	  	       s7_make_function(sc, "new-bool-arr", make_bool, 1, 0, false,
	  				"creates a heap allocated bool-arr (c-object)"));
	       s7_bool_set_ref(sc, type, ref_bool_arr);
	       s7_bool_set_set(sc, type, set_bool_arr);
	       s7_bool_set_free(sc, type, free_bool_arr);
	  }
	  
	  // ! ---------------------------- bool-arr ------------------------------
	  

	  
	  // ------------------------------ int-arr ------------------------------
	  
	  /**
	  defined in the header
	  
	  typedef struct {
	  	size_t size;
	  	int* elements;
	  } int_arr;
	   **/
	  
	  void free_int_arr(void *raw_data) {
	        int* data = (int*) raw_data;
	        int *elements = data->elements;
	        delete[] elements;
	        delete data;
	  }
	  
	  s7_pointer make_int_arr(s7_scheme *sc, s7_pointer args) {
	  	int len = s7_list_length(sc, args);
	  	if (len == 0) {
	  		return (s7_wrong_number_of_args_error(sc,
	  				"int_arr creating needs >0 arguments: ~S", args));
	  	}
	  	s7_pointer p = args;
	  	int_arr* data = new int_arr;
	  	data->size = len;
	  	data->elements = new int[len];
	  	for (int i = 0; i < len; i++) {
	  		data->elements[i] = (int) s7_number_to_integer(sc, s7_car(p));
	  		p = s7_cdr(p);
	  	}
	  	
	  	int type = s7_integer(s7_eval_c_string(sc, "(*foreign* 'type-int-arr)"));
	  
	  	s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
	  
	  	return obj;
	  }
	  
	  s7_pointer ref_int_arr(s7_scheme *sc, s7_pointer args) {
	  	int_arr* data = (int_arr*) s7_c_object_value(s7_car(args));
	  	int index = s7_integer(s7_cadr(args));
	  	if (index >= data->size) {
	  		return (s7_out_of_range_error(sc, "float-arr-ref", 2, s7_cadr(args),
	  				"should be less than float-arr length"));
	  	}
	  
	  	return s7_make_integer(sc, data->elements[index]);
	  }
	  
	  s7_pointer set_int_arr(s7_scheme *sc, s7_pointer args) {
	  	// 3 args: (block-set! data index value)
	  	if (s7_list_length(sc, args) != 3)
	  		return (s7_wrong_number_of_args_error(sc,
	  				"float-set! takes 3 arguments: ~S", args));
	  
	  	int_arr* data = (int_arr*) s7_c_object_value(s7_car(args));
	  	int index = s7_integer(s7_cadr(args));
	  	if (index >= data->size) {
	  		return (s7_out_of_range_error(sc, "int-arr-set!", 2, s7_cadr(args),
	  				"Index should be less than int-arr length"));
	  	}
	  
	  	int new_value = s7_number_to_integer(sc, s7_caddr(args));
	  
	  	data->elements[index] = new_value;
	  
	  	return (s7_cadr(args));
	  }
	  
	  void bind_int_arr(s7_scheme* sc, s7_pointer env) {
	       /* s7_pointer env = s7_inlet(sc, s7_nil(sc)); */
	       /* s7_gc_protect(sc, env); */
	  
	       // --- bool ----
	       s7_int type = s7_make_int(sc, "<int-arr>");
	       s7_define(sc, env, s7_make_symbol(sc, "type-int-arr"),
	  	       s7_make_integer(sc, type_int));
	       s7_define(sc, env, s7_make_symbol(sc, "new-int-arr"),
	  	       s7_make_function(sc, "new-int-arr", make_int, 1, 0, false,
	  				"creates a heap allocated int-arr (c-object)"));
	       s7_int_set_ref(sc, type, ref_int_arr);
	       s7_int_set_set(sc, type, set_int_arr);
	       s7_int_set_free(sc, type, free_int_arr);
	  }
	  
	  // ! ---------------------------- int-arr ------------------------------
	  

	  // note: s7_make_real gets a double argument, not float. hope it gets automatically casted
	  
	  // ------------------------------ float-arr ------------------------------
	  
	  /**
	  defined in the header
	  
	  typedef struct {
	  	size_t size;
	  	float* elements;
	  } float_arr;
	   **/
	  
	  void free_float_arr(void *raw_data) {
	        float* data = (float*) raw_data;
	        float *elements = data->elements;
	        delete[] elements;
	        delete data;
	  }
	  
	  s7_pointer make_float_arr(s7_scheme *sc, s7_pointer args) {
	  	int len = s7_list_length(sc, args);
	  	if (len == 0) {
	  		return (s7_wrong_number_of_args_error(sc,
	  				"float_arr creating needs >0 arguments: ~S", args));
	  	}
	  	s7_pointer p = args;
	  	float_arr* data = new float_arr;
	  	data->size = len;
	  	data->elements = new float[len];
	  	for (int i = 0; i < len; i++) {
	  		data->elements[i] = (float) s7_number_to_real(sc, s7_car(p));
	  		p = s7_cdr(p);
	  	}
	  	
	  	int type = s7_integer(s7_eval_c_string(sc, "(*foreign* 'type-float-arr)"));
	  
	  	s7_pointer obj = s7_make_c_object(sc, type, (void*) data);
	  
	  	return obj;
	  }
	  
	  s7_pointer ref_float_arr(s7_scheme *sc, s7_pointer args) {
	  	float_arr* data = (float_arr*) s7_c_object_value(s7_car(args));
	  	int index = s7_integer(s7_cadr(args));
	  	if (index >= data->size) {
	  		return (s7_out_of_range_error(sc, "float-arr-ref", 2, s7_cadr(args),
	  				"should be less than float-arr length"));
	  	}
	  
	  	return s7_make_real(sc, data->elements[index]);
	  }
	  
	  s7_pointer set_float_arr(s7_scheme *sc, s7_pointer args) {
	  	// 3 args: (block-set! data index value)
	  	if (s7_list_length(sc, args) != 3)
	  		return (s7_wrong_number_of_args_error(sc,
	  				"float-set! takes 3 arguments: ~S", args));
	  
	  	float_arr* data = (float_arr*) s7_c_object_value(s7_car(args));
	  	int index = s7_integer(s7_cadr(args));
	  	if (index >= data->size) {
	  		return (s7_out_of_range_error(sc, "float-arr-set!", 2, s7_cadr(args),
	  				"Index should be less than float-arr length"));
	  	}
	  
	  	float new_value = s7_number_to_real(sc, s7_caddr(args));
	  
	  	data->elements[index] = new_value;
	  
	  	return (s7_cadr(args));
	  }
	  
	  void bind_float_arr(s7_scheme* sc, s7_pointer env) {
	       /* s7_pointer env = s7_inlet(sc, s7_nil(sc)); */
	       /* s7_gc_protect(sc, env); */
	  
	       // --- bool ----
	       s7_int type = s7_make_float(sc, "<float-arr>");
	       s7_define(sc, env, s7_make_symbol(sc, "type-float-arr"),
	  	       s7_make_integer(sc, type_float));
	       s7_define(sc, env, s7_make_symbol(sc, "new-float-arr"),
	  	       s7_make_function(sc, "new-float-arr", make_float, 1, 0, false,
	  				"creates a heap allocated float-arr (c-object)"));
	       s7_float_set_ref(sc, type, ref_float_arr);
	       s7_float_set_set(sc, type, set_float_arr);
	       s7_float_set_free(sc, type, free_float_arr);
	  }
	  
	  // ! ---------------------------- float-arr ------------------------------
	  

	  void bind_primitives_arr(s7_scheme *sc) {
	       // either passing s7_curlet or s7_nil works..
	       // ..ugh still don't know what happens with environments
	       s7_pointer env = s7_inlet(sc, s7_nil(sc));
	       s7_gc_protect(sc, env);

	       // the bindings
	       bind_bool_arr(sc, env);
	       bind_int_arr(sc, env);
	       bind_float_arr(sc, env);
	  }

     } // s7
} // aod
