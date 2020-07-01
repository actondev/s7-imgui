#include "s7.h"
#include <stdio.h>
#include "aod/s7/c_primitives.hpp"

namespace aod {
namespace s7 {

void primitive_free(void *data) {
//	fprintf(stderr, "free called\n");
	delete data;
}

s7_pointer primitive_free_gc(s7_scheme *sc, s7_pointer obj) {
//	fprintf(stderr, "free_gc called\n");
	void *data = s7_c_object_value(obj);
	delete data;

	return NULL;
}

namespace { // bool impl

s7_pointer bool_make(s7_scheme *sc, s7_pointer args) {
	bool *data = new bool;
	*data = s7_boolean(sc, s7_car(args));
	int type = s7_integer(s7_eval_c_string(sc, "(*c-primitives* 'type-bool)"));
//	printf("got c type bool %d\n", type);
	s7_pointer obj = s7_make_c_object(sc, type, (void*) data);

	return obj;
}

s7_pointer bool_ref(s7_scheme *sc, s7_pointer args) {
	bool *data = (bool*) s7_c_object_value(s7_car(args));

	return s7_make_boolean(sc, *data);
}

s7_pointer bool_set(s7_scheme *sc, s7_pointer args) {
	// 2 args: (block-set! (ref) value)
	if (s7_list_length(sc, args) != 2)
		return (s7_wrong_number_of_args_error(sc,
				"bool-set! takes 2 arguments: ~S", args));

	bool *data = (bool*) s7_c_object_value(s7_car(args));

	bool new_value = s7_boolean(sc, s7_cadr(args));
	*data = new_value;

	return (s7_cadr(args));
}
} // ! bool impl

namespace { // int impl
s7_pointer int_make(s7_scheme *sc, s7_pointer args) {
	int *data = new int;
	*data = (int) s7_number_to_integer(sc, s7_car(args));
	int type = s7_integer(s7_eval_c_string(sc, "(*c-primitives* 'type-int)"));
//	printf("got c type int %d\n", type);
	s7_pointer obj = s7_make_c_object(sc, type, (void*) data);

	return obj;
}

s7_pointer int_ref(s7_scheme *sc, s7_pointer args) {
	int *data = (int*) s7_c_object_value(s7_car(args));

	return s7_make_integer(sc, *data);
}

s7_pointer int_set(s7_scheme *sc, s7_pointer args) {
	// 2 args: (block-set! (ref) value)
	if (s7_list_length(sc, args) != 2)
		return (s7_wrong_number_of_args_error(sc,
				"int-set! takes 2 arguments: ~S", args));

	int *data = (int*) s7_c_object_value(s7_car(args));

	int new_value = s7_number_to_integer(sc, s7_cadr(args));
	*data = new_value;

	return (s7_cadr(args));
}
} // ! int impl

namespace { // float impl

s7_pointer float_make(s7_scheme *sc, s7_pointer args) {
	float *data = new float;
	*data = (float) s7_number_to_integer(sc, s7_car(args));
	float type = s7_integer(
			s7_eval_c_string(sc, "(*c-primitives* 'type-float)"));
	s7_pointer obj = s7_make_c_object(sc, type, (void*) data);

	return obj;
}

s7_pointer float_ref(s7_scheme *sc, s7_pointer args) {
	float *data = (float*) s7_c_object_value(s7_car(args));

	return s7_make_integer(sc, *data);
}

s7_pointer float_set(s7_scheme *sc, s7_pointer args) {
	// 2 args: (block-set! (ref) value)
	if (s7_list_length(sc, args) != 2)
		return (s7_wrong_number_of_args_error(sc,
				"float-set! takes 2 arguments: ~S", args));

	float *data = (float*) s7_c_object_value(s7_car(args));

	float new_value = (float) s7_number_to_real(sc, s7_cadr(args));
	*data = new_value;

	return (s7_cadr(args));
}
} // ! float impl

s7_int float_arr_type(s7_scheme *sc){
	return s7_integer(s7_eval_c_string(sc, "(*c-primitives* 'type-float-arr)"));
}
namespace { // float-arr impl

void float_arr_free(void *raw_data) {
	float_arr *data = (float_arr*) raw_data;
	float *elements = data->elements;
	delete[] elements;
	delete data;
}

s7_pointer float_arr_make(s7_scheme *sc, s7_pointer args) {
	int len = s7_list_length(sc, args);
	if (len == 0) {
		return (s7_wrong_number_of_args_error(sc,
				"float_arr_make takes >0 arguments: ~S", args));
	}
	s7_pointer p = args;
	float_arr *data = new float_arr;
	data->size = len;
	data->elements = new float[len];
	for (int i = 0; i < len; i++) {
		data->elements[i] = (float) s7_number_to_real(sc, s7_car(p));
		p = s7_cdr(p);
	}

	float type = float_arr_type(sc);

	s7_pointer obj = s7_make_c_object(sc, type, (void*) data);

	return obj;
}

s7_pointer float_arr_ref(s7_scheme *sc, s7_pointer args) {
	float_arr *data = (float_arr*) s7_c_object_value(s7_car(args));
	int index = s7_integer(s7_cadr(args));
	if (index >= data->size) {
		return (s7_out_of_range_error(sc, "float-arr-ref", 2, s7_cadr(args),
				"should be less than float-arr length"));
	}

	return s7_make_real(sc, (s7_double) data->elements[index]);
}

s7_pointer float_arr_set(s7_scheme *sc, s7_pointer args) {
	// 2 args: (block-set! data index value)
	if (s7_list_length(sc, args) != 3)
		return (s7_wrong_number_of_args_error(sc,
				"float-set! takes 3 arguments: ~S", args));

	float_arr *data = (float_arr*) s7_c_object_value(s7_car(args));
	int index = s7_integer(s7_cadr(args));
	if (index >= data->size) {
		return (s7_out_of_range_error(sc, "float-arr-set!", 2, s7_cadr(args),
				"Index should be less than float-arr length"));
	}

	float new_value = s7_number_to_real(sc, s7_caddr(args));

	data->elements[index] = new_value;

	return (s7_cadr(args));
}
} // ! float impl

void bind_primitives(s7_scheme *sc) {

	// either passing s7_curlet or s7_nil works..
	// ..ugh still don't know what happens with environments
	s7_pointer env = s7_inlet(sc, s7_nil(sc));
	s7_gc_protect(sc, env);

	// --- bool ----
	s7_int bool_type = s7_make_c_type(sc, "<bool>");
	// maybe renaming to bool-type ??
	s7_define(sc, env, s7_make_symbol(sc, "type-bool"),
			s7_make_integer(sc, bool_type));
	s7_define(sc, env, s7_make_symbol(sc, "bool"),
			s7_make_function(sc, "bool", bool_make, 1, 0, false,
					"creates a heap allocated bool (c-object)"));
	s7_c_type_set_ref(sc, bool_type, bool_ref);
	s7_c_type_set_set(sc, bool_type, bool_set);
	s7_c_type_set_free(sc, bool_type, primitive_free);
	//	s7_c_type_set_gc_free(sc, int_type, primitive_free_gc);

	// --- int ---
	s7_int int_type = s7_make_c_type(sc, "<int>");
	s7_define(sc, env, s7_make_symbol(sc, "type-int"),
			s7_make_integer(sc, int_type));
	s7_define(sc, env, s7_make_symbol(sc, "int"),
			s7_make_function(sc, "int", int_make, 1, 0, false,
					"creates a heap allocated int (c-object)"));
	s7_c_type_set_ref(sc, int_type, int_ref);
	s7_c_type_set_set(sc, int_type, int_set);
	s7_c_type_set_free(sc, int_type, primitive_free);

	// --- float ---
	s7_int float_type = s7_make_c_type(sc, "<float>");
	s7_define(sc, env, s7_make_symbol(sc, "type-float"),
			s7_make_integer(sc, int_type));
	s7_define(sc, env, s7_make_symbol(sc, "float"),
			s7_make_function(sc, "float", float_make, 1, 0, false,
					"creates a heap allocated float (c-object)"));
	s7_c_type_set_ref(sc, float_type, int_ref);
	s7_c_type_set_set(sc, float_type, int_set);
	s7_c_type_set_free(sc, float_type, primitive_free);

	// --- float-arr ---
	s7_int float_arr_type = s7_make_c_type(sc, "<float-arr>");
	s7_define(sc, env, s7_make_symbol(sc, "type-float-arr"),
			s7_make_integer(sc, float_arr_type));
	s7_define(sc, env, s7_make_symbol(sc, "float-arr"),
			s7_make_function(sc, "float-arr", float_arr_make, 0, 0, true, // rest args
					"creates a heap allocated float (c-object)"));
	s7_c_type_set_ref(sc, float_arr_type, float_arr_ref);
	s7_c_type_set_set(sc, float_arr_type, float_arr_set);
	s7_c_type_set_free(sc, float_arr_type, float_arr_free);

	// ------

	s7_define(sc, s7_curlet(sc), s7_make_symbol(sc, "*c-primitives*"),
			s7_sublet(sc, s7_nil(sc), s7_let_to_list(sc, env)));

	// that would be the same:
	//	s7_define_variable(sc, "c-primitives-exports", s7_let_to_list(sc, env));
	// s7_eval_c_string(sc, "(define *c-primitives2* (apply sublet (curlet) c-primitives-exports))");
}

} // ! s7
} // ! aod
