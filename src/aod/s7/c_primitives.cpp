#include "s7.h"
#include <stdio.h>
namespace aod {
namespace s7 {

void primitive_free(void *data) {
//	fprintf(stderr, "free called\n");
	delete data;
}

s7_pointer primitive_free_gc(s7_scheme *sc, s7_pointer obj) {
//	fprintf(stderr, "free_gc called\n");
	void* data = s7_c_object_value(obj);
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
//	s7_c_type_set_gc_free(sc, int_type, primitive_free_gc);

	s7_define(sc, s7_curlet(sc), s7_make_symbol(sc, "*c-primitives*"),
			s7_sublet(sc, s7_nil(sc), s7_let_to_list(sc, env)));

	// that would be the same:
	//	s7_define_variable(sc, "c-primitives-exports", s7_let_to_list(sc, env));
	// s7_eval_c_string(sc, "(define *c-primitives2* (apply sublet (curlet) c-primitives-exports))");
}

} // ! s7
} // ! aod
