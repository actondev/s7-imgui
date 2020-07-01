/*
 * c_primitives.cpp
 *
 *  Created on: Jul 1, 2020
 *      Author: actondev
 */

#include "gtest/gtest.h"
#include "s7.h"
#include "aod/s7/c_primitives.hpp"
#include "aod/s7.hpp"

void sc_modify_bool_ref(s7_scheme *sc, s7_pointer args) {

}

TEST(c_primitives, boolean_ref_set) {
	s7_scheme *sc = s7_init();
	aod::s7::set_print_stderr(sc);

	aod::s7::bind_primitives(sc);

	s7_pointer b = s7_eval_c_string(sc, //
			"(define b (with-let *c-primitives* (bool #t)))" //
			);

	ASSERT_TRUE(s7_is_c_object(b));

	ASSERT_TRUE(s7_boolean(sc, s7_eval_c_string(sc, "(b)")));

	s7_eval_c_string(sc, "(set! (b) #f)");
	ASSERT_FALSE(s7_boolean(sc, s7_eval_c_string(sc, "(b)")));
}

TEST(c_primitives, boolean_pointer) {
	s7_scheme *sc = s7_init();
	aod::s7::set_print_stderr(sc);

	aod::s7::bind_primitives(sc);

	s7_pointer b_obj = s7_eval_c_string(sc, //
			"(define b (with-let *c-primitives* (bool #t)))" //
			);
	bool *b = (bool*) s7_c_object_value(b_obj);
	ASSERT_TRUE(*b);
	s7_eval_c_string(sc, "(set! (b) #f)");
	ASSERT_FALSE(*b);
	s7_eval_c_string(sc, "(set! (b) #t)");
	ASSERT_TRUE(*b);
}

TEST(c_primitives, int_pointer) {
	s7_scheme *sc = s7_init();
	aod::s7::set_print_stderr(sc);
	aod::s7::bind_primitives(sc);

	s7_pointer obj = s7_eval_c_string(sc, //
			"(define x (with-let *c-primitives* (int 10)))" //
			);
	int *data = (int*) s7_c_object_value(obj);
	ASSERT_EQ(10, *data);
	s7_eval_c_string(sc, "(set! (x) 20)");
	ASSERT_EQ(20, *data);
	s7_eval_c_string(sc, "(set! (x) 30)");
	ASSERT_EQ(30, *data);
}

TEST(c_primitives, gc_dummy) {
	s7_scheme *sc = s7_init();
	aod::s7::set_print_stderr(sc);
	aod::s7::bind_primitives(sc);

	s7_pointer obj = s7_eval_c_string(sc, //
			"(define x (with-let *c-primitives* (int 10)))" //
			);
	int *data = (int*) s7_c_object_value(obj);
	ASSERT_EQ(10, *data);
	s7_eval_c_string(sc, "(set! (x) 20)");
	ASSERT_EQ(20, *data);
	s7_eval_c_string(sc, "(set! (x) 30)");
	ASSERT_EQ(30, *data);

	s7_eval_c_string(sc, "(define x \"haha\")");

	// huh.. if only called once, doesn't work?
	s7_eval_c_string(sc, "(gc)");
	s7_eval_c_string(sc, "(gc)");

	// hm.. what should happen?
	// it seems that now I get 0
	// though this is now a dangling pointer
	ASSERT_EQ(0, *data);
}

TEST(c_primitives, styles_of_call) {
	s7_scheme *sc = s7_init();
	aod::s7::set_print_stderr(sc);
	aod::s7::bind_primitives(sc);

	// with-let
	s7_pointer obj1 = s7_eval_c_string(sc,
			"(define x1 (with-let *c-primitives* (int 10)))");
	int *data1 = (int*) s7_c_object_value(obj1);
	ASSERT_EQ(10, *data1);

	// getting the function from the environment, calling it

	s7_pointer obj2 = s7_eval_c_string(sc,
			"(define x2 ((*c-primitives* 'int) 20))");
	int *data2 = (int*) s7_c_object_value(obj2);
	ASSERT_EQ(20, *data2);
}
