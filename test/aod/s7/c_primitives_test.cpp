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
#include <string>
#include <stdio.h>

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
	// hmm.. in linux I get 0, in windows i get rubbish (eg -572662307)
	ASSERT_NE(30, *data);
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


TEST(c_primitives, float_arr) {
  printf("here, testing float_ar\n");
	s7_scheme *sc = s7_init();
	aod::s7::set_print_stderr(sc);
	aod::s7::bind_primitives(sc);

	s7_pointer obj = s7_eval_c_string(sc,
			"(define x (with-let *c-primitives* (float-arr 10 11 12)))");
	aod::s7::float_arr* data = (aod::s7::float_arr*) s7_c_object_value(obj);
	ASSERT_EQ(3, data->size);
	ASSERT_EQ(10, data->elements[0]);
	ASSERT_EQ(11, data->elements[1]);
	ASSERT_EQ(12, data->elements[2]);

	s7_eval_c_string(sc, "(set! (x 0) 100)");
	s7_eval_c_string(sc, "(set! (x 1) 101)");
	s7_eval_c_string(sc, "(set! (x 2) 102)");
	ASSERT_EQ(100, data->elements[0]);
	ASSERT_EQ(101, data->elements[1]);
	ASSERT_EQ(102, data->elements[2]);

	
	// should throw error
	s7_eval_c_string(sc, "(set! (x 3) 103)");
	std::string res = aod::s7::eval_write(sc, "(set! (x 3) 103)");
	// string::npos means string not found
	ASSERT_NE(std::string::npos, res.find("float-arr-set! argument 2, 3, is out of range (Index should be less than float-arr length)"));

	// hm.. calling (set! x "foo") didn't trigger the gc
	// neither (define x "bar")
	s7_eval_c_string(sc, "(define x \"no longer a c-object\")");
	s7_eval_c_string(sc, "(define x 1)");
	// gc
	// huh.. if only called once, doesn't work?
	// uhm.. I don't see the free call
	s7_eval_c_string(sc, "(gc)");
	s7_eval_c_string(sc, "(gc)");
	s7_eval_c_string(sc, "(gc)");
	ASSERT_NE(100, data->elements[0]);
	ASSERT_NE(101, data->elements[1]);
	ASSERT_NE(102, data->elements[2]);
}
