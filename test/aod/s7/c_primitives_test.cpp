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

TEST ( c_primitives, boolean_ref_set ) {
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

TEST ( c_primitives, boolean_pointer ) {
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

TEST ( c_primitives, int_pointer ) {
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

TEST ( c_primitives, gc_dummy ) {
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

TEST ( c_primitives, styles_of_call ) {
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

TEST ( c_primitives, float_arr ) {
    printf("here, testing float_ar\n");
    s7_scheme *sc = s7_init();
    aod::s7::set_print_stderr(sc);
    aod::s7::bind_primitives(sc);

    s7_pointer obj = s7_eval_c_string(sc,
            "(define x (with-let *c-primitives* (float-arr 10 11 12)))");
    aod::s7::float_arr *data = (aod::s7::float_arr*) s7_c_object_value(obj);
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
    ASSERT_NE(std::string::npos,
            res.find(
                    "float-arr-set! argument 2, 3, is out of range (Index should be less than float-arr length)"));
}

bool float_arr_gc__free_called = false;
void float_arr_gc__free(void* raw_data) {
    float_arr_gc__free_called = true;
    aod::s7::float_arr* data = (aod::s7::float_arr*) raw_data;
    float *elements = data->elements;
    delete[] elements;
    delete data;
}

TEST ( c_primitives, float_arr_gc ) {
    float_arr_gc__free_called = false;

    s7_scheme *sc = s7_init();
    aod::s7::bind_primitives(sc);

    s7_int float_arr_type = s7_number_to_integer(sc,
            s7_eval_c_string(sc, "(*c-primitives* 'type-float-arr)"));

    s7_c_type_set_free(sc, float_arr_type, float_arr_gc__free);

    s7_pointer obj = s7_eval_c_string(sc,
            "(define x (with-let *c-primitives* (float-arr 10 11)))");
    aod::s7::float_arr *data = (aod::s7::float_arr*) s7_c_object_value(obj);
    ASSERT_EQ(2, data->size);
    ASSERT_EQ(10, data->elements[0]);
    ASSERT_EQ(11, data->elements[1]);

    /**
     * Problem 1: Calling the next line will cause the free method
     * to not be called after another define on the variable (thus variable no longer used)
     **/
    // s7_eval_c_string(sc, "(set! (x 0) 10)"); // this: if called, the free is not called

    s7_eval_c_string(sc, "(define x 1)");
    // the data should still be available, gc not called
    ASSERT_EQ(10, data->elements[0]);
    ASSERT_EQ(11, data->elements[1]);

    /**
     * Problem 2: Calling (gc) only once doesn't trigger the free method
     * to be called
     **/
    // gc
    // huh.. if only called once, doesn't work?
    s7_eval_c_string(sc, "(gc)");
    s7_eval_c_string(sc, "(gc)");
    ASSERT_TRUE(float_arr_gc__free_called);
}

bool float_arr_gc2__free_called = false;
int float_arr_gc2__free_count = 0;
void float_arr_gc2__free(void* raw_data) {
//    printf("float_arr_gc2__free called\n");
    float_arr_gc2__free_called = true;
    float_arr_gc2__free_count++;
    aod::s7::float_arr* data = (aod::s7::float_arr*) raw_data;
    float *elements = data->elements;
    delete[] elements;
    delete data;
}

/**
 * S7 doesn't have a precise garbage collector
 */
TEST ( c_primitives, float_arr_gc2 ) {
    s7_scheme *sc = s7_init();
    aod::s7::bind_primitives(sc);

    s7_int float_arr_type = s7_number_to_integer(sc,
            s7_eval_c_string(sc, "(*c-primitives* 'type-float-arr)"));

    s7_c_type_set_free(sc, float_arr_type, float_arr_gc2__free);

    s7_eval_c_string(sc,
            "(do ((i 0 (+ i 1)))"
            "((= i 100))"
            "(let ((arr ((*c-primitives* 'float-arr) 0 1 2 3 4 5 6 7 8 9)))"
            "(set! (arr 0) i)"
            "(format #t \"~A \" i )"
            "))");

    s7_eval_c_string(sc, "(gc)");
    ASSERT_TRUE(float_arr_gc2__free_count > 0);
    int calls1 = float_arr_gc2__free_count;
    printf("\nfloat_arr_gc2__free_called times %d\n", float_arr_gc2__free_count);
    s7_eval_c_string(sc, "(gc)");
    ASSERT_TRUE(float_arr_gc2__free_count > calls1);
    printf("\nfloat_arr_gc2__free_called times %d\n", float_arr_gc2__free_count);
    printf("\nfloat_arr_gc2__free_called times %d\n", float_arr_gc2__free_count);

    ASSERT_TRUE(float_arr_gc2__free_called);

    // for some reason 99 calls are done, not 100
    ASSERT_TRUE(float_arr_gc2__free_count >= 99 && float_arr_gc2__free_count <= 100);

}
