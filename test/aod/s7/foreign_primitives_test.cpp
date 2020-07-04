#include "gtest/gtest.h"
#include "s7.h"
#include "aod/s7/foreign_primitives.hpp"
#include "aod/s7.hpp"
#include <string>
#include <stdio.h>

TEST ( foreign_primitives_gen, boolean ) {
    s7_scheme *sc = s7_init();
    aod::s7::set_print_stderr(sc);

    aod::s7::foreign::bind_primitives(sc);

    s7_pointer b = s7_eval_c_string(sc, //
            "(define b ((*foreign* 'new-bool) #t))" //
            );

    ASSERT_TRUE(s7_is_c_object(b));

    ASSERT_TRUE(s7_boolean(sc, s7_eval_c_string(sc, "(b)")));

    s7_eval_c_string(sc, "(set! (b) #f)");
    ASSERT_FALSE(s7_boolean(sc, s7_eval_c_string(sc, "(b)")));
}

TEST ( foreign_primitives_gen, integer ) {
    s7_scheme *sc = s7_init();
    aod::s7::set_print_stderr(sc);

    aod::s7::foreign::bind_primitives(sc);

    s7_pointer x = s7_eval_c_string(sc, //
            "(define x ((*foreign* 'new-int) 5))" //
            );

    ASSERT_TRUE(s7_is_c_object(x));

    ASSERT_EQ(5, s7_number_to_integer(sc, s7_eval_c_string(sc, "(x)")));

    s7_eval_c_string(sc, "(set! (x) 10)");
    ASSERT_EQ(10, s7_number_to_integer(sc, s7_eval_c_string(sc, "(x)")));

    // changing value in c by changing the pointer reference
    int* p_x = (int*) s7_c_object_value(s7_eval_c_string(sc, "x"));
    *p_x = 15;
    ASSERT_EQ(15, s7_number_to_integer(sc, s7_eval_c_string(sc, "(x)")));

}
