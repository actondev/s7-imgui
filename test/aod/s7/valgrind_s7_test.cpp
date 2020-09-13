#include "gtest/gtest.h"
#include "s7.h"
#include "aod/s7.hpp"
#include <string>
#include <stdio.h>

TEST(valgrind, s7_double_loop) {
    for (int i = 0; i < 3; i++) {
        s7_scheme *sc = s7_init();
        aod::s7::bind_all(sc);

        for (int j = 0; j < 3; j++) {
            s7_eval_c_string(sc, //
                             "(define x ((aod.c.foreign 'new-char[]) 10000000))" // 10 mb
                            );
        }
        s7_free(sc);
    }
    (void)1;
}

TEST(valgrind, s7_single_loop) {
    for (int i = 0; i < 3; i++) {
        s7_scheme *sc = s7_init();
        aod::s7::bind_all(sc); // binds the aod.c.foreign
        s7_eval_c_string(sc,
                         "(define x ((aod.c.foreign 'new-char[]) 100000000))" // ~100 mb
                        );
        s7_free(sc);
    }
    (void)1;
}

