#include "gtest/gtest.h"
#include "s7.h"
#include "aod/s7.hpp"
#include <string>
#include <stdio.h>

TEST(memory, demo) {
    GTEST_SKIP();
    for (int i = 0; i < 10; i++) {
        s7_scheme *sc = s7_init();
        aod::s7::bind_all(sc);
        
//         s7_eval_c_string(sc, "(display (*s7* 'memory-usage))");
        s7_eval_c_string(sc, //
                         "(define x ((aod.c.foreign 'new-char[]) 100000000))" // 100 mb
                        );
        
/*        
        for(int j=0; j<10; j++){
                s7_eval_c_string(sc, //
                         "(define x ((aod.c.foreign 'new-char[]) 10000000))" // 10 mb
                        );
        }*/
/*        
        s7_eval_c_string(sc, //
                         "(gc)" // 100 mb
                        );
        
        s7_eval_c_string(sc, //
                         "(gc)" // 100 mb
                        );
        s7_eval_c_string(sc, //
                         "(gc)" // 100 mb
                        );*/

        free(sc);
    }
    (void)1;
}
