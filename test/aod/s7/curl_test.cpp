#include <gtest/gtest.h>
#include "s7.h"
#include "aod/s7/curl.hpp"

TEST(curl, get) {
    s7_scheme* sc = s7_init();
    aod::s7::curl::bind(sc);
    
    s7_eval_c_string(sc, "(define curl (aod.c.curl 'curl))");
    
    s7_eval_c_string(sc, "(display (curl \"https://httpbin.org/get\"))");
    
    s7_free(sc);
}
