#include <gtest/gtest.h>
#include "s7.h"
#include "aod/s7/curl.hpp"
#include "aod/s7/json.hpp"

TEST(curl, get) {
    s7_scheme* sc = s7_init();
    aod::s7::curl::bind(sc);
    aod::s7::json::bind(sc);
    
    s7_eval_c_string(sc, "(define curl (aod.c.curl 'curl))");
    s7_eval_c_string(sc, "(define parse (aod.c.json 'parse))");
    
    s7_eval_c_string(sc, "(define res (curl \"https://httpbin.org/get?foo=bar\" :opts (inlet 'ssl-verify-peer 0)))");
    s7_eval_c_string(sc, "(define parsed (parse res))");
    s7_pointer foo = s7_eval_c_string(sc, "(define foo (parsed \"args\" \"foo\"))");
    
    ASSERT_STREQ("bar", s7_string(foo));
    // TODO remove possible httpbin.json file
    // s7_eval_c_string(sc, "(define res2 (curl \"https://httpbin.org/get?foo=bar\" :out \"httpbin.json\" :opts (inlet 'ssl-verify-peer 0)))");
    // TODO test that file httpbin.json exists, open, parse with json again
    
    // TODO s7_free crashes with star function definition
    // ok, fixed in upstream (probably around 24-9-2020)
    s7_free(sc);
}
