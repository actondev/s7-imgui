#include "s7.h"
#include "aod/img/core.hpp"
#include <stdio.h>

namespace aod {
namespace s7 {
namespace img {

s7_pointer equivalent(s7_scheme* sc, s7_pointer args) {
    s7_pointer img1 = s7_car(args);
    if (!s7_is_string(img1))
        return (s7_wrong_type_arg_error(sc, "equivalent?", 1, img1,
                                        "img1-filename string"));

    s7_pointer img2 = s7_cadr(args);
    if (!s7_is_string(img2))
        return (s7_wrong_type_arg_error(sc, "equivalent?", 2, img2,
                                        "img2-filename string"));

    const char* img1_char = s7_string(img1);
    const char* img2_char = s7_string(img2);
    bool res = aod::img::are_equivalent(img1_char, img2_char);
    fprintf(stderr, "tested %s against %s, result %d\n", img1_char, img2_char, res ? 1 : 0);

    return s7_make_boolean(sc, res);
}

void bind(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "equivalent?"),
              s7_make_function(sc, "equivalent?", equivalent, 2, 0, false,
                               "(equivalent? img1-filename img2-filename) Returns true if the images are equivalent"));

    s7_define_variable(sc, "aod.c.img", env);

}
} //img
} // s7
} // aod
