#include "s7.h"
#include "aod/gl/gl.hpp"
#include <stdio.h>

namespace aod {
namespace s7 {
namespace gl {

s7_pointer save_screenshot(s7_scheme *sc, s7_pointer args) {
    s7_pointer filename = s7_car(args);
    if (!s7_is_string(filename)) {
        return (s7_wrong_type_arg_error(sc, "save-screenshot", 1, filename,
                                        "Expecting string (filename)"));
    }
    const char *char_filename = s7_string(filename);
    int res = aod::gl::save_screenshot(char_filename);
    if (res != 1) {
        return s7_error(sc, s7_make_symbol(sc, "aod.c.gl"),
                        s7_list(sc, 2,
                                s7_make_string(sc, "Could not save screenshot at ~A\n"),
                                filename
                               ));

    }
    return s7_nil(sc);
}

void bind(s7_scheme *sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "save-screenshot"),
              s7_make_function(sc, "save-screenshot", save_screenshot, 1, 0, false,
                               "(save-screenshot filename) Saves a screenshot of the current gl context"));

    s7_define_variable(sc, "aod.c.gl", env);
}

} // gl
} // s7
} // aod
