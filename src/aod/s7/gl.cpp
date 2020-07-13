#include "s7.h"
#include "aod/gl/gl.hpp"
#include <stdio.h>

namespace aod {
namespace s7 {
namespace gl {

s7_pointer save_screenshot(s7_scheme *sc, s7_pointer args) {
    s7_pointer filename = s7_car(args);
    if (!s7_is_string(filename)) {
        return (s7_wrong_type_arg_error(sc, "gl/save-screenshot", 1, filename,
                "Expecting string (filename)"));
    }
    const char *char_filename = s7_string(filename);
    int res = aod::gl::save_screenshot(char_filename);
    if (res != 1) {
        fprintf(stderr, "could not save screenshot at %s\n", char_filename);
    }
    return s7_nil(sc);
}

void bind(s7_scheme *sc) {
    s7_define_function(sc, "gl/save-screenshot", save_screenshot, // ..
            1, // req args
            0, // optional args (the open boolean pointer)
            false, // rest args
            "Saves a screenshot of the gl context");
}

} // gl
} // s7
} // aod
