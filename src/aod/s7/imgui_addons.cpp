#include "aod/imgui/addons.hpp"
#include "s7.h"

namespace aod {
namespace s7 {
namespace imgui {

s7_pointer knob(s7_scheme *sc, s7_pointer args) {
    s7_pointer title = s7_car(args);
    if (!s7_is_string(title)) {
        return (s7_wrong_type_arg_error(sc, "imgui/knob", 1, title,
                "First argument is title, should be a string"));
    }
    s7_pointer c_float = s7_cadr(args);
    if (!s7_is_c_object(c_float)) {
        return (s7_wrong_type_arg_error(sc, "imgui/knob", 2, c_float,
                "Expeting a c-type : float"));
    }

    const char *title_char = s7_string(title);
    float *value = (float*) s7_c_object_value(c_float);
    float min = s7_number_to_real(sc, s7_caddr(args));
    float max = s7_number_to_real(sc, s7_cadddr(args));
    aod::imgui::Knob(title_char, value, min, max);
    return s7_nil(sc);
}

void bind_knob(s7_scheme *sc) {
    s7_define_function(sc, "imgui/knob", knob, // ..
            4, // req args
            0, // optional args (the open boolean pointer)
            false, // rest args
            "Draws a knob. (string label, float* value, float min, float max)");
}

}
}
}
