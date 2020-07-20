#include "s7.h"
#include <array>
#include "aod/colors.hpp"

namespace aod {
namespace s7 {
namespace colors {

s7_pointer ryb2rgb(s7_scheme* sc, s7_pointer args) {
    s7_pointer sc_ryb = s7_car(args);
    double r = s7_real(s7_list_ref(sc, sc_ryb, 0));
    double y = s7_real(s7_list_ref(sc, sc_ryb, 1));
    double b = s7_real(s7_list_ref(sc, sc_ryb, 2));

    std::array<double, 3> ryb = {r, y, b};
    auto rgb = aod::colors::ryb2rgb(ryb);

    return s7_list(sc, 3,
                   s7_make_real(sc, rgb[0]),
                   s7_make_real(sc, rgb[1]),
                   s7_make_real(sc, rgb[2])
                  );
}

void bind(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "ryb->rgb"),
              s7_make_function(sc, "ryb->rgb", ryb2rgb,
                               1, // req args
                               0, // optional args: thickness
                               false, // rest args
                               "Converts RYB to RGB"));


    s7_define_variable(sc, "aod.c.colors", env);

}

} // colors
} // s7
} // aod
