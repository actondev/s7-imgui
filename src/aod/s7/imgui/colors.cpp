#include "s7.h"
#include "imgui.h"
namespace aod {
namespace s7 {
namespace imgui {
namespace colors {

// binds all the color constants under aod.c.imgui.col
void bind(s7_scheme *sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "FrameBg"),
              s7_make_integer(sc, ImGuiCol_FrameBg));
    s7_define(sc, env, s7_make_symbol(sc, "FrameBgActive"),
              s7_make_integer(sc, ImGuiCol_FrameBgActive));
    s7_define(sc, env, s7_make_symbol(sc, "FrameBgHovered"),
              s7_make_integer(sc, ImGuiCol_FrameBgHovered));

    s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "aod.c.imgui.col"), env);
}

}

} // imgui
} // s7
} // aod
