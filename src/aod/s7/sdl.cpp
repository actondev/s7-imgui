#include "./sdl.hpp"
#include "s7.h"

namespace aod {
namespace s7 {
namespace sdl {

s7_pointer set_window_size(s7_scheme *sc, s7_pointer args) {
    int w = s7_number_to_integer(sc, s7_car(args));
    int h = s7_number_to_integer(sc, s7_cadr(args));

    SDL_Window *win = (SDL_Window*) s7_c_pointer(
            s7_eval_c_string(sc, "sdl/*window*"));
    SDL_SetWindowSize(win, w, h);
    return s7_nil(sc);
}

void bind(s7_scheme *sc, SDL_Window *window) {
    s7_pointer sc_sdl_window = s7_make_c_pointer(sc, window);
    // not sure if this is needed
    s7_gc_protect(sc, sc_sdl_window);
    s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "sdl/*window*"),
            sc_sdl_window);

    s7_define_function(sc, "sdl/set-window-size!", set_window_size, 2, 0, false,
            "(w h) sets the size of the sdl window");

}
}
}
}
