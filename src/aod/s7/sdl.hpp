#pragma once

#include "SDL.h"
#include "s7.h"

namespace aod {
namespace s7 {
namespace sdl {
/**
 * Notice extra space to avoid c++ warning -Wcomment
 * defines under sdl/ *window* and (sdl/set-window-size w h)
 */
void bind(s7_scheme *sc, SDL_Window *window);
}
}
}
