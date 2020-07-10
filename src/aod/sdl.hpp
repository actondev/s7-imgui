#pragma once
#include "SDL.h"

namespace aod {
namespace sdl {

typedef struct {
    SDL_Window *window;
    // dummy needed to initialize opengl properly apparently (at least in linux)
    SDL_Window *dummy;
} embedded_window;

embedded_window embed_window(void *pParent, SDL_WindowFlags window_flags);
void destroy_embedded(embedded_window);

}
}
