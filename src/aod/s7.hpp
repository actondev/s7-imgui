#pragma once

#include "s7.h"
#include <sstream>
#include <iostream>

// these are for the bind_all functions
// maybe have some definitions, eg
// #ifdef AOD_S7_IMGUI
// #ifdef AOD_S7_SDL etc ?
#include "aod/s7/imgui.hpp"
#include "aod/s7/foreign_primitives.hpp"
#include "aod/s7/foreign_primitives_arr.hpp"
#include "aod/s7/imgui_addons.hpp"
#include "aod/s7/gl.hpp"
#include "aod/s7/sdl.hpp"
#include "aod/s7/nfd.hpp"
#include "aod/s7/imgui_sdl.hpp"

namespace aod {
namespace s7 {

/**
 * Sets the stdout of s7 to stderr of the application
 */
void set_print_stderr(s7_scheme *sc);

/**
 * Loads a file. Prints a warning if file could not be found.
 */
void load_file(s7_scheme *sc, const char *file);

/**
 * Wraps the passed sexp around a (write ..) call,
 * and returns the written output.
 */
std::string eval_write(s7_scheme *sc, const char *sexp);

inline s7_pointer make_env(s7_scheme *sc) {
    // either passing s7_curlet or s7_nil works..
    // ..ugh still don't know what happens with environments
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    return env;
}

inline void bind_all(s7_scheme *sc) {
    s7_pointer primitives_env = make_env(sc);
    // eg ((aod.c.foreign 'new-bool) #t) for a bool* pointer with initial value true
    aod::s7::foreign::bind_primitives(sc, primitives_env);
    // eg ((aod.c.foreign 'new-bool[]) 4) for a bool[4] array
    aod::s7::foreign::bind_primitives_arr(sc, primitives_env);

    // imgui bindings
    aod::s7::imgui::bind(sc);
    aod::s7::imgui::bind_knob(sc);

    // gl bindings (eg gl/save-screenshot)
    aod::s7::gl::bind(sc);

    // nfd: native file dialog (*nfd* 'open)
    aod::s7::nfd::bind(sc);

    aod::s7::imgui_sdl::bind(sc);
}

void set_autoloads(s7_scheme *sc);

} // ! s7
} // ! aod
