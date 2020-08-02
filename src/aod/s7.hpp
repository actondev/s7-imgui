#pragma once

#include "s7.h"
#include <array>

// these are for the bind_all functions
// maybe have some definitions, eg
// #ifdef AOD_S7_IMGUI
// #ifdef AOD_S7_SDL etc ?
#include <string>
#include <filesystem>

namespace aod {
namespace s7 {

/**
 * Sets the stdout of s7 to stderr of the application
 */
void set_print_stderr(s7_scheme *sc);

/**
 * Loads a file. Prints a warning if file could not be found.
 */
void load_file(s7_scheme *sc, std::string file);

/**
 * Performs a (ns-load-file THE_FILE) call
 * Loads the file in its own namespace - if it has an (ns .. ) form
 */
void ns_load_file(s7_scheme* sc, std::string file);

/**
 * Wraps the passed sexp around a (write ..) call,
 * and returns the written output.
 */
std::string eval_write(s7_scheme *sc, const char *sexp);

/**
 * Adds the autoloading, loads aod.core, and binds all available bindings
 */ 
s7_scheme* init(std::filesystem::path init_load_path);

inline s7_pointer make_env(s7_scheme *sc) {
    // either passing s7_curlet or s7_nil works..
    // ..ugh still don't know what happens with environments
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    return env;
}

void bind_all(s7_scheme *sc);

void set_autoloads(s7_scheme *sc);

} // ! s7
} // ! aod
