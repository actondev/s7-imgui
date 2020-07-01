#pragma once

#include "s7.h"
#include <sstream>
#include <iostream>

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

} // ! s7
} // ! aod
