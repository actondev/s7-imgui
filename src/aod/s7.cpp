#include "s7.hpp"

#include "aod/s7/imgui/imgui.hpp"
#include "aod/s7/foreign_primitives.hpp"
#include "aod/s7/foreign_primitives_arr.hpp"
#include "aod/s7/imgui/addons.hpp"
#include "aod/s7/gl.hpp"
#include "aod/s7/sdl.hpp"
#include "aod/s7/nfd.hpp"
#include "aod/s7/imgui_sdl.hpp"
#include "aod/s7/repl.hpp"
#include "aod/s7/colors.hpp"
#include "aod/s7/img.hpp"
#include "aod/s7/midi.hpp"
#include "aod/s7/regex.hpp"

#include <sstream>
#include <iostream>
using std::cout;
using std::cerr;
using std::endl;

namespace aod {
namespace s7 {

namespace {
std::ostringstream _out_stream;
void _print_stderr(s7_scheme *sc, uint8_t c, s7_pointer port) {
    fprintf(stderr, "%c", c);
}
void _print_temp(s7_scheme *sc, uint8_t c, s7_pointer port) {
    _out_stream << c;
}
} // anonymous


void set_print_stderr(s7_scheme *sc) {
    s7_set_current_output_port(sc, s7_open_output_function(sc, _print_stderr));
}

void load_file(s7_scheme *sc, std::string file) {
    std::replace(file.begin(), file.end(), '\\', '/');
    if (!s7_load(sc, file.c_str())) {
        cerr << "Cannot load " << file << endl;
    }
}

void ns_load_file(s7_scheme* sc, std::string file) {
    // fucking windows separators
    std::replace(file.begin(), file.end(), '\\', '/');
    std::string sexp = "(ns-load-file \"" + file + "\")";
    s7_eval_c_string(sc, sexp.c_str());
}

/**
 * Wraps the passed sexp around a (write ..) call,
 * and returns the written output.
 */
std::string eval_write(s7_scheme *sc, const char *sexp) {
    std::ostringstream wrapped_sexp;
    // wrapping around begin as well to handle empty input (not enough arguments passed to write error)
    wrapped_sexp << "(write (begin " << sexp << "))";

    s7_pointer old_port = s7_set_current_output_port(sc,
                          s7_open_output_function(sc, _print_temp));

    s7_pointer old_error_port = s7_set_current_error_port(sc,
                                s7_open_output_function(sc, _print_temp));

    _out_stream.clear();
    _out_stream.str("");
    s7_eval_c_string(sc, wrapped_sexp.str().c_str());

    // reverting
    s7_set_current_output_port(sc, old_port);
    s7_set_current_error_port(sc, old_error_port);

    return _out_stream.str();
}


#define AOD_S7_AUTOLOAD 1
#define AOD_S7_AUTOLOAD_2 AOD_S7_AUTOLOAD*2
void set_autoloads(s7_scheme *sc) {
//     s7_autoload_set_names(sc, NULL, 0);
    // hm.. if I don't set this to static, everything goes to shit
    static const char *autoloads[AOD_S7_AUTOLOAD_2] = {
        // each pair of entries is entity name + file name
        "aod.core", "aod/core.scm",
//         "aod.clj", "aod/clj.scm", //
//         "aod.imgui", "aod/imgui/macros.scm",
//         "imgui-macros.scm", "aod/imgui_macros.scm", //
//         "aod.libs.lib1", "aod/imgui/macros.scm",
    };


    s7_autoload_set_names(sc, autoloads, AOD_S7_AUTOLOAD);
}

s7_scheme* init(std::filesystem::path init_load_path) {
    cout << "Initializing scheme in " << init_load_path << endl;
    s7_scheme *sc = s7_init();
    set_print_stderr(sc);
    set_autoloads(sc);
    bind_all(sc);

    // note: path.c_str() returns const value_type*
    // in linux it's indeed char* but in windows it's wchar_t
    s7_add_to_load_path(sc, init_load_path.string().c_str());
    aod::s7::load_file(sc, "aod/core.scm");

    return sc;
}

void bind_all(s7_scheme *sc) {
    s7_pointer primitives_env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, primitives_env);
    // eg ((aod.c.foreign 'new-bool) #t) for a bool* pointer with initial value true
    aod::s7::foreign::bind_primitives(sc, primitives_env);
    // eg ((aod.c.foreign 'new-bool[]) 4) for a bool[4] array
    aod::s7::foreign::bind_primitives_arr(sc, primitives_env);

    s7_define(sc, primitives_env, s7_make_symbol(sc, "*ns-doc*"),
              s7_make_string(sc, "Provides a way to create heap allocated primitives like int* float*, int* array, char* array etc. "
                             "For example, to create a c string call `(new-char[] size)`"));

    // imgui bindings
    aod::s7::imgui::bind(sc);
    aod::s7::imgui::bind_knob(sc);

    // gl bindings (eg gl/save-screenshot)
    aod::s7::gl::bind(sc);

    // nfd: native file dialog (*nfd* 'open)
    aod::s7::nfd::bind(sc);

    aod::s7::imgui_sdl::bind(sc);
    // aod.c.repl :
    // *eval-hook*
    aod::s7::repl::bind(sc);

    aod::s7::colors::bind(sc);
    aod::s7::sdl::bind(sc);
    aod::s7::img::bind(sc);
    aod::s7::midi::bind(sc);
    aod::s7::regex::bind(sc);
}

} // s7
} // aod




