#include "./nfd.hpp"
#include "aod/nfd.hpp"
namespace aod {
namespace s7 {
namespace nfd {

s7_pointer open_file(s7_scheme* sc, s7_pointer args) {
    auto file = aod::nfd::open_file();
    std::string file_str = file.value_or("");

    return s7_make_string(sc, file_str.c_str());
}

void bind(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "open"),
              s7_make_function(sc, "open", open_file, 0, 0, false,
                               "Open file dialog, returns the selected file as a string"));

    s7_define(sc, s7_curlet(sc), s7_make_symbol(sc, "*nfd*"),
              s7_sublet(sc, s7_nil(sc), s7_let_to_list(sc, env)));

}
}
}
}
