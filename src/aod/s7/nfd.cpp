#include "./nfd.hpp"
#include "aod/nfd.hpp"
namespace aod {
namespace s7 {
namespace nfd {

const char* help_open= "(open) Open file dialog";
s7_pointer open(s7_scheme* sc, s7_pointer) {
    auto file = aod::nfd::open();
    std::string file_str = file.value_or("");

    return s7_make_string(sc, file_str.c_str());
}

const char* help_save= "(save) Save file dialog";
s7_pointer save(s7_scheme* sc, s7_pointer) {
    auto file = aod::nfd::save();
    std::string file_str = file.value_or("");

    return s7_make_string(sc, file_str.c_str());
}

void bind(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "open"),
              s7_make_function(sc, "open", open, 0, 0, false,
                               help_open));
    
    s7_define(sc, env, s7_make_symbol(sc, "save"),
              s7_make_function(sc, "save", save, 0, 0, false,
                               help_save));
    
    // or call it native-file-dialog?
    s7_define_constant(sc, "aod.c.nfd", env);
}
}
}
}
