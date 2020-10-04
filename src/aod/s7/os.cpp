#include "os.hpp"
#include <filesystem>

namespace aod {
namespace s7 {
namespace os {
namespace fs = std::filesystem;
s7_pointer temp_directory_path(s7_scheme* sc, s7_pointer) {
    return s7_make_string(sc, fs::temp_directory_path().string().c_str());
}

s7_pointer execute(s7_scheme* sc, s7_pointer args) {
    return s7_make_integer(sc, system(s7_string(s7_car(args))));
}

void bind(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    // functions
    s7_define(sc, env, s7_make_symbol(sc, "temp-directory-path"),
              s7_make_function(sc, "temp-directory-path", temp_directory_path, 0, 0, false,
                               "(temp-directory-path) Returns the directory location suitable for temporary files"));
    
    s7_define(sc, env, s7_make_symbol(sc, "execute"),
              s7_make_function(sc, "execute", execute, 1, 0, false,
                               "(execute command) Returns the external command on the system"));


    s7_define_variable(sc, "aod.c.os", env);
}
} // os
} // s7
} // aod
