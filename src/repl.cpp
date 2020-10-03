#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "s7.h"
#include "aod/s7.hpp"
#include "aod/s7/repl.hpp"
#include <filesystem>
#include <iostream>

namespace fs = std::filesystem;
using std::cout, std::cerr, std::endl;

int main(int argc, char **argv) {
    char buffer[512];

    cout << "argv[0] " << argv[0] << " fs::current_path " << fs::current_path() << endl;
    fs::path base_path = (fs::current_path() / argv[0]).remove_filename();
    cout << "base path " << base_path << endl;

    // we are in build/ directory probably
    // TODO fix this for redistribution.
    // eg providing repl executable, and then a relative scheme/ folder
    fs::path scheme_path = base_path  / ".." / "src" / "scheme";
    // cout << "scheme path " << scheme_path << endl;

//     s7_scheme *sc = s7_init();
//     aod::s7::set_print_stderr(sc);
//     aod::s7::set_autoloads(sc);
//     aod::s7::bind_all(sc);
// 
//     s7_add_to_load_path(sc, scheme_path.c_str());
    
    s7_scheme* sc = aod::s7::init(scheme_path);
    

    if (argc >= 2) {
        cout << "Passed custom scheme file " << argv[1] << endl;
        fs::path passed_file = argv[1];
        if (!passed_file.is_absolute()) {
            passed_file = (fs::current_path() /  passed_file);
        }
        fprintf(stderr, "Passed file %s\n", passed_file.string().c_str());
        aod::s7::load_file(sc, "aod/core.scm");
        aod::s7::ns_load_file(sc, passed_file.string());
//         aod::s7::load_file(sc, passed_file.string());
//         s7_load(sc, passed_file.c_str());
    }

    aod::s7::Repl repl(sc);

    cout << "S7 Example Repl " << endl << "> ";

    while (true) {
        fgets(buffer, 512, stdin);
        if (repl.handleInput(buffer)) {
            auto result = repl.evalLastForm();
            cout << endl << result << endl << "> ";
        }

    }
}
