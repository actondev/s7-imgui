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

    fs::path base_path = (fs::current_path() / argv[0]).remove_filename();
    // cout << "base path " << base_path << endl;
    fs::path scheme_path = base_path / ".." / ".." / "src" / "scheme";
    // cout << "scheme path " << scheme_path << endl;

    s7_scheme *sc = s7_init();
    aod::s7::set_print_stderr(sc);
    aod::s7::set_autoloads(sc);

    s7_add_to_load_path(sc, scheme_path.c_str());

    if (argc >= 2) {
        cout <<"Passed custom scheme file " << argv[1] << endl;
        fs::path passed_file = fs::current_path() / argv[1];
        s7_add_to_load_path(sc, passed_file.remove_filename().c_str());
        aod::s7::load_file(sc, passed_file.c_str());
    }

    aod::s7::Repl repl(sc);
    
    cout << "S7 Example Repl " << endl;

    while (true) {
        fgets(buffer, 512, stdin);
        if (repl.handleInput(buffer))
        {
            auto result = repl.evalLastForm();
            cout << endl << result << endl << "> ";
        }

    }
}
