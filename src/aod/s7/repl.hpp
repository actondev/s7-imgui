#pragma once

#include <cstdio>
#include "s7.h"
#include <string>
#include <regex>

namespace aod {
namespace s7 {

class Repl {
private:
    s7_scheme *sc;
    // holds the user input. it fills until a valid sexp is introduced
    std::string input_buffer;
    s7_pointer last_form;

public:

    inline Repl() :
        Repl(s7_init()) {
    }
    Repl(s7_scheme *sc);

    /**
     * Returns true if the input string could be read (valid form).
     * Otherwise returns false.
     *
     * In case where the input doesn't constitute a valid form,
     * the input is stored and any following with the clearPreviousInput flag
     * set to false, build up the stored input until it constitues a valid form.
     *
     * If the result is true, you can then call the evalLastForm
     */
    bool handleInput(std::string str, bool clearPreviousInput = false);

    /**
     * Should be called after handleInput returns true
     */
    std::string evalLastForm();
};

namespace repl {
extern const std::regex NS_REGEXP;
//
// aod.c.repl bindings
// *eval-hook* etc
extern void bind(s7_scheme* sc);
}

} // s7
} // aod
