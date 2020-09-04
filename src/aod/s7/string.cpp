#include "s7.h"
#include <regex>
#include <string>
#include <iostream>

namespace aod {
namespace s7 {
namespace string {

std::regex regex_exp;
std::smatch regex_match;
std::string regex_str;

s7_pointer search(s7_scheme* sc, s7_pointer args) {
    const char* str = s7_string(s7_car(args));
    args = s7_cdr(args);
    const char* regex_char = s7_string(s7_car(args));
    regex_exp = std::regex(regex_char);
    regex_match = std::smatch();

    regex_str = str;
    return s7_make_boolean(sc, std::regex_search(regex_str, regex_match, regex_exp));
}

s7_pointer replace(s7_scheme* sc, s7_pointer args) {
    const char* str = s7_string(s7_car(args));
    args = s7_cdr(args);
    const char* regex_char = s7_string(s7_car(args));
    args = s7_cdr(args);
    const char* regex_replace_char = s7_string(s7_car(args));

    regex_exp = std::regex(regex_char);
    regex_match = std::smatch();

    regex_str = str;

    std::string replacement = regex_replace_char;
    return s7_make_string(sc, std::regex_replace(regex_str, regex_exp, replacement).c_str());
}

s7_pointer matches_size(s7_scheme* sc, s7_pointer) {
    return s7_make_integer(sc, regex_match.size());
}

s7_pointer get_match(s7_scheme* sc, s7_pointer args) {
    int idx = s7_integer(s7_car(args));
    if (idx > regex_match.size()) {
        return   s7_error(sc,
                          s7_make_symbol(sc, "regex-match-at-error"),
                          s7_cons(sc, s7_make_string(sc, "passed idx is greater than matches"), s7_nil(sc)));
    }
//     std::cerr << "match " << idx << " prefix " << regex_match.prefix() << " is " << regex_match[idx] << std::endl;
    return s7_make_string(sc, regex_match[idx].str().c_str());
//     return s7_f(sc);
}

s7_pointer uppercase(s7_scheme* sc, s7_pointer args) {
    const char* str_char = s7_string(s7_car(args));
    std::string str = str_char;
//     toupper()
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
    return s7_make_string(sc, str.c_str());
}

s7_pointer lowercase(s7_scheme* sc, s7_pointer args) {
    const char* str_char = s7_string(s7_car(args));
    std::string str = str_char;
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    return s7_make_string(sc, str.c_str());
}

void bind(s7_scheme* sc) {

    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "search"),
              s7_make_function(sc, "search", search, 2, 0, 0, "(search str regex) Returns #t or #f")
             );

    s7_define(sc, env, s7_make_symbol(sc, "count-matches"),
              s7_make_function(sc, "count-matches", matches_size, 0, 0, 0, "(count-matches)")
             );

    s7_define(sc, env, s7_make_symbol(sc, "match-at"),
              s7_make_function(sc, "match-at", get_match, 1, 0, 0, "(match-at)")
             );

    s7_define(sc, env, s7_make_symbol(sc, "replace"),
              s7_make_function(sc, "replace", replace, 3, 0, 0, "(replace str regex replacement)")
             );

    s7_define(sc, env, s7_make_symbol(sc, "uppercase"),
              s7_make_function(sc, "uppercase", uppercase, 1, 0, 0, "(uppercase str)")
             );

    s7_define(sc, env, s7_make_symbol(sc, "lowercase"),
              s7_make_function(sc, "lowercase", lowercase, 1, 0, 0, "(lowercase str)")
             );

    s7_define_constant(sc, "aod.c.string", env);

}
}
}
}
