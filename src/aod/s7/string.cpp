#include "s7.h"
#include <regex>
#include <string>
#include <iostream>

namespace aod {
namespace s7 {
namespace string {

std::regex regex_exp;
std::smatch regex_match;
std::string search_str;

const char* help_search = "(search str regex &optional ignore-case?) Returns #t or #f";
s7_pointer search(s7_scheme* sc, s7_pointer args) {
    const char* str = s7_string(s7_car(args));
    args = s7_cdr(args);
    const char* regex_char = s7_string(s7_car(args));
    auto syntax = std::regex::ECMAScript;
    args = s7_cdr(args);
    if (args != s7_nil(sc)) {
        bool ignore_case = s7_boolean(sc, s7_car(args));
        if (ignore_case) {
            syntax |= std::regex::icase;
        }
    }
    regex_exp = std::regex(regex_char, syntax);
    regex_match = std::smatch();

    search_str = str;
    return s7_make_boolean(sc, std::regex_search(search_str, regex_match, regex_exp));
}

const char* help_search_global = "(search-global str regex cb &optional ignore-case?) Returns the list of the results";
s7_pointer search_global(s7_scheme* sc, s7_pointer args) {
    const char* str = s7_string(s7_car(args));
    std::string s(str);
    args = s7_cdr(args);
    const char* regex_char = s7_string(s7_car(args));
    auto syntax = std::regex::ECMAScript;
    args = s7_cdr(args);
    s7_pointer cb = s7_car(args);

    args = s7_cdr(args);
    if (args != s7_nil(sc)) {
        bool ignore_case = s7_boolean(sc, s7_car(args));
        if (ignore_case) {
            syntax |= std::regex::icase;
        }
    }
    std::regex r = std::regex(regex_char, syntax);
    regex_match = std::smatch();

    s7_pointer list = s7_cons(sc, s7_nil(sc), s7_nil(sc));
    s7_pointer list_run = list;
    int count = 0;


    while (std::regex_search(s, regex_match, r)) {
        if (count++ > 0) {
            // appending
            s7_set_cdr(list_run, s7_cons(sc, s7_nil(sc), s7_nil(sc)));
            list_run = s7_cdr(list_run);
        }
        s7_set_car(list_run,
                   s7_call(sc, cb, s7_nil(sc)));

        s = regex_match.suffix();
    }

    return list;
}

s7_pointer replace(s7_scheme* sc, s7_pointer args) {
    const char* str = s7_string(s7_car(args));
    args = s7_cdr(args);
    const char* regex_char = s7_string(s7_car(args));
    args = s7_cdr(args);
    const char* regex_replace_char = s7_string(s7_car(args));

    regex_exp = std::regex(regex_char);
    regex_match = std::smatch();

    search_str = str;

    std::string replacement = regex_replace_char;
    return s7_make_string(sc, std::regex_replace(search_str, regex_exp, replacement).c_str());
}

s7_pointer matches_size(s7_scheme* sc, s7_pointer) {
    return s7_make_integer(sc, regex_match.size());
}

s7_pointer get_match(s7_scheme* sc, s7_pointer args) {
    int idx = s7_integer(s7_car(args));
    if (idx >= regex_match.size()) {
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
              s7_make_function(sc, "search", search, 2, 1, 0, help_search)
             );

    s7_define(sc, env, s7_make_symbol(sc, "search-global"),
              s7_make_function(sc, "search-global", search_global, 2, 1, 0, help_search_global)
             );

    s7_define(sc, env, s7_make_symbol(sc, "match-count"),
              s7_make_function(sc, "match-count", matches_size, 0, 0, 0, "(match-count)")
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
