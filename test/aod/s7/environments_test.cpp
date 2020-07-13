#include "gtest/gtest.h"
#include "s7.h"
#include "aod/s7.hpp"
#include <string>
#include <stdio.h>
#include <filesystem>
#include <iostream>
namespace fs = std::filesystem;

fs::path pwd = fs::current_path().remove_filename();
fs::path scheme_path = pwd / ".." / "src" / "scheme";

TEST(s7_environments, autoloads_bug) {
    GTEST_SKIP_("Waiting for s7 mailing list for this..");
    const char *autoloads[6] = {
        // each pair of entries is entity name + file name
        "aod.lib1", "aod/lib1.scm", //
        "aod.lib2", "aod/lib2.scm", //
        "aod.extra.foo", "aod/extra/foo.scm",
    };

    s7_scheme* sc1 = s7_init();
    s7_autoload_set_names(sc1, autoloads, 3);
    char* sexp = "(begin "
                 "(require aod.lib1)"
                 "(require aod.lib2)"
                 "1)";
    // ok that works
    ASSERT_EQ(1, s7_integer(s7_eval_c_string(sc1, sexp)));

    s7_scheme* sc2 = s7_init();
    s7_autoload_set_names(sc2, autoloads, 3);
    char* sexp2 = "(begin "
                  "(require aod.extra.foo)"
                  "2)";
    // THAT FAILS!!
    ASSERT_EQ(2, s7_integer(s7_eval_c_string(sc2, sexp2)));
    /**
     * ----------
    ;require: no autoload info for aod.extra.foo
    ; (require aod.extra.foo)
    ; ((lambda (hook lst) (if (do ((p lst (cdr ...
    ; (2)
    * -----------
    */
}

TEST(s7_environments, autoloads) {
    s7_scheme* sc = s7_init();
    aod::s7::bind_all(sc);
//     s7_add_to_load_path(sc, scheme_path.c_str());
    aod::s7::set_autoloads(sc);
    const char* sexp = "(begin "
                       "(require aod.core)"
//                        "(require aod.imgui)"
//                        "(require imgui-macros.scm)"
//                        "(require aod.libs.lib1)"
                       "2)";
    ASSERT_EQ(2, s7_integer(s7_eval_c_string(sc, sexp)));
}

TEST(s7_environments, require) {
    s7_scheme* sc = s7_init();
    aod::s7::bind_all(sc);
    s7_add_to_load_path(sc, scheme_path.c_str());
    aod::s7::set_autoloads(sc);

    const char* sexp = "(begin "
                       "(require aod.core)"
                       "(aod/require aod.foreign)"
                       "(define i2 (aod.foreign/new-int 2))"
                       "(i2)"
                       ")"
                       ;
    ASSERT_EQ(2, s7_integer(s7_eval_c_string(sc, sexp)));
}

TEST(s7_environments, require_as) {
    s7_scheme* sc = s7_init();
    aod::s7::bind_all(sc);
    s7_add_to_load_path(sc, scheme_path.c_str());
    aod::s7::set_autoloads(sc);

    const char* sexp1 = "(begin "
                        "(require aod.core)"
                        "(define i1 ((*foreign* 'new-int) 1))"
                        "(i1)"
                        ")"
                        ;
    ASSERT_EQ(1, s7_integer(s7_eval_c_string(sc, sexp1)));


    const char* sexp2 = "(begin "
                        "(require aod.core)"
                        "(comment aha)"
                        "(aod/require aod.foreign :as c)"
                        "(define i2 (c/new-int 2))"
                        "(i2)"
                        ")"
                        ;
    ASSERT_EQ(2, s7_integer(s7_eval_c_string(sc, sexp2)));

    const char* sexp3 = "(begin "
                        "(require aod.core)"
                        "(comment YUP cause aod.clj is already normally required from aod.core)"
                        "(aod/require aod.clj)"
                        "(aod.clj/comment AHA clj style require with aod/require)"
                        "(aod/require aod.clj :as my-clj-things)"
                        "(my-clj-things/comment AHA 2! clj style require with aod/require)"
                        "3)"
                        ;
    ASSERT_EQ(3, s7_integer(s7_eval_c_string(sc, sexp3)));

}

