#include "./wm.hpp"
#include "aod/wm/wm.hpp"
#include <list>
#include <string>

namespace aod {
namespace s7 {
namespace wm {

s7_pointer list_windows(s7_scheme* sc, s7_pointer args) {
    s7_pointer list = s7_cons(sc, s7_nil(sc), s7_nil(sc));
    s7_pointer list_run = list;
    std::list<aod::wm::t_window> windows = aod::wm::list_windows();
    int count = 0;
    for (const auto& window : windows) {
        if (count++ > 0) {
            // appending
            s7_set_cdr(list_run, s7_cons(sc, s7_nil(sc), s7_nil(sc)));
            list_run = s7_cdr(list_run);
        }

        s7_set_car(list_run,
                   s7_inlet(sc,
                            s7_list(sc, 4,
                                    s7_make_symbol(sc, "title"),
                                    s7_make_string(sc, window.title.c_str()),
                                    s7_make_symbol(sc, "window"),
                                    s7_make_integer(sc, window.window)
                                   )));
    }

    return list;
}

enum window_op {focus, raise};

void run_window_op(window_op op, unsigned long window) {
    switch (op) {
    case focus:
        aod::wm::focus_window(window);
        return;
    case raise:
        aod::wm::raise_window(window);
    }
}

s7_pointer focus_window(s7_scheme* sc, s7_pointer args) {
    s7_pointer window_pointer = s7_car(args);

    if (!s7_is_integer(window_pointer)) {
        s7_error(sc, s7_make_symbol(sc, "was waiting a number (window id)"), s7_nil(sc));
    }
    int window = s7_integer(window_pointer);
    aod::wm::focus_window(window);

    return s7_t(sc);
}

s7_pointer raise_window(s7_scheme* sc, s7_pointer args) {
    s7_pointer window_pointer = s7_car(args);

    if (!s7_is_integer(window_pointer)) {
        s7_error(sc, s7_make_symbol(sc, "was waiting a number (window id)"), s7_nil(sc));
    }
    int window = s7_integer(window_pointer);
    aod::wm::raise_window(window);

    return s7_t(sc);
}


s7_pointer bind(s7_scheme* sc, s7_pointer args) {

    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "list-windows"),
              s7_make_function(sc, "list-windows", list_windows,
                               0, // req args
                               0, // optional args: thickness
                               false, // rest args
                               "Lists windows"));

    s7_define(sc, env, s7_make_symbol(sc, "focus-window"),
              s7_make_function(sc, "focus-window", focus_window,
                               1, // req args
                               0, // optional args: thickness
                               false, // rest args
                               ""));

    s7_define(sc, env, s7_make_symbol(sc, "raise-window"),
              s7_make_function(sc, "raise-window", raise_window,
                               1, // req args
                               0, // optional args: thickness
                               false, // rest args
                               ""));

    s7_define_variable(sc, "aod.c.wm", env);

    return env;
}

} // wm
} // s7
} // aod



