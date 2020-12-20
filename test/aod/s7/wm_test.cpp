#include "gtest/gtest.h"
#include "aod/s7/wm.hpp"

using std::cout;
using std::endl;
TEST(s7_wm, test) {
    s7_scheme* sc = s7_init();

    aod::s7::wm::bind(sc, s7_nil(sc));

    s7_eval_c_string(sc, "(define wm/list-windows (aod.c.wm 'list-windows))");

    s7_eval_c_string(sc, "(format #t \"~A\" (wm/list-windows))");
    
    s7_eval_c_string(sc, "(define wm/focus-window (aod.c.wm 'focus-window))");
    s7_eval_c_string(sc, "(define wm/raise-window (aod.c.wm 'raise-window))");

//     s7_eval_c_string(sc, "(wm/raise-window 127926276)");    
//     s7_eval_c_string(sc, "(wm/focus-window 127926276)");
    
//     127926276
    
    s7_free(sc);
}

/**
 ((inlet 'title "xfce4-panel" 'window 16777219) (inlet 'title "xfce4-panel" 'window 16777237) (inlet 'title "xfce4-panel" 'window 16777241) (inlet 'title "Desktop" 'window 58720275) (inlet 'title "Spotify Premium" 'window 127926276) (inlet 'title "actondev@actondev-ltp01: ~" 'window 113246215) (inlet 'title "Search results for \"kdevelop\"" 'window 136314887) (inlet 'title "how do I use an enum value on a switch statement in C++ - Stack Overflow �\x80;\x94; Mozilla Firefox" 'window 46137347) (inlet 'title "/home/actondev/dev/actondev/s7-imgui/test/aod/s7/environments_test.cpp  -- gnu/linux" 'window 100663498) (inlet 'title "s7imgui:  s7-imgui - [ s7-imgui:src/aod/s7/wm.cpp ] �\x80;\x94; KDevelop" 'window 132120579) (inlet 'title "s7-imgui - [~/dev/actondev/s7-imgui] - SmartGit 20.1.5 (for non-commercial use only) " 'window 140509226) (inlet 'title "Bluetooth Devices" 'window 111149064) ())[       OK ] s7_wm.test (7 ms)
 */
