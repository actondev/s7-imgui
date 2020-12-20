// #include <X11/X.h>

#include <list>
#include <string>
#pragma once
namespace aod {
namespace wm {

struct t_window {
    std::string title;
    // Window is just unsigned long
    // TODO windows?
    // Window is just unsigned long
    unsigned long window;
};

// int list_windows_old();
std::list<t_window> list_windows();

// sets the input focus (keyboard) to the window
void focus_window(unsigned long window);
// raises the window (making it visible)
void raise_window(unsigned long window);
}
}
