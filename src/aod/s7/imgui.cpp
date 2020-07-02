#include "imgui.h"
#include "s7.h"
#include "aod/s7/c_primitives.hpp"

namespace aod {
namespace s7 {
namespace imgui {

namespace { // anonymous namespace: the functions

s7_pointer begin(s7_scheme *sc, s7_pointer args) {
    s7_pointer title = s7_car(args);
    if (!s7_is_string(title))
        return (s7_wrong_type_arg_error(sc, "imgui/begin", 1, title,
                "First argument is title, should be a string"));

    const char *str = s7_string(title);
    s7_pointer bool_open = s7_cadr(args);
    if (s7_is_c_object(bool_open)) {
        bool *p_open = (bool*) s7_c_object_value(bool_open);
        ImGui::Begin(str, p_open);
    } else {
        ImGui::Begin(str);
    }

    return (s7_nil(sc));
}

s7_pointer checkbox(s7_scheme *sc, s7_pointer args) {
    s7_pointer text = s7_car(args);
    if (!s7_is_string(text)) {
        return (s7_wrong_type_arg_error(sc, "imgui/checkbox", 1, text,
                "First argument is title, should be a string"));
    }

    s7_pointer bool_open = s7_cadr(args);
    if (!s7_is_c_object(bool_open)) {
        return (s7_wrong_type_arg_error(sc, "imgui/checkbox", 2, bool_open,
                "Second argument should be a c-object pointing to a c bool*"));
    }

    bool *p_check = (bool*) s7_c_object_value(bool_open);
    ImGui::Checkbox(s7_string(text), p_check);
}

s7_pointer end(s7_scheme *sc, s7_pointer args) {
    ImGui::End();
    return (s7_nil(sc));
}

s7_pointer text(s7_scheme *sc, s7_pointer args) {
    s7_pointer text = s7_car(args);
    if (!s7_is_string(text))
        return (s7_wrong_type_arg_error(sc, "aod.s7/text", 1, text,
                "text should get a string argument"));

    ImGui::Text("%s", s7_string(text));
    return (s7_nil(sc));
}

s7_pointer button(s7_scheme *sc, s7_pointer args) {
    s7_pointer text = s7_car(args);
    if (!s7_is_string(text))
        return (s7_wrong_type_arg_error(sc, "aod.s7/button", 1, text,
                "button should get a string argument"));

    bool clicked = ImGui::Button(s7_string(text));
    return (s7_make_boolean(sc, clicked));
}

s7_pointer color_edit_3(s7_scheme *sc, s7_pointer args) {
    s7_pointer text = s7_car(args);
    if (!s7_is_string(text)) {
        return (s7_wrong_type_arg_error(sc, "imgui/color-edit-3", 1, text,
                "Expecting a string (title)"));
    }

    s7_pointer obj = s7_cadr(args);
    if (!s7_is_c_object(obj)) {
        return (s7_wrong_type_arg_error(sc, "imgui/color-edit-3", 2, obj,
                "Expecting a c-object pointing to a c float* array"));
    }

    float_arr *arr = (float_arr*) s7_c_object_value(obj);
    ImGui::ColorEdit3(s7_string(text), arr->elements);

}

} // ! anonymous namespace: the functions

namespace menus {

s7_pointer begin_main_menu_bar(s7_scheme *sc, s7_pointer args) {
    return s7_make_boolean(sc, ImGui::BeginMainMenuBar());
}

s7_pointer end_main_menu_bar(s7_scheme *sc, s7_pointer args) {
    ImGui::EndMainMenuBar();

    return s7_nil(sc);
}

s7_pointer begin_menu(s7_scheme *sc, s7_pointer args) {
    s7_pointer text = s7_car(args);
    if (!s7_is_string(text))
        return (s7_wrong_type_arg_error(sc, "aod.s7/button", 1, text,
                "button should get a string argument"));

    return s7_make_boolean(sc, ImGui::BeginMenu(s7_string(text)));
}

s7_pointer end_menu(s7_scheme *sc, s7_pointer args) {
    ImGui::EndMenu();

    return s7_nil(sc);
}

// separator is general not for menus. but for now stays here :)
s7_pointer separator(s7_scheme *sc, s7_pointer args) {
    ImGui::Separator();

    return s7_nil(sc);
}

s7_pointer menu_item(s7_scheme *sc, s7_pointer args) {
    s7_pointer text = s7_car(args);
    if (!s7_is_string(text))
        return (s7_wrong_type_arg_error(sc, "aod.s7/button", 1, text,
                "button should get a string argument"));

    return s7_make_boolean(sc, ImGui::MenuItem((s7_string(text))));
}

void bind(s7_scheme *sc) {
    s7_define_function(sc, "imgui/begin-main-menu-bar", begin_main_menu_bar, // ..
            0, // req args
            0, // optional args
            false, // rest args
            "ImGui::BeginMainMenuBar");

    s7_define_function(sc, "imgui/end-main-menu-bar", end_main_menu_bar, // ..
            0, // req args
            0, // optional args
            false, // rest args
            "ImGui::EndMainMenuBar");

    s7_define_function(sc, "imgui/begin-menu", begin_menu, // ..
            1, // req args
            0, // optional args
            false, // rest args
            "Menu group (eg File)");

    s7_define_function(sc, "imgui/end-menu", end_menu, // ..
            0, // req args
            0, // optional args
            false, // rest args
            "Ends the menu group (eg File)");

    s7_define_function(sc, "imgui/separator", separator, // ..
            0, // req args
            0, // optional args
            false, // rest args
            "Separator (eg between menu items)");

    s7_define_function(sc, "imgui/menu-item", menu_item, // ..
            1, // req args
            0, // optional args
            false, // rest args
            "Menu item. TODO add more args (kbd shortcut, enabled, selected))");

}

} // ! menus

namespace layout {
s7_pointer same_line(s7_scheme *sc, s7_pointer args) {
    ImGui::SameLine();

    return s7_nil(sc);
}

void bind(s7_scheme *sc){
    s7_define_function(sc, "imgui/same-line", same_line, // ..
            0, // req args
            0, // optional args (the open boolean pointer)
            false, // rest args
            "Puts the next element in the same line as the previous one");
}
}

void bind(s7_scheme *sc) {
    using namespace menus;

    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

// TODO env
    s7_define_function(sc, "imgui/begin", begin, // ..
            1, // req args
            1, // optional args (the open boolean pointer)
            false, // rest args
            "Begin a window");

    s7_define_function(sc, "imgui/end", end, // ..
            0, // req args
            0, // optional args
            false, // rest args
            "End a window");

    s7_define_function(sc, "imgui/text", text, // ..
            1, // req args
            0, // optional args
            false, // rest args
            "Draw a text label");

    s7_define_function(sc, "imgui/button", button, // ..
            1, // req args
               // TODO apparently there are optional args, about the size?
            0, // optional args
            false, // rest args
            "Draw a button. Returns a boolean, true if clicked");

    s7_define_function(sc, "imgui/checkbox", checkbox, // ..
            2, // req args
            0, // optional args
            false, // rest args
            "Checkbox");

    s7_define_function(sc, "imgui/color-edit-3", color_edit_3, // ..
            2, // req args
            0, // optional args
            false, // rest args
            "ColorEdit3");

    menus::bind(sc);
    layout::bind(sc);

}

} // imgui
} // s7
} // aod
