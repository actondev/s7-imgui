#include "imgui.h"
#include "s7.h"
#include "aod/s7/foreign_primitives_arr.hpp"
#include "aod/s7/foreign_primitives.hpp"
#include "imgui_internal.h"
#ifdef _WIN32
// fixes gl.h errors
#include <windows.h>
#endif
#include <GL/gl.h>

namespace aod {
namespace s7 {
namespace imgui {

namespace windows {
s7_pointer begin(s7_scheme *sc, s7_pointer args) {
    s7_pointer title = s7_car(args);
    if (!s7_is_string(title))
        return (s7_wrong_type_arg_error(sc, "imgui/begin", 1, title,
                                        "First argument is title, should be a string"));

    const char *str = s7_string(title);
    s7_pointer obj = s7_cadr(args);
    bool *p_open = (bool*) s7_c_object_value_checked(obj,
                   aod::s7::foreign::tag_bool(sc));
    if (p_open == NULL) {
        // we don't throw an error. begin has multiple arity
        ImGui::Begin(str);
    } else {
        ImGui::Begin(str, p_open);
    }

    return (s7_nil(sc));
}

s7_pointer begin_maximized(s7_scheme *sc, s7_pointer args) {
    s7_pointer title = s7_car(args);
    if (!s7_is_string(title))
        return (s7_wrong_type_arg_error(sc, "imgui/begin-maximized", 1, title,
                                        "First argument is title, should be a string~A"));

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);

    // int x = viewport[0];
    // int y = viewport[1];
    int w = viewport[2];
    int h = viewport[3];

    ImGui::SetNextWindowPos(ImVec2(0.0f, 0.0f));
    ImGui::SetNextWindowSize(ImVec2((float) w, (float) h));

    ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);
    ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);

    bool show = true;
    // TODO pass flags
    ImGui::Begin("maximized-window", &show,
//                  ImGuiWindowFlags_MenuBar
                 ImGuiWindowFlags_NoTitleBar
                 | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove
                 | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoBringToFrontOnFocus
                 | ImGuiWindowFlags_NoNavFocus);
    ImGui::PopStyleVar(2);

    return s7_nil(sc);
}

s7_pointer end(s7_scheme *sc, s7_pointer args) {
    ImGui::End();
    return (s7_nil(sc));
}

void bind(s7_scheme *sc, s7_pointer env) {

    s7_define_function(sc, "imgui/begin-maximized", begin_maximized,   // ..
                       1, // req args
                       0,
                       false, // rest args
                       "Begin a/the maximized window");

    s7_define_function(sc, "imgui/begin", begin,   // ..
                       1, // req args
                       1, // optional args (the open boolean pointer)
                       false, // rest args
                       "Begin a window");
    s7_define_function(sc, "imgui/end", end,   // ..
                       0, // req args
                       0, // optional args
                       false, // rest args
                       "End a window");

    // WIP going to environments
    s7_define(sc, env, s7_make_symbol(sc, "begin"),
              s7_make_function(sc, "begin", begin, 1, 1, false,
                               "Begin a window"));

    s7_define(sc, env, s7_make_symbol(sc, "begin-maximized"),
              s7_make_function(sc, "begin-maximized", begin_maximized, 1, 0, false,
                               "Begin the maximized window"));
    s7_define(sc, env, s7_make_symbol(sc, "end"),
              s7_make_function(sc, "end", end, 0, 0, false,
                               "Ends a window"));

}
}

namespace general { // anonymous namespace: the functions

s7_pointer checkbox(s7_scheme *sc, s7_pointer args) {
    s7_pointer text = s7_car(args);
    if (!s7_is_string(text)) {
        return (s7_wrong_type_arg_error(sc, "imgui/checkbox", 1, text,
                                        "First argument is title, should be a string"));
    }

    s7_pointer obj = s7_cadr(args);
    bool *p_check = (bool*) s7_c_object_value_checked(obj,
                    aod::s7::foreign::tag_bool(sc));
    if (p_check == NULL) {
        return (s7_wrong_type_arg_error(sc, "imgui/checkbox", 2, obj,
                                        "expecting bool*"));
    }

    ImGui::Checkbox(s7_string(text), p_check);
    return s7_nil(sc);
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



void bind(s7_scheme *sc, s7_pointer env) {
    s7_define_function(sc, "imgui/text", text,   // ..
                       1, // req args
                       0, // optional args
                       false, // rest args
                       "Draw a text label");

    s7_define(sc, env, s7_make_symbol(sc, "text"),
              s7_make_function(sc, "text", text, 1, 0, false,
                               "Text"));

    s7_define_function(sc, "imgui/button", button,   // ..
                       1, // req args
                       // TODO apparently there are optional args, about the size?
                       0, // optional args
                       false, // rest args
                       "Draw a button. Returns a boolean, true if clicked");
    s7_define(sc, env, s7_make_symbol(sc, "button"),
              s7_make_function(sc, "button", button, 1, 0, false,
                               "Button"));

    s7_define_function(sc, "imgui/checkbox", checkbox,   // ..
                       2, // req args
                       0, // optional args
                       false, // rest args
                       "Checkbox");

    s7_define(sc, env, s7_make_symbol(sc, "checkbox"),
              s7_make_function(sc, "checkbox", checkbox, 2, 0, false,
                               "Checkbox"));

}
} // ! anonymous namespace: the functions

namespace menus {

s7_pointer begin_menu_bar(s7_scheme *sc, s7_pointer args) {
    return s7_make_boolean(sc, ImGui::BeginMenuBar());
}

s7_pointer end_menu_bar(s7_scheme *sc, s7_pointer args) {
    ImGui::EndMenuBar();

    return s7_nil(sc);
}

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

void bind(s7_scheme *sc, s7_pointer env) {
    s7_define_function(sc, "imgui/begin-menu-bar", begin_menu_bar,   // ..
                       0, // req args
                       0, // optional args
                       false, // rest args
                       "ImGui::BeginMenuBar");

    s7_define(sc, env, s7_make_symbol(sc, "begin-menu-bar"),
              s7_make_function(sc, "begin-menu-bar", begin_menu_bar, 0, 0, false,
                               "BeginMenuBar"));

    s7_define_function(sc, "imgui/end-menu-bar", end_menu_bar,   // ..
                       0, // req args
                       0, // optional args
                       false, // rest args
                       "ImGui::EndMenuBar");


    s7_define(sc, env, s7_make_symbol(sc, "end-menu-bar"),
              s7_make_function(sc, "end-menu-bar", end_menu_bar, 0, 0, false,
                               "EndMenuBar"));

    s7_define_function(sc, "imgui/begin-main-menu-bar", begin_main_menu_bar,   // ..
                       0, // req args
                       0, // optional args
                       false, // rest args
                       "ImGui::BeginMainMenuBar");

    s7_define(sc, env, s7_make_symbol(sc, "begin-main-menu-bar"),
              s7_make_function(sc, "begin-main-menu-bar", begin_main_menu_bar, 0, 0, false,
                               "BeginMainMenuBar"));

    s7_define_function(sc, "imgui/end-main-menu-bar", end_main_menu_bar,   // ..
                       0, // req args
                       0, // optional args
                       false, // rest args
                       "ImGui::EndMainMenuBar");

    s7_define(sc, env, s7_make_symbol(sc, "end-main-menu-bar"),
              s7_make_function(sc, "end-main-menu-bar", end_main_menu_bar, 0, 0, false,
                               "EndMainMenuBar"));

    s7_define_function(sc, "imgui/begin-menu", begin_menu,   // ..
                       1, // req args
                       0, // optional args
                       false, // rest args
                       "Menu group (eg File)");

    s7_define(sc, env, s7_make_symbol(sc, "begin-menu"),
              s7_make_function(sc, "begin-menu", begin_menu, 1, 0, false,
                               "BeginMenu"));

    s7_define_function(sc, "imgui/end-menu", end_menu,   // ..
                       0, // req args
                       0, // optional args
                       false, // rest args
                       "Ends the menu group (eg File)");

    s7_define(sc, env, s7_make_symbol(sc, "end-menu"),
              s7_make_function(sc, "end-menu", end_menu, 0, 0, false,
                               "EndMenu"));

    s7_define_function(sc, "imgui/separator", separator,   // ..
                       0, // req args
                       0, // optional args
                       false, // rest args
                       "Separator (eg between menu items)");

    s7_define(sc, env, s7_make_symbol(sc, "separator"),
              s7_make_function(sc, "separator", separator, 0, 0, false,
                               "Separator"));

    s7_define_function(sc, "imgui/menu-item", menu_item,   // ..
                       1, // req args
                       0, // optional args
                       false, // rest args
                       "Menu item. TODO add more args (kbd shortcut, enabled, selected))");

    s7_define(sc, env, s7_make_symbol(sc, "menu-item"),
              s7_make_function(sc, "menu-item", menu_item, 1, 0, false,
                               "Menu item. TODO add more args (kbd shortcut, enabled, selected"));
}

} // ! menus

namespace layout {
s7_pointer same_line(s7_scheme *sc, s7_pointer args) {
    ImGui::SameLine();

    return s7_nil(sc);
}

//s7_pointer collumns(s7_scheme *sc, s7_pointer args) {
//    ImGui::Columns(1);
//
//    return s7_nil(sc);
//}

s7_pointer begin_child(s7_scheme *sc, s7_pointer args) {
    s7_pointer title = s7_car(args);
    if (!s7_is_string(title))
        return (s7_wrong_type_arg_error(sc, "imgui/begin-child", 1, title,
                                        "First argument is title, should be a string"));
// only automatic width for now
    ImGui::BeginChild(s7_string(title), ImVec2(0, 0));

    return s7_nil(sc);
}

s7_pointer end_child(s7_scheme *sc, s7_pointer args) {
    ImGui::EndChild();

    return s7_nil(sc);
}

s7_pointer begin_group(s7_scheme *sc, s7_pointer args) {
    ImGui::BeginGroup();

    return s7_nil(sc);
}

s7_pointer end_group(s7_scheme *sc, s7_pointer args) {
    ImGui::EndGroup();

    return s7_nil(sc);
}

s7_pointer dummy(s7_scheme *sc, s7_pointer args) {
    float w = s7_number_to_real(sc, s7_car(args));
    float h = s7_number_to_real(sc, s7_cadr(args));

    ImGuiStyle &style = ImGui::GetStyle();
    ImGui::Dummy(
        ImVec2(w + style.ItemInnerSpacing.x, h + style.ItemInnerSpacing.y));

    return s7_nil(sc);
}

void bind(s7_scheme *sc, s7_pointer env) {
    s7_define_function(sc, "imgui/same-line", same_line,   // ..
                       0, // req args
                       0, // optional args (the open boolean pointer)
                       false, // rest args
                       "Puts the next element in the same line as the previous one");

    s7_define(sc, env, s7_make_symbol(sc, "same-line"),
              s7_make_function(sc, "same-line", same_line, 0, 0, false,
                               "Puts the next element in the same line as the previous one"));


    s7_define_function(sc, "imgui/begin-child", begin_child,   // ..
                       1, // req args
                       0, // optional args (the open boolean pointer)
                       false, // rest args
                       "BeginChild");

    s7_define(sc, env, s7_make_symbol(sc, "begin-child"),
              s7_make_function(sc, "begin-child", begin_child, 1, 0, false,
                               "BeginChild"));

    s7_define_function(sc, "imgui/end-child", end_child,   // ..
                       0, // req args
                       0, // optional args (the open boolean pointer)
                       false, // rest args
                       "EndChild");

    s7_define(sc, env, s7_make_symbol(sc, "end-child"),
              s7_make_function(sc, "end-child", end_child, 0, 0, false,
                               "EndChild"));

    s7_define_function(sc, "imgui/begin-group", begin_group, 0, 0, false,
                       "BeginGroup");
    s7_define(sc, env, s7_make_symbol(sc, "begin-group"),
              s7_make_function(sc, "begin-group", begin_group, 0, 0, false,
                               "BeginGroup"));

    s7_define_function(sc, "imgui/end-group", end_group, 0, 0, false,
                       "EndGroup");
    s7_define(sc, env, s7_make_symbol(sc, "end-group"),
              s7_make_function(sc, "end-group", end_group, 0, 0, false,
                               "EndGroup"));

    s7_define_function(sc, "imgui/dummy", dummy, 2, 0, false,
                       "Dummy - a container (for eg drawing - think of it as a canvas)");

    s7_define(sc, env, s7_make_symbol(sc, "dummy"),
              s7_make_function(sc, "dummy", dummy, 2, 0, false,
                               "Dummy - a container (a placeholder for custom drawin, sets the w,h to offset the next element)"));
}
}

namespace draw {

s7_pointer circle(s7_scheme *sc, s7_pointer args) {
    ImVec2 p = ImGui::GetCursorScreenPos();

    ImDrawList *draw_list = ImGui::GetWindowDrawList();

    float cx = s7_number_to_real(sc, s7_list_ref(sc, args, 0));
    float cy = s7_number_to_real(sc, s7_list_ref(sc, args, 1));
    float r = s7_number_to_real(sc, s7_list_ref(sc, args, 2));
    unsigned int col = (unsigned int) s7_number_to_real(sc, s7_list_ref(sc, args, 3));
    float thickness = 1;
    int segments = 32;
    s7_pointer sc_segments = s7_list_ref(sc, args, 4);
    s7_pointer sc_thickness = s7_list_ref(sc, args, 5);
    if (s7_is_number(sc_segments)) {
        segments = s7_number_to_integer(sc, sc_segments);

        if (s7_is_number(sc_thickness)) {
            thickness = s7_number_to_real(sc, sc_thickness);
        }
    }

    draw_list->AddCircle(ImVec2(p.x + cx, p.y + cy), r, col, segments,
                         thickness);
    return s7_nil(sc);
}

s7_pointer circle_filled(s7_scheme *sc, s7_pointer args) {
    ImVec2 p = ImGui::GetCursorScreenPos();

    ImDrawList *draw_list = ImGui::GetWindowDrawList();

    float cx = s7_number_to_real(sc, s7_list_ref(sc, args, 0));
    float cy = s7_number_to_real(sc, s7_list_ref(sc, args, 1));
    float r = s7_number_to_real(sc, s7_list_ref(sc, args, 2));
    unsigned int col = (unsigned int) s7_number_to_real(sc, s7_list_ref(sc, args, 3));
    int segments = 0;
    s7_pointer sc_segments = s7_list_ref(sc, args, 4);
    if (s7_is_number(sc_segments)) {
        segments = s7_number_to_integer(sc, sc_segments);
    }

    draw_list->AddCircleFilled(ImVec2(p.x + cx, p.y + cy), r, col, segments);
    return s7_nil(sc);
}

s7_pointer text(s7_scheme *sc, s7_pointer args) {
    ImVec2 p = ImGui::GetCursorScreenPos();

    ImDrawList *draw_list = ImGui::GetWindowDrawList();

    float x = s7_number_to_real(sc, s7_car(args));
    float y = s7_number_to_real(sc, s7_cadr(args));
    const char *str = s7_string(s7_caddr(args));

    // I really need to this. if not it crashes.
    char buffer[64];
    sprintf(buffer, "%s", str);

    unsigned int col = (unsigned int) s7_number_to_real(sc, s7_cadddr(args));

    // ImGuiContext& g = *GImGui;
    // ImGuiWindow* window = g.CurrentWindow;
    // float wrap_width = 100.0f;
    // float font_size = 0.0f; // auto
//    draw_list->AddText(NULL, font_size, ImVec2(p.x + x, p.y + y), col, "begin",
//            "end", wrap_width, NULL);

    draw_list->AddText(ImVec2(p.x + x, p.y + y), col, buffer);

    return s7_nil(sc);
}

s7_pointer line(s7_scheme *sc, s7_pointer args) {
    ImVec2 p = ImGui::GetCursorScreenPos();

    ImDrawList *draw_list = ImGui::GetWindowDrawList();

    float x1 = s7_number_to_real(sc, s7_car(args));
    float y1 = s7_number_to_real(sc, s7_cadr(args));
    float x2 = s7_number_to_real(sc, s7_caddr(args));
    s7_pointer arg4plus = s7_cdddr(args);
    float y2 = s7_number_to_real(sc, s7_car(arg4plus));
    unsigned int col = (unsigned int) s7_number_to_real(sc, s7_cadr(arg4plus));
    float thickness = 1.0f;
    s7_pointer sc_thickness = s7_caddr(arg4plus);
    if (s7_is_number(sc_thickness)) {
        thickness = s7_number_to_real(sc, sc_thickness);
    }

    draw_list->AddLine(ImVec2(p.x + x1, p.y + y1), ImVec2(p.x + x2, p.y + y2),
                       col, thickness);

    return s7_nil(sc);
}

s7_pointer arc(s7_scheme *sc, s7_pointer args) {
    //     IMGUI_API void  PathArcTo(const ImVec2& center, float radius, float a_min, float a_max, int num_segments = 10);
    ImVec2 p = ImGui::GetCursorScreenPos();

    ImDrawList *draw_list = ImGui::GetWindowDrawList();

    float cx = p.x + s7_number_to_real(sc, s7_list_ref(sc, args, 0));
    float cy = p.y + s7_number_to_real(sc, s7_list_ref(sc, args, 1));
    float r =  s7_number_to_real(sc, s7_list_ref(sc, args, 2));
    float a_min = s7_number_to_real(sc, s7_list_ref(sc, args, 3));
    float a_max = s7_number_to_real(sc, s7_list_ref(sc, args, 4));
    unsigned int col = (unsigned int) s7_number_to_real(sc, s7_list_ref(sc, args, 5));


    float thickness = 1;
    int segments = 32;
    s7_pointer sc_segments = s7_list_ref(sc, args, 6);
    s7_pointer sc_thickness = s7_list_ref(sc, args, 7);
    if (s7_is_number(sc_segments)) {
        segments = s7_number_to_integer(sc, sc_segments);

        if (s7_is_number(sc_thickness)) {
            thickness = s7_number_to_real(sc, sc_thickness);
        }
    }

    draw_list->PathArcTo(ImVec2(cx, cy), r, a_min, a_max, segments);
    draw_list->PathStroke(col, false, thickness);

    return s7_nil(sc);
}

void bind(s7_scheme *sc, s7_pointer env) {
    s7_define_function(sc, "imgui.draw/circle", circle,   // ..
                       4, // req args: cx cy r col
                       2, // optional args: segments, thickness
                       false, // rest args
                       "(cx cy r col &optional segments thickness)");

    s7_define(sc, env, s7_make_symbol(sc, "draw-circle"),
              s7_make_function(sc, "draw-circle", circle,
                               4, // req args: cx cy r col
                               2, // optional args: segments, thickness
                               false, // rest args
                               "(cx cy r col &optional segments thickness)"));
    s7_define(sc, env, s7_make_symbol(sc, "draw-arc"),
              s7_make_function(sc, "draw-arc", arc,
                               6, // req args: cx cy r a_min a_max col
                               2, // optional args: segments, thickness
                               false, // rest args
                               "(cx cy r a-min a-max col &optional segments thickness)"));

    s7_define(sc, env, s7_make_symbol(sc, "draw-circle-filled"),
              s7_make_function(sc, "draw-circle", circle_filled,
                               4, // req args: cx cy r col
                               1, // optional args: segments, thickness
                               false, // rest args
                               "(cx cy r col &optional segments)"));

    s7_define_function(sc, "imgui.draw/line", line,   // ..
                       5, // req args: x1 x2 y1 y2 col
                       1, // optional args: thickness
                       false, // rest args
                       "(x1 y1 x2 y2 col &optional thickness)");

    s7_define(sc, env, s7_make_symbol(sc, "draw-line"),
              s7_make_function(sc, "draw-line", line,
                               5, // req args: x1 x2 y1 y2 col
                               1, // optional args: thickness
                               false, // rest args
                               "(x1 y1 x2 y2 col &optional thickness)"));

    s7_define_function(sc, "imgui.draw/text", text,   // ..
                       4, // req args: x y text color
                       0, // optional args: thickness
                       false, // rest args
                       "(x y text color)");

    s7_define(sc, env, s7_make_symbol(sc, "draw-text"),
              s7_make_function(sc, "draw-text", text,
                               4, // req args: x y text color
                               0,
                               false, // rest args
                               "(x y text color)"));
}
}

namespace colors {

s7_pointer color32(s7_scheme *sc, s7_pointer args) {
    ImU32 r = s7_number_to_real(sc, s7_car(args));
    ImU32 g = s7_number_to_real(sc, s7_cadr(args));
    ImU32 b = s7_number_to_real(sc, s7_caddr(args));
    ImU32 a = 255;
    s7_pointer passed_alpha = s7_cadddr(args);
    if (s7_is_number(passed_alpha)) {
        a = s7_number_to_real(sc, passed_alpha);
    }
    ImU32 res = IM_COL32(r, g, b, a);

    return s7_make_integer(sc, (int) res);
}

void bind(s7_scheme *sc, s7_pointer env) {
    s7_define(sc, env, s7_make_symbol(sc, "color32"),
              s7_make_function(sc, "color32", color32,
                               3, // req args
                               1, // optional args: alpha
                               false, // rest args
                               "(color32 r g b &optional a) input ranging from 0 to 255"
                               "Returns a u32 representation of the color 0xRRGGBBAA"));
}
}

namespace sliders {
s7_pointer slider_float(s7_scheme *sc, s7_pointer args) {
    s7_pointer text = s7_car(args);
    if (!s7_is_string(text)) {
        return (s7_wrong_type_arg_error(sc, "imgui/color-edit-3", 1, text,
                                        "Expecting a string (title)"));
    }

    float *p_value = (float*) s7_c_object_value_checked(s7_cadr(args),
                     aod::s7::foreign::tag_float(sc));

    float min = (float) s7_real(s7_caddr(args));
    float max = (float) s7_real(s7_cadddr(args));

    return s7_make_boolean(sc, ImGui::SliderFloat(s7_string(text), p_value, min, max));
}

s7_pointer color_edit_3(s7_scheme *sc, s7_pointer args) {
    s7_pointer text = s7_car(args);
    if (!s7_is_string(text)) {
        return (s7_wrong_type_arg_error(sc, "imgui/color-edit-3", 1, text,
                                        "Expecting a string (title)"));
    }

    s7_pointer obj = s7_cadr(args);
    float *arr = (float*) s7_c_object_value_checked(obj,
                 aod::s7::foreign::tag_float_arr(sc));
    if (arr == NULL) {
        return (s7_wrong_type_arg_error(sc, "imgui/color-edit-3", 2, obj,
                                        "float* array"));
    }

    ImGui::ColorEdit3(s7_string(text), arr);
    return s7_nil(sc);
}

void bind(s7_scheme* sc, s7_pointer env) {
    s7_define_function(sc, "imgui/color-edit-3", color_edit_3,   // ..
                       2, // req args
                       0, // optional args
                       false, // rest args
                       "ColorEdit3");
    s7_define(sc, env, s7_make_symbol(sc, "color-edit-3"),
              s7_make_function(sc, "color-edit-3", color_edit_3,
                               2, // req args
                               0, // optional args: thickness
                               false, // rest args
                               "ColorEdit3"));


    s7_define_function(sc, "imgui/slider-float", slider_float,   // ..
                       4, // req args
                       0, // optional args
                       false, // rest args
                       "SliderFloat");
    s7_define(sc, env, s7_make_symbol(sc, "slider-float"),
              s7_make_function(sc, "slider-float", slider_float,
                               4, // req args
                               0, // optional args: thickness
                               false, // rest args
                               "SliderFloat"));

}
}

namespace inputs {
s7_pointer text_input(s7_scheme* sc, s7_pointer args) {
    /*
     static char str0[128] = "Hello, world!";
     ImGui::InputText("input text", str0, IM_ARRAYSIZE(str0));
     */
    s7_pointer sc_label = s7_car(args);


    if (!s7_is_string(sc_label)) {
        return (s7_wrong_type_arg_error(sc, "text", 1, sc_label,
                                        "expecting string"));
    }

    char* str = (char*) s7_c_object_value_checked(s7_cadr(args),
                aod::s7::foreign::tag_char_arr(sc));
    if (str == NULL) {
        return (s7_wrong_type_arg_error(sc, "text", 2, s7_cadr(args),
                                        "expecting char* from aod.c.foreign/new-char[]"));
    }

    s7_pointer sc_size = s7_caddr(args);
    if (!s7_is_number(sc_size)) {
        return (s7_wrong_type_arg_error(sc, "text", 3, sc_size,
                                        "expecting number for buffer size"));
    }

    // shit.. I have to know the buffer size
    return s7_make_boolean(sc, ImGui::InputText("input text", str, s7_integer(sc_size)));
}


void bind(s7_scheme *sc, s7_pointer env) {
    s7_define(sc, env, s7_make_symbol(sc, "text-input"),
              s7_make_function(sc, "text-input", text_input, 3, // label, char* point
                               0, false,
                               "(label *char buffer-size)"));
}

}
// exposed function (inc in header)
void bind(s7_scheme *sc) {
//     using namespace menus;

    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    windows::bind(sc, env);
    general::bind(sc, env);
    menus::bind(sc, env);
    layout::bind(sc, env);
    draw::bind(sc, env);
    colors::bind(sc, env);
    sliders::bind(sc, env);
    inputs::bind(sc, env);

    // the provide is needed to define the *features* symbol in this environment
    // this is checked to avoid duplicate requires of this environment
    s7_eval_c_string_with_environment(sc, "(provide 'aod.c.imgui)", env);
    s7_define_variable(sc, "aod.c.imgui", env);
}

} // imgui
} // s7
} // aod

