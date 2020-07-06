#include "imgui.h"
#include "s7.h"
#include "aod/s7/foreign_primitives_arr.hpp"
#include "aod/s7/foreign_primitives.hpp"
#include <GL/gl.h>

namespace aod {
namespace s7 {
namespace imgui {

namespace windows {
s7_pointer begin_maximized(s7_scheme *sc, s7_pointer args) {
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);

    int x = viewport[0];
    int y = viewport[1];
    int w = viewport[2];
    int h = viewport[3];

    ImGui::SetNextWindowPos(ImVec2(0.0f, 0.0f));
    ImGui::SetNextWindowSize(ImVec2((float) w, (float) h));

//    ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 0.0f);
//    ImGui::PushStyleVar(ImGuiStyleVar_WindowBorderSize, 0.0f);

    bool show = true;
    ImGui::Begin("maximized-window", &show,
            ImGuiWindowFlags_MenuBar | ImGuiWindowFlags_NoTitleBar
                    | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove
                    | ImGuiWindowFlags_NoCollapse);

//    ImGuiWindowFlags window_flags = ImGuiWindowFlags_MenuBar;
//    window_flags |= ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoResize
//            | ImGuiWindowFlags_NoMove;
//    window_flags |= ImGuiWindowFlags_NoBringToFrontOnFocus
//            | ImGuiWindowFlags_NoNavFocus;

//    ImGui::PopStyleVar(2);

    return s7_nil(sc);
}

void bind(s7_scheme *sc) {

    s7_define_function(sc, "imgui/begin-maximized", begin_maximized, // ..
            0, // req args
            0, // optional args (the open boolean pointer)
            false, // rest args
            "Begin a/the maximized window");

}
}

namespace general { // anonymous namespace: the functions

s7_pointer begin(s7_scheme *sc, s7_pointer args) {
    s7_pointer title = s7_car(args);
    if (!s7_is_string(title))
        return (s7_wrong_type_arg_error(sc, "imgui/begin", 1, title,
                "First argument is title, should be a string"));

    const char *str = s7_string(title);
    s7_pointer obj = s7_cadr(args);
    bool *p_open = (bool*) s7_c_object_value_checked(obj,
            s7::foreign::tag_bool(sc));
    if (p_open == NULL) {
        // we don't throw an error. begin has multiple arity
        ImGui::Begin(str);
    } else {
        ImGui::Begin(str, p_open);
    }

    return (s7_nil(sc));
}

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
    float *arr = (float*) s7_c_object_value_checked(obj,
            aod::s7::foreign::tag_float_arr(sc));
    if (arr == NULL) {
        return (s7_wrong_type_arg_error(sc, "imgui/color-edit-3", 2, obj,
                "float* array"));
    }

    ImGui::ColorEdit3(s7_string(text), arr);
    return s7_nil(sc);
}

void bind(s7_scheme *sc) {
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

void bind(s7_scheme *sc) {
    s7_define_function(sc, "imgui/begin-menu-bar", begin_menu_bar, // ..
            0, // req args
            0, // optional args
            false, // rest args
            "ImGui::BeginMenuBar");

    s7_define_function(sc, "imgui/end-menu-bar", end_menu_bar, // ..
            0, // req args
            0, // optional args
            false, // rest args
            "ImGui::EndMenuBar");

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

void bind(s7_scheme *sc) {
    s7_define_function(sc, "imgui/same-line", same_line, // ..
            0, // req args
            0, // optional args (the open boolean pointer)
            false, // rest args
            "Puts the next element in the same line as the previous one");

    s7_define_function(sc, "imgui/begin-child", begin_child, // ..
            1, // req args
            0, // optional args (the open boolean pointer)
            false, // rest args
            "BeginChild");

    s7_define_function(sc, "imgui/end-child", end_child, // ..
            0, // req args
            0, // optional args (the open boolean pointer)
            false, // rest args
            "EndChild");

    s7_define_function(sc, "imgui/begin-group", begin_group, 0, 0, false,
            "BeginGroup");
    s7_define_function(sc, "imgui/end-group", end_group, 0, 0, false,
            "EndGroup");

    s7_define_function(sc, "imgui/dummy", dummy, 2, 0, false,
            "Dummy - a container (for eg drawing - think of it as a canvas)");
}
}

namespace draw {

s7_pointer circle(s7_scheme *sc, s7_pointer args) {
    ImVec2 p = ImGui::GetCursorScreenPos();

    ImDrawList *draw_list = ImGui::GetWindowDrawList();

    float cx = s7_number_to_real(sc, s7_car(args));
    float cy = s7_number_to_real(sc, s7_cadr(args));
    float r = s7_number_to_real(sc, s7_caddr(args));
    unsigned int col = (unsigned int) s7_number_to_real(sc, s7_cadddr(args));
    float thickness = 1;
    int segments = 32;
    s7_pointer sc_segments = s7_car(s7_cddddr(args));
    s7_pointer sc_thickness = s7_cadr(s7_cddddr(args));
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

void bind(s7_scheme *sc) {
    s7_define_function(sc, "imgui.draw/circle", circle, // ..
            4, // req args: cx cy r col
            2, // optional args: segments, thickness
            false, // rest args
            "(cx cy r col &optional segments thickness)");

    s7_define_function(sc, "imgui.draw/line", line, // ..
            5, // req args: x1 x2 y1 y2 col
            1, // optional args: thickness
            false, // rest args
            "(x1 y1 x2 y2 col &optional thickness)");
}
}

namespace colors {

s7_pointer float_rgb_to_u32(s7_scheme *sc, s7_pointer args) {
    ImU32 r = (ImU32) (255 * s7_number_to_real(sc, s7_car(args)));
    ImU32 g = (ImU32) (255 * s7_number_to_real(sc, s7_cadr(args)));
    ImU32 b = (ImU32) (255 * s7_number_to_real(sc, s7_caddr(args)));
    ImU32 res = IM_COL32(r, g, b, 255);

    return s7_make_integer(sc, (int) res);
}

s7_pointer int_rgb_to_u32(s7_scheme *sc, s7_pointer args) {
    int r = s7_number_to_integer(sc, s7_car(args));
    int g = s7_number_to_integer(sc, s7_car(args));
    int b = s7_number_to_integer(sc, s7_car(args));
    unsigned int res = IM_COL32(r, g, b, 255);

    return s7_make_integer(sc, res);
}

void bind(s7_scheme *sc) {
    s7_define_function(sc, "imgui.color/frgb->u32",
            float_rgb_to_u32, // ..
            3, // req args
            0, // optional args (the open boolean pointer)
            false, // rest args
            "Returns a u32 representation of the color 0xRRGGBBAA. Inputs are from 0.0 to 1.0");

    s7_define_function(sc, "imgui.color/rgb->u32",
            int_rgb_to_u32, // ..
            3, // req args
            0, // optional args (the open boolean pointer)
            false, // rest args
            "Returns a u32 representation of the color 0xRRGGBBAA. Inputs are from 0 to 255.");
}
}

// exposed function (inc in header)
void bind(s7_scheme *sc) {
    using namespace menus;

    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    general::bind(sc);
    menus::bind(sc);
    layout::bind(sc);
    draw::bind(sc);
    colors::bind(sc);
    windows::bind(sc);

}

} // imgui
} // s7
} // aod
