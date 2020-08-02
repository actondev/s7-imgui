#include "s7.h"
#include "imgui.h"
namespace aod {
namespace s7 {
namespace imgui {
namespace enums {

void bind_colors(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);

    s7_define(sc, env, s7_make_symbol(sc, "Text"),
              s7_make_integer(sc, ImGuiCol_Text));
    s7_define(sc, env, s7_make_symbol(sc, "TextDisabled"),
              s7_make_integer(sc, ImGuiCol_TextDisabled));
    s7_define(sc, env, s7_make_symbol(sc, "WindowBg"),
              s7_make_integer(sc, ImGuiCol_WindowBg));
    s7_define(sc, env, s7_make_symbol(sc, "ChildBg"),
              s7_make_integer(sc, ImGuiCol_ChildBg));
    s7_define(sc, env, s7_make_symbol(sc, "PopupBg"),
              s7_make_integer(sc, ImGuiCol_PopupBg));
    s7_define(sc, env, s7_make_symbol(sc, "Border"),
              s7_make_integer(sc, ImGuiCol_Border));
    s7_define(sc, env, s7_make_symbol(sc, "BorderShadow"),
              s7_make_integer(sc, ImGuiCol_BorderShadow));
    s7_define(sc, env, s7_make_symbol(sc, "FrameBg"),
              s7_make_integer(sc, ImGuiCol_FrameBg));
    s7_define(sc, env, s7_make_symbol(sc, "FrameBgHovered"),
              s7_make_integer(sc, ImGuiCol_FrameBgHovered));
    s7_define(sc, env, s7_make_symbol(sc, "FrameBgActive"),
              s7_make_integer(sc, ImGuiCol_FrameBgActive));
    s7_define(sc, env, s7_make_symbol(sc, "TitleBg"),
              s7_make_integer(sc, ImGuiCol_TitleBg));
    s7_define(sc, env, s7_make_symbol(sc, "TitleBgActive"),
              s7_make_integer(sc, ImGuiCol_TitleBgActive));
    s7_define(sc, env, s7_make_symbol(sc, "TitleBgCollapsed"),
              s7_make_integer(sc, ImGuiCol_TitleBgCollapsed));
    s7_define(sc, env, s7_make_symbol(sc, "MenuBarBg"),
              s7_make_integer(sc, ImGuiCol_MenuBarBg));
    s7_define(sc, env, s7_make_symbol(sc, "ScrollbarBg"),
              s7_make_integer(sc, ImGuiCol_ScrollbarBg));
    s7_define(sc, env, s7_make_symbol(sc, "ScrollbarGrab"),
              s7_make_integer(sc, ImGuiCol_ScrollbarGrab));
    s7_define(sc, env, s7_make_symbol(sc, "ScrollbarGrabHovered"),
              s7_make_integer(sc, ImGuiCol_ScrollbarGrabHovered));
    s7_define(sc, env, s7_make_symbol(sc, "ScrollbarGrabActive"),
              s7_make_integer(sc, ImGuiCol_ScrollbarGrabActive));
    s7_define(sc, env, s7_make_symbol(sc, "CheckMark"),
              s7_make_integer(sc, ImGuiCol_CheckMark));
    s7_define(sc, env, s7_make_symbol(sc, "SliderGrab"),
              s7_make_integer(sc, ImGuiCol_SliderGrab));
    s7_define(sc, env, s7_make_symbol(sc, "SliderGrabActive"),
              s7_make_integer(sc, ImGuiCol_SliderGrabActive));
    s7_define(sc, env, s7_make_symbol(sc, "Button"),
              s7_make_integer(sc, ImGuiCol_Button));
    s7_define(sc, env, s7_make_symbol(sc, "ButtonHovered"),
              s7_make_integer(sc, ImGuiCol_ButtonHovered));
    s7_define(sc, env, s7_make_symbol(sc, "ButtonActive"),
              s7_make_integer(sc, ImGuiCol_ButtonActive));
    s7_define(sc, env, s7_make_symbol(sc, "Header"),
              s7_make_integer(sc, ImGuiCol_Header));
    s7_define(sc, env, s7_make_symbol(sc, "HeaderHovered"),
              s7_make_integer(sc, ImGuiCol_HeaderHovered));
    s7_define(sc, env, s7_make_symbol(sc, "HeaderActive"),
              s7_make_integer(sc, ImGuiCol_HeaderActive));
    s7_define(sc, env, s7_make_symbol(sc, "Separator"),
              s7_make_integer(sc, ImGuiCol_Separator));
    s7_define(sc, env, s7_make_symbol(sc, "SeparatorHovered"),
              s7_make_integer(sc, ImGuiCol_SeparatorHovered));
    s7_define(sc, env, s7_make_symbol(sc, "SeparatorActive"),
              s7_make_integer(sc, ImGuiCol_SeparatorActive));
    s7_define(sc, env, s7_make_symbol(sc, "ResizeGrip"),
              s7_make_integer(sc, ImGuiCol_ResizeGrip));
    s7_define(sc, env, s7_make_symbol(sc, "ResizeGripHovered"),
              s7_make_integer(sc, ImGuiCol_ResizeGripHovered));
    s7_define(sc, env, s7_make_symbol(sc, "ResizeGripActive"),
              s7_make_integer(sc, ImGuiCol_ResizeGripActive));
    s7_define(sc, env, s7_make_symbol(sc, "Tab"),
              s7_make_integer(sc, ImGuiCol_Tab));
    s7_define(sc, env, s7_make_symbol(sc, "TabHovered"),
              s7_make_integer(sc, ImGuiCol_TabHovered));
    s7_define(sc, env, s7_make_symbol(sc, "TabActive"),
              s7_make_integer(sc, ImGuiCol_TabActive));
    s7_define(sc, env, s7_make_symbol(sc, "TabUnfocused"),
              s7_make_integer(sc, ImGuiCol_TabUnfocused));
    s7_define(sc, env, s7_make_symbol(sc, "TabUnfocusedActive"),
              s7_make_integer(sc, ImGuiCol_TabUnfocusedActive));
    s7_define(sc, env, s7_make_symbol(sc, "PlotLines"),
              s7_make_integer(sc, ImGuiCol_PlotLines));
    s7_define(sc, env, s7_make_symbol(sc, "PlotLinesHovered"),
              s7_make_integer(sc, ImGuiCol_PlotLinesHovered));
    s7_define(sc, env, s7_make_symbol(sc, "PlotHistogram"),
              s7_make_integer(sc, ImGuiCol_PlotHistogram));
    s7_define(sc, env, s7_make_symbol(sc, "PlotHistogramHovered"),
              s7_make_integer(sc, ImGuiCol_PlotHistogramHovered));
    s7_define(sc, env, s7_make_symbol(sc, "TextSelectedBg"),
              s7_make_integer(sc, ImGuiCol_TextSelectedBg));
    s7_define(sc, env, s7_make_symbol(sc, "DragDropTarget"),
              s7_make_integer(sc, ImGuiCol_DragDropTarget));
    s7_define(sc, env, s7_make_symbol(sc, "NavHighlight"),
              s7_make_integer(sc, ImGuiCol_NavHighlight));
    s7_define(sc, env, s7_make_symbol(sc, "NavWindowingHighlight"),
              s7_make_integer(sc, ImGuiCol_NavWindowingHighlight));
    s7_define(sc, env, s7_make_symbol(sc, "NavWindowingDimBg"),
              s7_make_integer(sc, ImGuiCol_NavWindowingDimBg));
    s7_define(sc, env, s7_make_symbol(sc, "ModalWindowDimBg"),
              s7_make_integer(sc, ImGuiCol_ModalWindowDimBg));

    s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "aod.c.imgui.col"), env);
}

void bind_window_flags(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);
    
    s7_define(sc, env, s7_make_symbol(sc, "None"),
              s7_make_integer(sc, ImGuiWindowFlags_None));
    s7_define(sc, env, s7_make_symbol(sc, "NoTitleBar"),
              s7_make_integer(sc, ImGuiWindowFlags_NoTitleBar));
    s7_define(sc, env, s7_make_symbol(sc, "NoResize"),
              s7_make_integer(sc, ImGuiWindowFlags_NoResize));
    s7_define(sc, env, s7_make_symbol(sc, "NoMove"),
              s7_make_integer(sc, ImGuiWindowFlags_NoMove));
    s7_define(sc, env, s7_make_symbol(sc, "NoScrollbar"),
              s7_make_integer(sc, ImGuiWindowFlags_NoScrollbar));
    s7_define(sc, env, s7_make_symbol(sc, "NoScrollWithMouse"),
              s7_make_integer(sc, ImGuiWindowFlags_NoScrollWithMouse));
    s7_define(sc, env, s7_make_symbol(sc, "NoCollapse"),
              s7_make_integer(sc, ImGuiWindowFlags_NoCollapse));
    s7_define(sc, env, s7_make_symbol(sc, "AlwaysAutoResize"),
              s7_make_integer(sc, ImGuiWindowFlags_AlwaysAutoResize));
    s7_define(sc, env, s7_make_symbol(sc, "NoBackground"),
              s7_make_integer(sc, ImGuiWindowFlags_NoBackground));
    s7_define(sc, env, s7_make_symbol(sc, "NoSavedSettings"),
              s7_make_integer(sc, ImGuiWindowFlags_NoSavedSettings));
    s7_define(sc, env, s7_make_symbol(sc, "NoMouseInputs"),
              s7_make_integer(sc, ImGuiWindowFlags_NoMouseInputs));
    s7_define(sc, env, s7_make_symbol(sc, "MenuBar"),
              s7_make_integer(sc, ImGuiWindowFlags_MenuBar));
    s7_define(sc, env, s7_make_symbol(sc, "HorizontalScrollbar"),
              s7_make_integer(sc, ImGuiWindowFlags_HorizontalScrollbar));
    s7_define(sc, env, s7_make_symbol(sc, "NoFocusOnAppearing"),
              s7_make_integer(sc, ImGuiWindowFlags_NoFocusOnAppearing));
    s7_define(sc, env, s7_make_symbol(sc, "NoBringToFrontOnFocus"),
              s7_make_integer(sc, ImGuiWindowFlags_NoBringToFrontOnFocus));
    s7_define(sc, env, s7_make_symbol(sc, "AlwaysVerticalScrollbar"),
              s7_make_integer(sc, ImGuiWindowFlags_AlwaysVerticalScrollbar));
    s7_define(sc, env, s7_make_symbol(sc, "AlwaysHorizontalScrollbar"),
              s7_make_integer(sc, ImGuiWindowFlags_AlwaysHorizontalScrollbar));
    s7_define(sc, env, s7_make_symbol(sc, "AlwaysUseWindowPadding"),
              s7_make_integer(sc, ImGuiWindowFlags_AlwaysUseWindowPadding));
    s7_define(sc, env, s7_make_symbol(sc, "NoNavInputs"),
              s7_make_integer(sc, ImGuiWindowFlags_NoNavInputs));
    s7_define(sc, env, s7_make_symbol(sc, "NoNavFocus"),
              s7_make_integer(sc, ImGuiWindowFlags_NoNavFocus));
    s7_define(sc, env, s7_make_symbol(sc, "UnsavedDocument"),
              s7_make_integer(sc, ImGuiWindowFlags_UnsavedDocument));
    s7_define(sc, env, s7_make_symbol(sc, "NoNav"),
              s7_make_integer(sc, ImGuiWindowFlags_NoNav));
    s7_define(sc, env, s7_make_symbol(sc, "NoDecoration"),
              s7_make_integer(sc, ImGuiWindowFlags_NoDecoration));
    s7_define(sc, env, s7_make_symbol(sc, "NoInputs"),
              s7_make_integer(sc, ImGuiWindowFlags_NoInputs));
    
    s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "aod.c.imgui.window-flags"), env);
}

// binds all the color constants under aod.c.imgui.col
void bind(s7_scheme *sc) {
    bind_colors(sc);
    bind_window_flags(sc);
}

}

} // imgui
} // s7
} // aod
