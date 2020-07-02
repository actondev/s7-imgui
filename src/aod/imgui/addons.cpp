#include "imgui.h"
#include "imgui_internal.h"

namespace aod {
namespace imgui {

/**
 * as seen on https://github.com/ocornut/imgui/issues/942
 */
bool Knob(const char *label, float *value_p, float minv, float maxv) {
    ImGuiStyle &style = ImGui::GetStyle();
    float line_height = ImGui::GetTextLineHeight();

    ImVec2 p = ImGui::GetCursorScreenPos();
    float sz = 36.0f;
    float radio = sz * 0.5f;
    ImVec2 center = ImVec2(p.x + radio, p.y + radio);
    float val1 = (value_p[0] - minv) / (maxv - minv);
    char textval[32];
    ImFormatString(textval, IM_ARRAYSIZE(textval), "%04.1f", value_p[0]);

    // ImVec2 textpos = p;
    float gamma = M_PI / 4.0f; //0 value in knob
    float alpha = (M_PI - gamma) * val1 * 2.0f + gamma;

    float x2 = -sinf(alpha) * radio + center.x;
    float y2 = cosf(alpha) * radio + center.y;

    ImGui::InvisibleButton(label,
            ImVec2(sz, sz + line_height + style.ItemInnerSpacing.y));

    bool is_active = ImGui::IsItemActive();
    bool is_hovered = ImGui::IsItemHovered();
    bool touched = false;

    if (is_active) {
        touched = true;
        ImVec2 mp = ImGui::GetIO().MousePos;
        alpha = atan2f(mp.x - center.x, center.y - mp.y) + M_PI;
        alpha = ImMax(gamma, ImMin((float) (2.0f * M_PI - gamma), alpha));
        float value = 0.5f * (alpha - gamma) / (M_PI - gamma);
        value_p[0] = value * (maxv - minv) + minv;
    }

    ImU32 col32 = ImGui::GetColorU32(
            is_active ? ImGuiCol_FrameBgActive :
            is_hovered ? ImGuiCol_FrameBgHovered : ImGuiCol_FrameBg);
    ImU32 col32line = ImGui::GetColorU32(ImGuiCol_SliderGrabActive);
    ImU32 col32text = ImGui::GetColorU32(ImGuiCol_Text);
    ImDrawList *draw_list = ImGui::GetWindowDrawList();
    draw_list->AddCircleFilled(center, radio, col32, 16);
    draw_list->AddLine(center, ImVec2(x2, y2), col32line, 1);
//    draw_list->AddText(textpos, col32text, textval);
    draw_list->AddText(ImVec2(p.x, p.y + sz + style.ItemInnerSpacing.y),
            col32text, label);

    return touched;
}

}
}
