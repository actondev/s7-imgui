// dear imgui: standalone example application for SDL2 + OpenGL
// If you are new to dear imgui, see examples/README.txt and documentation at the top of imgui.cpp.
// (SDL is a cross-platform general purpose library for handling windows, inputs, OpenGL/Vulkan/Metal graphics context creation, etc.)

// **DO NOT USE THIS CODE IF YOUR CODE/ENGINE IS USING MODERN OPENGL (SHADERS, VBO, VAO, etc.)**
// **Prefer using the code in the example_sdl_opengl3/ folder**
// See imgui_impl_sdl.cpp for details.


#include <stdio.h>
#include <sstream>
#include <iostream>
#include <filesystem>
#include <mutex>
#include <thread>

// imgui
#include "imgui.h"
#include "imgui_impl_opengl2.h"
// sfml
#include <SFML/Window.hpp>
#include <SFML/OpenGL.hpp>
#include "aod/imgui/imgui-SFML.h"


#define SCREEN_WIDTH 400
#define SCREEN_HEIGHT 400


using std::cout;
using std::cerr;
using std::endl;

// globals, cause that's how we like it
std::mutex g_gui_loop_mutex;
bool running = true;
bool g_force_redraw = false;

namespace fs = std::filesystem;


std::mutex g_s7_mutex;

// Main code
int main(int argc, char *argv[]) {


    sf::Window window(sf::VideoMode(800, 600), "SFML works!");


    IMGUI_CHECKVERSION();
    ImGui::CreateContext();

    ImGuiIO& io = ImGui::GetIO();
    (void) io;
    ImGui::StyleColorsDark();

    fs::path cwd_launch = fs::current_path();
    fs::path base_path = fs::path(argv[0]);
    fprintf(stderr, "argv[0] %s\n", argv[0]);
    fprintf(stderr, "current path %s\n", cwd_launch.c_str());
    fs::path font_file = cwd_launch / ".." / ".." / "fonts" / "Roboto-Medium.ttf";
//     fs::path font_file = cwd_launch / ".." / ".." / "fonts" / "Arial Unicode MS.ttf";
    std::cout << "font file " << fs::path(font_file) << std::endl;


    ImVector<ImWchar> ranges;
    ImFontGlyphRangesBuilder builder;
    builder.AddRanges(io.Fonts->GetGlyphRangesDefault());
    // no need for u8"..." ?
    builder.AddText(u8"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ");
    builder.AddText("αβγδεζηθικλμνξοπρστυφχψω");
    builder.AddText("ΆΈΉΊΌΎΏ");
    builder.AddText("άέήίόύώ");
//     builder.AddRanges(io.Fonts->GetGlyphRangesJapanese()); // Add one of the default ranges
    builder.BuildRanges(&ranges);                          // Build the final result (ordered ranges with all the unique characters submitted)


    // utf8 font
    io.Fonts->AddFontFromFileTTF(font_file.string().c_str(), 18, NULL, ranges.Data);

//     io.Fonts->AddFontFromFileTTF(font_file.string().c_str(), 18, NULL, io.Fonts->GetGlyphRangesJapanese());   // Load Japanese characters

//     io.Fonts->GetTexDataAsRGBA32();
//     io.ImeWindowHandle = MY_HWND;      // To input using Microsoft IME, give ImGui the hwnd of your application

    ImGui::SFML::Init(window);
    ImGui_ImplOpenGL2_Init();
    ImGui::PushStyleColor(ImGuiCol_WindowBg, IM_COL32(30, 30, 30, 255));



    sf::Clock deltaClock;
    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            printf("event!\n");
            // TODO I get click events even if another window
            // has focus (on top of this windows)
            // the window.hasFocus doesn't help
            ImGui::SFML::ProcessEvent(event);

            if (event.type == sf::Event::Closed) {
                window.close();
            }
        }

        ImGui::SFML::Update(window, deltaClock.restart());
        ImGui_ImplOpenGL2_NewFrame(); // builds the font (font atlas?)
        ImGui::ShowDemoWindow();

        ImGui::Begin("Font showcase");
        ImGui::Text("One two three four");
        ImGui::Text(u8"Ένα δύο τρία τέσσερα");
        ImGui::End();

//         window.clear();

        glViewport(0, 0, (int) io.DisplaySize.x, (int) io.DisplaySize.y);
        glClearColor(0, 0, 0, 0);
        glClear(GL_COLOR_BUFFER_BIT);


        ImGui::SFML::Render(window);
        window.display();
    }

    ImGui_ImplOpenGL2_Shutdown();
    ImGui::DestroyContext();


    return 0;
}


