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



std::mutex g_s7_mutex;

// Main code
int main(int argc, char *argv[]) {


    sf::Window window(sf::VideoMode(800, 600), "SFML works!");


    IMGUI_CHECKVERSION();
    ImGui::CreateContext();

    ImGuiIO& io = ImGui::GetIO();
    (void) io;
    ImGui::StyleColorsDark();

    ImGui::SFML::Init(window);
    ImGui_ImplOpenGL2_Init();
    ImGui::PushStyleColor(ImGuiCol_WindowBg, IM_COL32(30, 30, 30, 255));


    sf::Clock deltaClock;
    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            printf("event!\n");
            ImGui::SFML::ProcessEvent(event);

            if (event.type == sf::Event::Closed) {
                window.close();
            }
        }

        ImGui::SFML::Update(window, deltaClock.restart());
        ImGui_ImplOpenGL2_NewFrame(); // builds the font (font atlas?)
        ImGui::ShowDemoWindow();

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

