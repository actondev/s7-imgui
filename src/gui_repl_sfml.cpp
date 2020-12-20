// dear imgui: standalone example application for SDL2 + OpenGL
// If you are new to dear imgui, see examples/README.txt and documentation at the top of imgui.cpp.
// (SDL is a cross-platform general purpose library for handling windows, inputs, OpenGL/Vulkan/Metal graphics context creation, etc.)

// **DO NOT USE THIS CODE IF YOUR CODE/ENGINE IS USING MODERN OPENGL (SHADERS, VBO, VAO, etc.)**
// **Prefer using the code in the example_sdl_opengl3/ folder**
// See imgui_impl_sdl.cpp for details.


#include <stdio.h>
#include "s7.h"
#include "aod/s7.hpp"
#include "aod/s7/repl.hpp"
#include "aod/path.hpp"
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

#define DRAW_FN "draw"
#define POST_DRAW_FN "post-draw"
#define SETUP_FN "setup"

#define SCREEN_WIDTH 400
#define SCREEN_HEIGHT 400

#define REPL_PORT 1234

using std::cout;
using std::cerr;
using std::endl;
namespace fs = std::filesystem;

// globals, cause that's how we like it
std::mutex g_gui_loop_mutex;
s7_scheme* sc;
bool running = true;
bool g_force_redraw = false;


int guiLoop() {
    sf::Window window(sf::VideoMode(200, 200), "S7 GUI REPL!");


    IMGUI_CHECKVERSION();
    ImGui::CreateContext();

    ImGuiIO& io = ImGui::GetIO();
    (void) io;
    ImGui::StyleColorsDark();

    ImGui::SFML::Init(window);
    ImGui_ImplOpenGL2_Init();
    ImGui::PushStyleColor(ImGuiCol_WindowBg, IM_COL32(30, 30, 30, 255));

    printf("guiLoop: quit gui event loop, cleaning up \n");

    sf::Clock deltaClock;
    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            ImGui::SFML::ProcessEvent(event);

            if (event.type == sf::Event::Closed) {
                window.close();
            }
        }

        ImGui::SFML::Update(window, deltaClock.restart());
        ImGui_ImplOpenGL2_NewFrame(); // builds the font (font atlas?)

        s7_eval_c_string(sc, "(draw)");

//         window.clear();
        ImGui::SFML::Render(window);
        window.display();
        // TODO do not redraw if no event..?
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    ImGui_ImplOpenGL2_Shutdown();
    ImGui::DestroyContext();


    printf("guiLoop: ----- gui loop quit ------\n");
    // fgets is blocking, so we have to forcefully quit
    exit(0);
    return 0;

}


std::mutex g_s7_mutex;

// Main code
int main(int argc, char *argv[]) {
    // TODO gotta find the way to get the application path here
    fs::path cwd_launch = fs::current_path();
    fs::path base_path = fs::path(argv[0]);
    fprintf(stderr, "argv[0] %s\n", argv[0]);
    fprintf(stderr, "current path %s\n", cwd_launch.c_str());

    fs::path scheme_path = cwd_launch / "src/scheme";
    std::cout << "scheme path is " << cwd_launch / "src/scheme" << '\n';
    sc = aod::s7::init(scheme_path);

    if (argc >= 2) {
        fprintf(stderr, "Passed custom scheme file %s\n", argv[1]);
        fs::path passed_file = cwd_launch / argv[1];
        std::cout << "path of passed file is " << passed_file.parent_path()
                  << '\n';
        s7_add_to_load_path(sc, passed_file.parent_path().string().c_str());
        aod::s7::load_file(sc, passed_file.string().c_str());
    } else {
        aod::s7::load_file(sc, "main.scm");
    }

    new std::thread(guiLoop);

    aod::s7::Repl repl(sc);

    cout << "S7 Example Repl " << endl << "> ";

    char buffer[512];
    while (running) {
        fgets(buffer, 512, stdin);
        std::unique_lock<std::mutex> lock_loop(g_gui_loop_mutex);
        if (repl.handleInput(buffer)) {
            auto result = repl.evalLastForm();
            cout << endl << result << endl << "> ";
            g_force_redraw = true;
        }
    }

    return 0;
}

