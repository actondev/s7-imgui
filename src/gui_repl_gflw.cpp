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

// gflw
#include "imgui_impl_glfw.h"
#include <GLFW/glfw3.h>
// #include <SFML/Window.hpp>
// #include <SFML/OpenGL.hpp>
// #include "aod/imgui/imgui-SFML.h"

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

static void glfw_error_callback(int error, const char* description) {
    fprintf(stderr, "Glfw Error %d: %s\n", error, description);
}

char** g_argv;

// https://github.com/glfw/glfw/issues/310#issuecomment-52048508
void glfwSetWindowCenter(GLFWwindow* window) {
    // Get window position and size
    int window_x, window_y;
    glfwGetWindowPos(window, &window_x, &window_y);

    int window_width, window_height;
    glfwGetWindowSize(window, &window_width, &window_height);

    // Halve the window size and use it to adjust the window position to the center of the window
    window_width *= 0.5;
    window_height *= 0.5;

    window_x += window_width;
    window_y += window_height;

    // Get the list of monitors
    int monitors_length;
    GLFWmonitor **monitors = glfwGetMonitors(&monitors_length);

    if (monitors == NULL) {
        // Got no monitors back
        return;
    }

    // Figure out which monitor the window is in
    GLFWmonitor *owner = NULL;
    int owner_x, owner_y, owner_width, owner_height;

    for (int i = 0; i < monitors_length; i++) {
        // Get the monitor position
        int monitor_x, monitor_y;
        glfwGetMonitorPos(monitors[i], &monitor_x, &monitor_y);

        // Get the monitor size from its video mode
        int monitor_width, monitor_height;
        GLFWvidmode *monitor_vidmode = (GLFWvidmode*) glfwGetVideoMode(monitors[i]);

        if (monitor_vidmode == NULL) {
            // Video mode is required for width and height, so skip this monitor
            continue;

        } else {
            monitor_width = monitor_vidmode->width;
            monitor_height = monitor_vidmode->height;
        }

        // Set the owner to this monitor if the center of the window is within its bounding box
        if ((window_x > monitor_x && window_x < (monitor_x + monitor_width)) && (window_y > monitor_y && window_y < (monitor_y + monitor_height))) {
            owner = monitors[i];

            owner_x = monitor_x;
            owner_y = monitor_y;

            owner_width = monitor_width;
            owner_height = monitor_height;
        }
    }

    if (owner != NULL) {
        // Set the window position to the center of the owner monitor
        glfwSetWindowPos(window, owner_x + (owner_width * 0.5) - window_width, owner_y + (owner_height * 0.5) - window_height);
    }
}


int guiLoop() {
    glfwSetErrorCallback(glfw_error_callback);
    if (!glfwInit()) {
        fprintf(stderr, "Could not glfwInit!\n");
        return 1;
    }

    // TODO make this controllable somewhoe?
    // from the (setup) function??
    glfwWindowHint(GLFW_DECORATED, GLFW_FALSE);
    printf("creating window\n");
    GLFWwindow* window = glfwCreateWindow(700, 400, "S7 Gui Repl (gflw)", NULL, NULL);
//     glfwSetWindowCenter(window);
    if (window == NULL) {
        fprintf(stderr, "Could not create window!\n");
        return 1;
    }
    
//     printf("created window\n");

    IMGUI_CHECKVERSION();
    ImGui::CreateContext();

    ImGuiIO& io = ImGui::GetIO();
    (void) io;
    ImGui::StyleColorsDark();


    // FONT
    fs::path fonts_path = fs::path(g_argv[0]).parent_path() / "fonts";
    fs::path font_file = fonts_path / "Roboto-Medium.ttf";
    printf("font file %s\n", font_file.string().c_str());


    ImVector<ImWchar> ranges;
    ImFontGlyphRangesBuilder builder;
    builder.AddRanges(io.Fonts->GetGlyphRangesDefault());
    builder.AddRanges(io.Fonts->GetGlyphRangesCyrillic());
    builder.AddText("—’");
    // no need for u8"..." ?
    builder.AddText(u8"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ");
    builder.AddText("αβγδεζηθικλμνξοπρστυφχψως");
    builder.AddText("ΆΈΉΊΌΎΏ");
    builder.AddText("άέήίόύώ");
    builder.BuildRanges(&ranges);

    io.Fonts->AddFontFromFileTTF(font_file.string().c_str(), 18, NULL, ranges.Data);

    // !!! FONT


    glfwMakeContextCurrent(window);
    glfwSwapInterval(1); // Enable vsync

    ImGui_ImplGlfw_InitForOpenGL(window, true);
    ImGui_ImplOpenGL2_Init();
    ImGui::PushStyleColor(ImGuiCol_WindowBg, IM_COL32(30, 30, 30, 255));

//     sf::Clock deltaClock;
    while (!glfwWindowShouldClose(window)) {
        // non-blocking, should sleep
//         glfwPollEvents();
        // blocking, no need to sleep
        glfwWaitEvents();
        std::unique_lock<std::mutex> lock_loop(g_gui_loop_mutex);

        ImGui_ImplOpenGL2_NewFrame();
        ImGui_ImplGlfw_NewFrame();
        ImGui::NewFrame();

        // TODO do not redraw if no event..?
        s7_eval_c_string(sc, "(draw)");

        ImGui::Render();
        int display_w, display_h;
        glfwGetFramebufferSize(window, &display_w, &display_h);
        glViewport(0, 0, display_w, display_h);
        glClearColor(0, 0, 0, 1);
        glClear(GL_COLOR_BUFFER_BIT);
        ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());
        glfwMakeContextCurrent(window);
        glfwSwapBuffers(window);
//         std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    ImGui_ImplOpenGL2_Shutdown();
    ImGui_ImplGlfw_Shutdown();
    ImGui::DestroyContext();

    glfwDestroyWindow(window);
    glfwTerminate();

    printf("guiLoop: ----- gui loop quit ------\n");
    // fgets is blocking, so we have to forcefully quit
    exit(0);
    return 0;

}


std::mutex g_s7_mutex;

// Main code
int main(int argc, char *argv[]) {
    g_argv = argv;
    // TODO gotta find the way to get the application path here
//     fs::path cwd_launch = fs::current_path();
    fs::path base_path = fs::path(argv[0]).parent_path();
    fs::path scheme_path = base_path / "scheme";
    printf("scheme path: %s\n", scheme_path.c_str());

    sc = aod::s7::init(scheme_path);

    if (argc >= 2) {
        fs::path passed_file = scheme_path / argv[1];
        // TODO test if it exists, if not try "cwd/argv[1]"
        s7_add_to_load_path(sc, passed_file.parent_path().string().c_str());
        printf("loading scheme file: %s\n", passed_file.c_str());
        aod::s7::load_file(sc, passed_file.c_str());
    } else {
        aod::s7::load_file(sc, "main.scm");
    }

    bool run_thread_in_loop_with_repl = false;

    if (!run_thread_in_loop_with_repl) {
        guiLoop();
    } else {
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
    }

    return 0;
}

