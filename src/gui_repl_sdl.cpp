// dear imgui: standalone example application for SDL2 + OpenGL
// If you are new to dear imgui, see examples/README.txt and documentation at the top of imgui.cpp.
// (SDL is a cross-platform general purpose library for handling windows, inputs, OpenGL/Vulkan/Metal graphics context creation, etc.)

// **DO NOT USE THIS CODE IF YOUR CODE/ENGINE IS USING MODERN OPENGL (SHADERS, VBO, VAO, etc.)**
// **Prefer using the code in the example_sdl_opengl3/ folder**
// See imgui_impl_sdl.cpp for details.

#include "imgui.h"
#include "imgui_impl_sdl.h"
#include "imgui_impl_opengl2.h"
#include <stdio.h>
#include "SDL.h"
#include "SDL_opengl.h"
#include "s7.h"
#include "aod/s7.hpp"
#include "aod/s7/repl.hpp"
#include "aod/path.hpp"
#include <sstream>
#include <iostream>
#include <iostream>
#include <filesystem>
#include <mutex>
#include <thread>
#include "SDL.h"

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
char** g_argv;

s7_scheme* sc;
bool running = true;
bool g_force_redraw = false;


int guiLoop() {


    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        printf("Error: %s\n", SDL_GetError());
        return 1;
    }

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);


    SDL_Window* window = SDL_CreateWindow("s7imgui - Gui Repl", 0, 0,
                                          700, 400,
                                          // SDL_WINDOWPOS_CENTERED caused the app to not show on linux. wtf
//                                           SDL_WINDOWPOS_CENTERED |
                                          SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE | SDL_WINDOW_BORDERLESS);

    SDL_GLContext gl_context = SDL_GL_CreateContext(window);
    SDL_GL_MakeCurrent(window, gl_context);
    SDL_GL_SetSwapInterval(1);    // Enable vsync

// Setup Dear ImGui context
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();




    ImGuiIO& io = ImGui::GetIO();

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

    ImGui::StyleColorsDark();

    bool sdlInit = ImGui_ImplSDL2_InitForOpenGL(window, gl_context);
    ImGui_ImplOpenGL2_Init();
    ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
    ImGui::PushStyleColor(ImGuiCol_WindowBg, IM_COL32(30, 30, 30, 255));

    s7_pointer setup_fn = s7_name_to_value(sc, "setup");
    if (setup_fn != s7_undefined(sc)) {
        s7_call(sc, setup_fn, s7_nil(sc));
    }

    //While application is running
//     bool have_drawn = false;
    unsigned int redraws_after_no_events = 0;
    while (running) {

        bool have_events = false;
        std::unique_lock<std::mutex> lock_loop(g_gui_loop_mutex);

        SDL_Event e;

        //Handle events on queue
        while (SDL_PollEvent(&e) != 0) {
//            printf("SDL event\n");
            have_events = true;
            redraws_after_no_events = 0;
            //User requests quit
            switch (e.type) {
            case SDL_QUIT:
                printf("SDL_QUIT event\n");
                running = false;
                break;
            case SDL_MOUSEBUTTONDOWN:
            case SDL_MOUSEBUTTONUP:
//                     fprintf(stderr, "Mouse down/up at (%d,%d)\n", e.motion.x,
//                             e.motion.y);
                break;
            }
            ImGui_ImplSDL2_ProcessEvent(&e);
        }
        if (!running) {
            break;
        }
//         if (redraws_after_no_events > 2 && !g_force_redraw) {
//             no need to redraw!
//             lock_loop.unlock();
//             std::this_thread::sleep_for(std::chrono::milliseconds(10));
//             continue;
//         }
        g_force_redraw = false;

        if (!have_events) {
            redraws_after_no_events++;
        }

        ImGui_ImplOpenGL2_NewFrame();
        ImGui_ImplSDL2_NewFrame(window);
        ImGui::NewFrame();

        s7_eval_c_string(sc, "(draw)");

        ImGui::Render();

        glViewport(0, 0, (int) io.DisplaySize.x, (int) io.DisplaySize.y);
        // glClearColor is freezing if the window has been closed
        glClearColor((GLclampf) clear_color.x, (GLclampf) clear_color.y,
                     (GLclampf) clear_color.z, (GLclampf) clear_color.w);
        glClear(GL_COLOR_BUFFER_BIT);
        //glUseProgram(0); // You may want this if using this code in an OpenGL 3+ context where shaders may be bound
        ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());
        SDL_GL_SwapWindow(window);

        lock_loop.unlock();
//         std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    printf("guiLoop: quit gui event loop, cleaning up \n");

    ImGui_ImplOpenGL2_Shutdown();
    ImGui_ImplSDL2_Shutdown();
    ImGui::DestroyContext();

    SDL_GL_DeleteContext(gl_context);

    SDL_DestroyWindow(window);

    SDL_Quit();

    printf("guiLoop: ----- gui loop quit ------\n");
    // fgets is blocking, so we have to forcefully quit
    exit(0);
    return 0;
}


std::mutex g_s7_mutex;

// Main code
int main(int argc, char *argv[]) {
    g_argv = argv;
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


    new std::thread(guiLoop);

    aod::s7::Repl repl(sc);

    cout << "S7 Example Repl " << endl << "> ";

    char buffer[512];
    while (running) {
//         fgets(buffer, 512, stdin);
//         std::unique_lock<std::mutex> lock_loop(g_gui_loop_mutex);
//         if (repl.handleInput(buffer)) {
//             auto result = repl.evalLastForm();
//             cout << endl << result << endl << "> ";
//             g_force_redraw = true;
//         }
    }

    return 0;
}


