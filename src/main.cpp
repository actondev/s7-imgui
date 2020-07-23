// dear imgui: standalone example application for SDL2 + OpenGL
// If you are new to dear imgui, see examples/README.txt and documentation at the top of imgui.cpp.
// (SDL is a cross-platform general purpose library for handling windows, inputs, OpenGL/Vulkan/Metal graphics context creation, etc.)

// **DO NOT USE THIS CODE IF YOUR CODE/ENGINE IS USING MODERN OPENGL (SHADERS, VBO, VAO, etc.)**
// **Prefer using the code in the example_sdl_opengl3/ folder**
// See imgui_impl_sdl.cpp for details.

#include <aod/imgui/addons.hpp>
#include "imgui.h"
#include "imgui_impl_sdl.h"
#include "imgui_impl_opengl2.h"
#include <stdio.h>
#include "SDL.h"
#include "SDL_opengl.h"
#include "s7.h"
#include "aod/s7.hpp"
#include "aod/s7/repl.hpp"
#include "aod/tcp_server.hpp"
#include "aod/path.hpp"
#include "aod/s7/imgui.hpp"
#include <sstream>
#include <iostream>
#include "aod/s7/foreign_primitives.hpp"
#include "aod/s7/foreign_primitives_arr.hpp"
#include "aod/s7/imgui_addons.hpp"
#include "aod/s7/gl.hpp"
#include "aod/s7/sdl.hpp"
#include <iostream>
#include <filesystem>
#include <mutex>
#include <thread>

#define DRAW_FN "draw"
#define POST_DRAW_FN "post-draw"
#define SETUP_FN "setup"

#define SDL_WIDTH 1000
#define SDL_HEIGHT 600

#define REPL_PORT 1234

namespace fs = std::filesystem;

std::mutex g_s7_mutex;

int sdl_net_demo(void *data) {
    fprintf(stderr, "sdl_net_demo\n");
    return 0;
}

void scm_print(s7_scheme *sc, uint8_t c, s7_pointer port) {
    fprintf(stderr, "%c", c);
}

// Main loop
bool main_loop_running = true;

// a way to exit the application from the scheme file
// or.. no need to? 'exit is defined in s7 and.. it's callable heh
s7_pointer sc_exit(s7_scheme *sc, s7_pointer args) {
    main_loop_running = false;
    fprintf(stderr, "called (exit) from s7, quitting main loop\n");
    return s7_nil(sc);
}

// Main code
int main(int argc, char *argv[]) {
#ifdef __WIN32__
    // AllocConsole();
#endif

    fs::path cwd_launch = fs::current_path();
    char *path_char = SDL_GetBasePath();
    fs::path base_path = fs::path(path_char);
    fprintf(stderr, "argv[0] %s\n", argv[0]);

    SDL_CreateThread(sdl_net_demo, "sdl_net", (void*) NULL);
    s7_scheme *sc = s7_init();
    s7_define_function(sc, "exit", sc_exit, 0, 0, 0, "exits the main loop");
    aod::s7::Repl repl(sc);

    std::string base_path_str;
    base_path_str += path_char;
    std::string scheme_path;
    scheme_path += base_path_str + "scheme/";
    std::string scheme_s7_path;
    scheme_s7_path += scheme_path + "s7/";
    fprintf(stderr, "base path is %s\n", base_path_str.c_str());

    aod::s7::set_print_stderr(sc);
    std::cout << "scheme path is " << base_path / "scheme" << '\n';
    s7_add_to_load_path(sc, (base_path / "scheme").string().c_str());
//    s7_add_to_load_path(sc, "s7");

    aod::s7::set_print_stderr(sc);
    s7_eval_c_string(sc, "(display \"hello from s7 from cpp\n\"))");

    /**
     * adds definitions for imgui under aod.imgui/...
     * eg
     * - (aod.imgui/begin WINDOW_TITLE)
     * - (aod.imgui/end)
     *
     * etc
     */

    s7_pointer primitives_env = aod::s7::make_env(sc);
    // eg ((aod.c.foreign 'new-bool) #t) for a bool* pointer with initial value true
    aod::s7::foreign::bind_primitives(sc, primitives_env);
    // eg ((aod.c.foreign 'new-bool[]) 4) for a bool[4] array
    aod::s7::foreign::bind_primitives_arr(sc, primitives_env);

    // imgui bindings
    aod::s7::imgui::bind(sc);
    aod::s7::imgui::bind_knob(sc);

    // gl bindings (eg gl/save-screenshot)
    aod::s7::gl::bind(sc);

//     s7_autoload_set_names(sc, autoloads, AOD_S7_AUTOLOADS_TIMES_2 / 2);
    aod::s7::set_autoloads(sc);

    if (argc >= 2) {
        fprintf(stderr, "Passed custom scheme file %s\n", argv[1]);
        fs::path passed_file = cwd_launch / argv[1];
//        passed_file.append()
//        passed_file.replace_filename(argv[1]);
//        std::cout << "cwd was " << cwd_launch << '\n';
//        std::cout << "cwd is " << cwd_launch << " passed file " << passed_file << '\n';
        std::cout << "path of passed file is " << passed_file.parent_path()
                  << '\n';
        s7_add_to_load_path(sc, passed_file.parent_path().string().c_str());
        aod::s7::load_file(sc, passed_file.string().c_str());
    } else {
        aod::s7::load_file(sc, "main.scm");
    }

//    aod::path::set(base_path_str);

    /*
     * Socket REPL
     */
    aod::TcpServer server;
    aod::Callback cb = [&repl](const char *data) -> std::string {
        std::string res;
        std::lock_guard guard(g_s7_mutex);

        if (repl.handleInput(data)) {
            res = repl.evalLastForm();
            res += "\n> ";
            return res;
        }
        return res;
    };
    server.listen(REPL_PORT, cb, "s7-imgui repl\n> ");

    /*
     * Initializing sdl etc
     */

    // Setup SDL
    // (Some versions of SDL before <2.0.10 appears to have performance/stalling issues on a minority of Windows systems,
    // depending on whether SDL_INIT_GAMECONTROLLER is enabled or disabled.. updating to latest version of SDL is recommended!)
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMECONTROLLER)
            != 0) {
        printf("Error: %s\n", SDL_GetError());
        return -1;
    }

    // Setup window
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 2);
    SDL_WindowFlags window_flags = (SDL_WindowFlags)(SDL_WINDOW_OPENGL
                                   | SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
    SDL_Window *window = SDL_CreateWindow("Dear ImGui SDL2+OpenGL example",
                                          SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, SDL_WIDTH, SDL_HEIGHT,
                                          window_flags);

//    SDL_SetWindowSize()

    // sdl bindings (sdl/set-window-size!)
    aod::s7::sdl::bind_TODO_REMOVE(sc, window);

    // .....

    if (window == NULL) {
        fprintf(stderr, "Could not create SDL window");
        return -1;
    }
    SDL_GLContext gl_context = SDL_GL_CreateContext(window);
    SDL_GL_MakeCurrent(window, gl_context);
    SDL_GL_SetSwapInterval(1); // Enable vsync

    // Setup Dear ImGui context
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO &io = ImGui::GetIO();
    io.IniFilename = NULL; // Disable imgui.ini
    (void) io;

    ImGuiStyle &style = ImGui::GetStyle();

    style.WindowPadding = ImVec2(8, 6);
    style.WindowRounding = 0.0f;
    style.Colors[ImGuiCol_Text] = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
    style.Colors[ImGuiCol_TextDisabled] = ImVec4(0.50f, 0.50f, 0.50f, 1.00f);
    style.Colors[ImGuiCol_WindowBg] = ImVec4(0.06f, 0.06f, 0.06f, 0.94f);
    style.Colors[ImGuiCol_Button] = ImVec4(0.44f, 0.44f, 0.44f, 0.40f);
    style.Colors[ImGuiCol_ButtonHovered] = ImVec4(0.46f, 0.47f, 0.48f, 1.00f);
    style.Colors[ImGuiCol_ButtonActive] = ImVec4(0.42f, 0.42f, 0.42f, 1.00f);

    // Setup Platform/Renderer bindings
    ImGui_ImplSDL2_InitForOpenGL(window, gl_context);
    ImGui_ImplOpenGL2_Init();

    // Our state
    bool show_demo_window = false;
    float clear_color[] = { 0.45f, 0.55f, 0.60f, 1.00f };

    // binding the clear color to imgui/clear-color
    s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "imgui/clear-color"),
              s7_make_c_object(sc, aod::s7::foreign::tag_float_arr(sc),
                               (void*) &clear_color));

    s7_call(sc, s7_name_to_value(sc, SETUP_FN), s7_nil(sc));

    // Main loop
    while (main_loop_running) {
        {
            {
                std::unique_lock lock(g_s7_mutex);
                // Poll and handle events (inputs, window resize, etc.)
                // You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
                // - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application.
                // - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application.
                // Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags.
                SDL_Event event;
                while (SDL_PollEvent(&event)) {
                    ImGui_ImplSDL2_ProcessEvent(&event);
                    if (event.type == SDL_QUIT)
                        main_loop_running = false;
                }

                // Start the Dear ImGui frame
                ImGui_ImplOpenGL2_NewFrame();
                ImGui_ImplSDL2_NewFrame(window);
                ImGui::NewFrame();

                s7_call(sc, s7_name_to_value(sc, DRAW_FN), s7_nil(sc));

                // 1. Show the big demo window (Most of the sample code is in ImGui::ShowDemoWindow()! You can browse its code to learn more about Dear ImGui!).
                if (show_demo_window)
                    ImGui::ShowDemoWindow(&show_demo_window);

//        {
//            // testing custom widgets
//            ImGui::Begin("custom widgets");
//            ImGui::Text("knobs");
//
//            aod::imgui::Knob("knob 1", &knob_value, 0, 1);
//            ImGui::SameLine();
//            aod::imgui::Knob("knob 2", &knob_value, 0, 1);
//            ImGui::SameLine();
//            aod::imgui::Knob("knob 3", &knob_value, 0, 1);
//            ImGui::End();
//        }

                // Rendering
                ImGui::Render();
                glViewport(0, 0, (int) io.DisplaySize.x, (int) io.DisplaySize.y);
//	glClearColor(clear_color.x, clear_color.y, clear_color.z,
//		     clear_color.w);
                glClearColor(clear_color[0], clear_color[1], clear_color[2],
                             clear_color[3]);
                glClear(GL_COLOR_BUFFER_BIT);
                //glUseProgram(0); // You may want this if using this code in an OpenGL 3+ context where shaders may be bound
                ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());
                SDL_GL_SwapWindow(window);

//        s7_pointer post_draw = s7_name_to_value(sc, POST_DRAW_FN);

//        s7_pointer post_draw = s7_symbol_table_find_name(sc, POST_DRAW_FN);
//        s7_pointer post_draw = s7_symbol_table_find_name(sc, POST_DRAW_FN); // returns a symbol
////         s7_is_defined
//        if (post_draw) {
//            s7_call(sc, s7_symbol_value(sc, post_draw), s7_nil(sc));
//        }
                s7_eval_c_string(sc, "(if (defined? 'post-draw) (post-draw))");
            }
//             lock.release();

            SDL_Delay(20);
        }
    }
//    s7_is_
    fprintf(stderr, "Quit main loop, cleaning up..\n");

    // Cleanup
    ImGui_ImplOpenGL2_Shutdown();
    ImGui_ImplSDL2_Shutdown();
    ImGui::DestroyContext();

    SDL_GL_DeleteContext(gl_context);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return 0;
}

