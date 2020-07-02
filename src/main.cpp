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
#include "aod/tcp_server.hpp"
#include "aod/path.hpp"
#include "aod/s7/imgui.hpp"
#include <sstream>
#include <iostream>
#include "aod/s7/c_primitives.hpp"

#define DRAW_FN "draw"
#define SETUP_FN "setup"

#define REPL_PORT 1234

int sdl_net_demo(void *data) {
    fprintf(stderr, "sdl_net_demo\n");
    return 0;
}

void scm_print(s7_scheme *sc, uint8_t c, s7_pointer port) {
    fprintf(stderr, "%c", c);
}

static void scm_load(s7_scheme *sc, const char *file) {
    if (!s7_load(sc, file)) {
        fprintf(stderr, "can't load %s\n", file);
    }
}

// Main code
int main(int, char**) {
#ifdef __WIN32__
    // AllocConsole();
#endif
    SDL_CreateThread(sdl_net_demo, "sdl_net", (void*) NULL);
    s7_scheme *sc = s7_init();
    aod::s7::Repl repl(sc);

    char *path = SDL_GetBasePath();
    std::string base_path;
    base_path += path;
    std::string scheme_path;
    scheme_path += base_path + "scheme/";
    std::string scheme_s7_path;
    scheme_s7_path += scheme_path + "s7/";
    fprintf(stderr, "base path is %s\n", base_path.c_str());

    aod::path::set(base_path);

    aod::s7::set_print_stderr(sc);
    s7_add_to_load_path(sc, "scheme");
    s7_add_to_load_path(sc, "s7");

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

    aod::s7::imgui::bind(sc);
    // (*c-primitives* 'bool) etc, for heap allocated c primitives
    // that I can pass around as a reference
    aod::s7::bind_primitives(sc);
    // libc etc magic: it creates a .c file
    aod::path::set(scheme_path);
    // scm_load(sc, "r7rs.scm");

    // aod::path::set(scheme_path);
//    s7_autoload_set_names(sc, snd_names, 5924);
    static const char *autoloads[4] = {
    // each pair of entries is entity name + file name
            "clj.scm", "clj.scm", //
            "imgui-macros.scm", "imgui_macros.scm", //
            };

    s7_autoload_set_names(sc, autoloads, 2);

    scm_load(sc, "main.scm");
    aod::path::set(base_path);

    /*
     * Socket REPL
     */
    aod::TcpServer server;
    aod::Callback cb = [&repl](const char *data) -> std::string {
        std::string res;
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
    SDL_WindowFlags window_flags = (SDL_WindowFlags) (SDL_WINDOW_OPENGL
            | SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
    SDL_Window *window = SDL_CreateWindow("Dear ImGui SDL2+OpenGL example",
    SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 600, 400, window_flags);
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
    (void) io;
    //io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
    //io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls

    // Setup Dear ImGui style
    // ImGui::StyleColorsDark();
    // ImGui::StyleColorsClassic();

    ImGuiStyle &style = ImGui::GetStyle();

    style.WindowPadding = ImVec2(8, 6);
    style.WindowRounding = 0.0f;
    // style.FramePadding                   = ImVec2(2, 4);
    // style.ItemSpacing                    = ImVec2(5, 5);
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
    bool show_another_window = false;
//    ImVec4 clear_color = ImVec4(0.45f, 0.55f, 0.60f, 1.00f);
    float clear_color[] = { 0.45f, 0.55f, 0.60f, 1.00f };
    aod::s7::float_arr arr;
    arr.size = 4;
    arr.elements = clear_color;

    // binding the clear color to imgui/clear-color
    s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "imgui/clear-color"),
            s7_make_c_object(sc, aod::s7::float_arr_type(sc), (void*) &arr));

    // Main loop
    bool done = false;

    s7_call(sc, s7_name_to_value(sc, SETUP_FN), s7_nil(sc));
    while (!done) {
        // Poll and handle events (inputs, window resize, etc.)
        // You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
        // - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application.
        // - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application.
        // Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags.
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            ImGui_ImplSDL2_ProcessEvent(&event);
            if (event.type == SDL_QUIT)
                done = true;
        }

        // Start the Dear ImGui frame
        ImGui_ImplOpenGL2_NewFrame();
        ImGui_ImplSDL2_NewFrame(window);
        ImGui::NewFrame();

        s7_call(sc, s7_name_to_value(sc, DRAW_FN), s7_nil(sc));

        // 1. Show the big demo window (Most of the sample code is in ImGui::ShowDemoWindow()! You can browse its code to learn more about Dear ImGui!).
        if (show_demo_window)
            ImGui::ShowDemoWindow(&show_demo_window);

        // 2. Show a simple window that we create ourselves. We use a Begin/End pair to created a named window.
        {
            static float f = 0.0f;
            static int counter = 0;

            ImGui::Begin("Hello, world!"); // Create a window called "Hello, world!" and append into it.

            ImGui::Text("This is some useful text."); // Display some text (you can use a format strings too)
            ImGui::Checkbox("Demo Window", &show_demo_window); // Edit bools storing our window open/close state
            ImGui::Checkbox("Another Window", &show_another_window);

            ImGui::SliderFloat("float", &f, 0.0f, 1.0f); // Edit 1 float using a slider from 0.0f to 1.0f
            ImGui::ColorEdit3("clear color", (float*) &clear_color); // Edit 3 floats representing a color

            if (ImGui::Button("Button")) // Buttons return true when clicked (most widgets return true when edited/activated)
                counter++;
            ImGui::SameLine();
            ImGui::Text("counter = %d", counter);

            ImGui::Text("Application average %.3f ms/frame (%.1f FPS)",
                    1000.0f / ImGui::GetIO().Framerate,
                    ImGui::GetIO().Framerate);
            ImGui::End();
        }

        // 3. Show another simple window.
        if (show_another_window) {
            ImGui::Begin("Another Window", &show_another_window); // Pass a pointer to our bool variable (the window will have a closing button that will clear the bool when clicked)
            ImGui::Text("Hello from another window!");
            if (ImGui::Button("Close Me"))
                show_another_window = false;
            ImGui::End();
        }

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
    }

    // Cleanup
    ImGui_ImplOpenGL2_Shutdown();
    ImGui_ImplSDL2_Shutdown();
    ImGui::DestroyContext();

    SDL_GL_DeleteContext(gl_context);
    SDL_DestroyWindow(window);
    SDL_Quit();

    return 0;
}
