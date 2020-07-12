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
#include "aod/sdl/audio.hpp"
#include "aod/nfd.hpp"
#include <iostream>

#define DRAW_FN "draw"
#define POST_DRAW_FN "post-draw"
#define SETUP_FN "setup"

#define SDL_WIDTH 1000
#define SDL_HEIGHT 600

#define REPL_PORT 1234

namespace fs = std::filesystem;

std::mutex g_s7_mutex;

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

aod::sdl::AudioObject* g_AudioObject = nullptr;

s7_pointer sc_audio_play(s7_scheme* sc, s7_pointer args) {
    if (g_AudioObject == nullptr) {
        return   s7_error(sc,
                          s7_make_symbol(sc, "audio-object-error"),
                          s7_cons(sc, s7_make_string(sc, "AudioObject is null"), s7_nil(sc)));
    }

    g_AudioObject->play();
    return s7_nil(sc);
}

s7_pointer sc_audio_stop(s7_scheme* sc, s7_pointer args) {
    if (g_AudioObject == nullptr) {
        return   s7_error(sc,
                          s7_make_symbol(sc, "audio-object-error"),
                          s7_cons(sc, s7_make_string(sc, "AudioObject is null"), s7_nil(sc)));
    }

    g_AudioObject->stop();
    return s7_nil(sc);
}

s7_pointer sc_audio_glitch(s7_scheme* sc, s7_pointer args) {
    if (g_AudioObject == nullptr) {
        return   s7_error(sc,
                          s7_make_symbol(sc, "audio-object-error"),
                          s7_cons(sc, s7_make_string(sc, "AudioObject is null"), s7_nil(sc)));
    }

//     g_AudioObject->freeWav();
    g_AudioObject->glitch();
    return s7_nil(sc);
}

// Main code
int main(int argc, char *argv[]) {
    char *path_char = SDL_GetBasePath();
    fs::path base_path = fs::path(path_char);

    s7_scheme *sc = s7_init();
    s7_define_function(sc, "exit", sc_exit, 0, 0, 0, "exits the main loop");
    s7_define_function(sc, "audio/play", sc_audio_play, 0, 0, 0, "Begins playback");
    s7_define_function(sc, "audio/stop", sc_audio_stop, 0, 0, 0, "Stops playback");
    s7_define_function(sc, "audio/glitch", sc_audio_glitch, 0, 0, 0, "Frees the audio buffer");


    aod::s7::set_print_stderr(sc);
    s7_add_to_load_path(sc, SDL_GetBasePath());
    s7_add_to_load_path(sc, (base_path / "scheme").c_str());
    s7_add_to_load_path(sc, (base_path / "aod").c_str());

    aod::s7::bind_all(sc);
    aod::s7::set_autoloads(sc);

    aod::s7::load_file(sc, "audio_player.scm");

    /*
     * Socket REPL
     */
    aod::s7::Repl repl(sc);
    aod::TcpServer server;
    aod::Callback cb = [&repl](const char *data) -> std::string {
        std::string res;
        std::unique_lock guard(g_s7_mutex);

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



    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMECONTROLLER)
            != 0) {
        printf("Error: %s\n", SDL_GetError());
        return -1;
    }

    auto audioObject = aod::sdl::AudioObject::fromFile((base_path / "owl.wav").c_str());
    if (audioObject) {
        audioObject->setId(1);

    } else {
        fprintf(stderr, "Could not create audio object: %s\n", SDL_GetError());

    }
//     audioObject->freeWav();

    g_AudioObject = audioObject.get();

//     SDL_PauseAudio(0);
    // unfortunately you can only play 1 audio source per time like this
    // for more, SDL_mixer is needed

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

    aod::s7::sdl::bind(sc, window);

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
    // (void) io;
    ImGuiStyle &style = ImGui::GetStyle();

    style.WindowPadding = ImVec2(8, 6);
    style.WindowRounding = 0.0f;

    // Setup Platform/Renderer bindings
    ImGui_ImplSDL2_InitForOpenGL(window, gl_context);
    ImGui_ImplOpenGL2_Init();

    float clear_color[] = { 0.3f, 0.3f, 0.3f, 1.00f };

//     binding the clear color to imgui/clear-color
    s7_define(sc, s7_nil(sc), s7_make_symbol(sc, "imgui/clear-color"),
              s7_make_c_object(sc, aod::s7::foreign::tag_float_arr(sc),
                               (void*) &clear_color));

    s7_call(sc, s7_name_to_value(sc, SETUP_FN), s7_nil(sc));

    // Main loop
    while (main_loop_running) {
        std::lock_guard guard(g_s7_mutex);

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

        // Rendering
        ImGui::Render();
        glViewport(0, 0, (int) io.DisplaySize.x, (int) io.DisplaySize.y);
        glClearColor(clear_color[0], clear_color[1], clear_color[2],
                     clear_color[3]);
        glClear(GL_COLOR_BUFFER_BIT);
        ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());
        SDL_GL_SwapWindow(window);

//         s7_eval_c_string(sc, "(if (defined? 'post-draw) (post-draw))");
//         SDL_Delay(100);
    }
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

