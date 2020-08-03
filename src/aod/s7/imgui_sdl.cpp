// for creating the window and drawing ourselves
// not having an event loop that calls us
// this is meant for quick prototyping of drawing

#include "./imgui_sdl.hpp"
#include "imgui_impl_sdl.h"
#include "imgui_impl_opengl2.h"
#include "imgui.h"
#include "SDL.h"
#include "SDL_opengl.h"
#include <stdio.h> //fprintf

namespace aod {
namespace s7 {
namespace imgui_sdl {

struct Data {
    SDL_Window* window;
    SDL_GLContext gl_context;
    bool destroyed = false;
    bool should_quit = false;
};

s7_pointer setup(s7_scheme* sc, s7_pointer args) {
    int w = s7_number_to_integer(sc, s7_list_ref(sc, args, 0));
    int h = s7_number_to_integer(sc, s7_list_ref(sc, args, 1));
    SDL_WindowFlags window_flags = (SDL_WindowFlags)(SDL_WINDOW_OPENGL
                                   | SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
    SDL_Window* window = SDL_CreateWindow("Dear ImGui SDL2+OpenGL example",
                                          SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, w, h,
                                          window_flags);

    if (window == NULL) {
        fprintf(stderr, "Could not create SDL window");
        return s7_error(sc, s7_make_symbol(sc, "sdl-error"),
                        s7_cons(sc, s7_make_string(sc, "Could not create SDL window"), s7_nil(sc)));
    }

    SDL_GLContext gl_context = SDL_GL_CreateContext(window);
    SDL_GL_MakeCurrent(window, gl_context);
    SDL_GL_SetSwapInterval(1); // Enable vsync

    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO &io = ImGui::GetIO();
    io.IniFilename = NULL; // Disable imgui.ini
    (void) io;

    ImGui_ImplSDL2_InitForOpenGL(window, gl_context);
    ImGui_ImplOpenGL2_Init();


    Data* data = new Data;
    data->window = window;
    data->gl_context = gl_context;

    return s7_make_c_pointer(sc, (void*)data);
}

s7_pointer prepare(s7_scheme* sc, s7_pointer args) {
    Data* data = (Data*) s7_c_pointer(s7_car(args));
    if (data->destroyed) {
        return s7_error(sc, s7_make_symbol(sc, "imgui-sdl-error"),
                        s7_cons(sc, s7_make_string(sc, "Already destroyed"), s7_nil(sc)));
    }
    ImGui_ImplOpenGL2_NewFrame();
    ImGui_ImplSDL2_NewFrame(data->window);
    ImGui::NewFrame();

    return s7_nil(sc);
}

s7_pointer destroy(s7_scheme* sc, s7_pointer args) {
    Data* data = (Data*) s7_c_pointer(s7_car(args));
    if (data->destroyed) {
        return s7_error(sc, s7_make_symbol(sc, "imgui-sdl-error"),
                        s7_cons(sc, s7_make_string(sc, "Already destroyed"), s7_nil(sc)));
    }

    ImGui_ImplOpenGL2_Shutdown();
    ImGui_ImplSDL2_Shutdown();
    ImGui::DestroyContext();

    SDL_GL_DeleteContext(data->gl_context);
    SDL_DestroyWindow(data->window);
    SDL_Quit();

    data->destroyed = true;

    return s7_nil(sc);
}

s7_pointer flush(s7_scheme* sc, s7_pointer args) {
    Data* data = (Data*) s7_c_pointer(s7_car(args));
    if (data->destroyed) {
        return s7_error(sc, s7_make_symbol(sc, "imgui-sdl-error"),
                        s7_cons(sc, s7_make_string(sc, "Already destroyed"), s7_nil(sc)));
    }
    ImGui::Render();
    ImGuiIO &io = ImGui::GetIO();

    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        ImGui_ImplSDL2_ProcessEvent(&event);
        if (event.type == SDL_QUIT) {
            fprintf(stderr, "Got SDL quit signal\n");
            data->should_quit = true;
            // should i destroy here, or after painting..?
            return destroy(sc, args);
        }
    }

    glViewport(0, 0, (int) io.DisplaySize.x, (int) io.DisplaySize.y);
    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT);
    //glUseProgram(0); // You may want this if using this code in an OpenGL 3+ context where shaders may be bound
    ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());
    SDL_GL_SwapWindow(data->window);

    return s7_nil(sc);

}

void bind(s7_scheme* sc) {
    s7_pointer env = s7_inlet(sc, s7_nil(sc));
    s7_gc_protect(sc, env);
    // other possible names:
    // imgui-proto (for prototype)
    // imgui-playground
    // imgui-scratch

    s7_define(sc, env, s7_make_symbol(sc, "setup"),
              s7_make_function(sc, "setup", setup, 2, 0, false,
                               "(setup width height) returns *window\n"
                               "Creates a new SDL_Window, setups opengl, inits imgui"));

    s7_define(sc, env, s7_make_symbol(sc, "prepare"),
              s7_make_function(sc, "prepare", prepare, 1, 0, false,
                               "(prepare void*) To be called before calling any ImGui draw functionality"));

    s7_define(sc, env, s7_make_symbol(sc, "flush"),
              s7_make_function(sc, "flush", flush, 1, 0, false,
                               "(flush void*) To be called after having called any ImGui draw functionality. Paints the window"));

    s7_define(sc, env, s7_make_symbol(sc, "destroy"),
              s7_make_function(sc, "destroy", destroy, 1, 0, false,
                               "(destroy *window) Destroys the window & the opengl context"));

    s7_define(sc, env, s7_make_symbol(sc, "*ns-doc*"),
              s7_make_string(sc, "Bindings to manually create an SDL_Window and draw to it with imgui. This is to use directly from a simple repl.\n"
              "ie when no (draw) function is to be called by anyone."));


    s7_define_variable(sc, "aod.c.imgui-sdl", env);
}

} // imgui_sdl
} // s7
} // aod
