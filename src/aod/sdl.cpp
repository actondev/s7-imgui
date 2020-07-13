#include "./sdl.hpp"
#include "SDL_video.h"

#ifdef __linux__
#include "SDL_syswm.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#endif

// HACK AHEAD

/**
 * The problem was the SDL_CreateWindowFrom and the opengl flags.
 * In windows, the following snippet is the solution
 *
 *
 *
 SDL_Window *dummyWin = SDL_CreateWindow("", 0, 0, 1, 1,
 SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);

 char sBuf[32];
 sprintf(sBuf, "%p", dummyWin);
 printf("dummy window %p\n", dummyWin);

 printf("Setting hint SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT to %s\n",
 sBuf);
 SDL_SetHint(SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT, sBuf);

 const char *hint = SDL_GetHint(SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT);

 SDL_Window *window = SDL_CreateWindowFrom(that->pParent);
 SDL_SetHint(SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT, nullptr);
 *
 *
 *
 * While on Windows that works, in Linux I had to use a fork of mine from SDL.
 *
 * So, I realized, it could be better to just be able to modify the flags of a window myself.
 * For that, the definition of SDL_Window is needed
 *
 */

/** \brief A union containing parameters for shaped windows. */
typedef union {
    /** \brief a cutoff alpha value for binarization of the window shape's alpha channel. */
    Uint8 binarizationCutoff;
    SDL_Color colorKey;
} SDL_WindowShapeParams;

/** \brief An enum denoting the specific type of contents present in an SDL_WindowShapeParams union. */
typedef enum {
    /** \brief The default mode, a binarized alpha cutoff of 1. */
    ShapeModeDefault,
    /** \brief A binarized alpha cutoff with a given integer value. */
    ShapeModeBinarizeAlpha,
    /** \brief A binarized alpha cutoff with a given integer value, but with the opposite comparison. */
    ShapeModeReverseBinarizeAlpha,
    /** \brief A color key is applied. */
    ShapeModeColorKey
} WindowShapeMode;

typedef struct SDL_WindowShapeMode {
    /** \brief The mode of these window-shape parameters. */
    WindowShapeMode mode;
    /** \brief Window-shape parameters. */
    SDL_WindowShapeParams parameters;
} SDL_WindowShapeMode;

/* Define the SDL window-shaper structure */
struct SDL_WindowShaper {
    /* The window associated with the shaper */
    SDL_Window *window;

    /* The user's specified coordinates for the window, for once we give it a shape. */
    Uint32 userx, usery;

    /* The parameters for shape calculation. */
    SDL_WindowShapeMode mode;

    /* Has this window been assigned a shape? */
    SDL_bool hasshape;

    void *driverdata;
};

typedef struct SDL_WindowUserData {
    char *name;
    void *data;
    struct SDL_WindowUserData *next;
} SDL_WindowUserData;

struct SDL_Window {
    const void *magic;
    Uint32 id;
    char *title;
    SDL_Surface *icon;
    int x, y;
    int w, h;
    int min_w, min_h;
    int max_w, max_h;
    Uint32 flags;
    Uint32 last_fullscreen_flags;

    /* Stored position and size for windowed mode */
    SDL_Rect windowed;

    SDL_DisplayMode fullscreen_mode;

    float opacity;

    float brightness;
    Uint16 *gamma;
    Uint16 *saved_gamma; /* (just offset into gamma) */

    SDL_Surface *surface;
    SDL_bool surface_valid;

    SDL_bool is_hiding;
    SDL_bool is_destroying;
    SDL_bool is_dropping; /* drag/drop in progress, expecting SDL_SendDropComplete(). */

    SDL_WindowShaper *shaper;

    SDL_HitTest hit_test;
    void *hit_test_data;

    SDL_WindowUserData *data;

    void *driverdata;

    SDL_Window *prev;
    SDL_Window *next;
};

namespace aod {
namespace sdl {

#ifdef __linux__
namespace x11 {
void fix_input(SDL_Window *window) {
    SDL_SysWMinfo info;
    SDL_VERSION(&info.version); /* initialize info structure with SDL version info */
    if (!SDL_GetWindowWMInfo(window, &info)) {
        fprintf(stderr,
                "Could not get WM info. X11 input fix could not be applied!\n");
        return;
    }
    Display *display = info.info.x11.display;
    Window w = info.info.x11.window;
    XSelectInput(display, w,
            (FocusChangeMask | EnterWindowMask | LeaveWindowMask |
            ExposureMask | ButtonPressMask | ButtonReleaseMask |
            PointerMotionMask | KeyPressMask | KeyReleaseMask |
            PropertyChangeMask | StructureNotifyMask |
            KeymapStateMask));
}
#endif
} // x11

// not working. destroying the opengl window and then creating the other one throws an error
//SDL_Window* create_window_from(void *pParent, SDL_WindowFlags window_flags) {
//    if (window_flags & SDL_WINDOW_OPENGL) {
//        SDL_Window* dummy = SDL_CreateWindow("", 0, 0, 1, 1,
//                SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
//        SDL_DestroyWindow(dummy);
//    }
//    SDL_Window* window = SDL_CreateWindowFrom(pParent);
////    window->flags = window_flags; // let's see how to solve this
//    window->flags |= window_flags;
//
//#ifdef __linux__
//    x11::fix_input(window);
//#endif
//
//    return window;
//}

embedded_window embed_window(void *pParent, SDL_WindowFlags window_flags) {
    embedded_window emb;
    if (window_flags & SDL_WINDOW_OPENGL) {
        emb.dummy = SDL_CreateWindow("", 0, 0, 1, 1,
                SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
    } else {
        emb.dummy = nullptr;
    }
    emb.window = SDL_CreateWindowFrom(pParent);
//    window->flags = window_flags; // let's see how to solve this
    emb.window->flags |= window_flags;

#ifdef __linux__
    x11::fix_input(emb.window);
#endif

    return emb;
}

void destroy_embedded(embedded_window emb) {
    if (emb.dummy != nullptr) {
        SDL_DestroyWindow(emb.dummy);
    }
    SDL_DestroyWindow(emb.window);
}

// Note: the original (working in Windows, not in linux) way to embed with opengl, is the following:

//    SDL_Window *dummyWin = SDL_CreateWindow("", 0, 0, 1, 1,
//            SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);

//     char sBuf[32];
//     sprintf(sBuf, "%p", dummyWin);
//     printf("dummy window %p\n", dummyWin);
//
//     printf("Setting hint SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT to %s\n",
//     sBuf);
//     SDL_SetHint(SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT, sBuf);
//
//     const char *hint = SDL_GetHint(SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT);
//
//     SDL_Window *window = SDL_CreateWindowFrom(that->pParent);
//     SDL_SetHint(SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT, nullptr);
//
//     int flags = SDL_GetWindowFlags(window);
//     printf("old way window flags? %d\n", flags);
//     printf("window has opengl flag? %s\n",
//     (flags & SDL_WINDOW_OPENGL) ? "true" : "false");

}// sdl
} // aod
