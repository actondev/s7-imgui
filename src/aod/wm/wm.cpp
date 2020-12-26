#include "./wm.hpp"

#include <X11/Xlib.h> // TODO this is the linux impl
#include <stdio.h>
#include <X11/Xatom.h>
#include <string.h>
#include <stdlib.h>
#include <glib-2.0/glib.h>

// from xdo
#include <sys/types.h>
// #include <X11/Xlib.h>
#include <X11/X.h>

// c++ std
#include <string>
#include <list>
#include <X11/Xutil.h>
// #include <iostream>

#define MAX_PROPERTY_VALUE_LEN 4096

// #define gchar char

// window manager operations: eg list windows, focus/raise window etc
namespace aod {
namespace wm {



static char *get_property(Display *disp, Window win,  /*{{{*/
                          Atom xa_prop_type, char *prop_name, unsigned long *size) {
    Atom xa_prop_name;
    Atom xa_ret_type;
    int ret_format;
    unsigned long ret_nitems;
    unsigned long ret_bytes_after;
    unsigned long tmp_size;
    unsigned char *ret_prop;
    char *ret;

    xa_prop_name = XInternAtom(disp, prop_name, False);

    /* MAX_PROPERTY_VALUE_LEN / 4 explanation (XGetWindowProperty manpage):
     *
     * long_length = Specifies the length in 32-bit multiples of the
     *               data to be retrieved.
     */
    if (XGetWindowProperty(disp, win, xa_prop_name, 0, MAX_PROPERTY_VALUE_LEN / 4, False,
                           xa_prop_type, &xa_ret_type, &ret_format,
                           &ret_nitems, &ret_bytes_after, &ret_prop) != Success) {
//         p_verbose("Cannot get %s property.\n", prop_name);
        return NULL;
    }

    if (xa_ret_type != xa_prop_type) {
//         p_verbose("Invalid type of %s property.\n", prop_name);
        XFree(ret_prop);
        return NULL;
    }

    /* null terminate the result to make string handling easier */
    tmp_size = (ret_format / (32 / sizeof(long))) * ret_nitems;
    ret = (char*)malloc(tmp_size + 1);
    memcpy(ret, ret_prop, tmp_size);
    ret[tmp_size] = '\0';

    if (size) {
        *size = tmp_size;
    }

    XFree(ret_prop);
    return ret;
}/*}}}*/

static Window *get_client_list(Display *disp, unsigned long *size) { /*{{{*/
    Window *client_list;

    if ((client_list = (Window *)get_property(disp, DefaultRootWindow(disp),
                       XA_WINDOW, (char*)"_NET_CLIENT_LIST", size)) == NULL) {
        if ((client_list = (Window *)get_property(disp, DefaultRootWindow(disp),
                           XA_CARDINAL, (char*)"_WIN_CLIENT_LIST", size)) == NULL) {
            fputs("Cannot get client list properties. \n"
                  "(_NET_CLIENT_LIST or _WIN_CLIENT_LIST)"
                  "\n", stderr);
            return NULL;
        }
    }

    return client_list;
}/*}}}*/

static gchar *get_window_title(Display *disp, Window win) { /*{{{*/
    gchar *title_utf8;
    gchar *wm_name;
    gchar *net_wm_name;

    wm_name = get_property(disp, win, XA_STRING, (char*)"WM_NAME", NULL);
    net_wm_name = get_property(disp, win,
                               XInternAtom(disp, "UTF8_STRING", False), (char*)"_NET_WM_NAME", NULL);

    if (net_wm_name) {
        title_utf8 = strdup(net_wm_name);
    } else {
        if (wm_name) {
            title_utf8 = g_locale_to_utf8(wm_name, -1, NULL, NULL, NULL);
        } else {
            title_utf8 = NULL;
        }
    }

    free(wm_name);
    free(net_wm_name);

    return title_utf8;
}/*}}}*/

std::list<t_window> list_windows() {
    std::list<t_window> list = std::list<t_window>();
    Display *disp;
    disp = XOpenDisplay(NULL);
    if (!disp) {
        fprintf(stderr, "Cannot open display.\n");
        return list;
    }



    Window *client_list;
    unsigned long client_list_size;
    int i;
    unsigned long *desktop;

    if ((client_list = get_client_list(disp, &client_list_size)) == NULL) {
        // error
        return list;
    }

    /* print the list */
    for (i = 0; i < client_list_size / sizeof(Window); i++) {
        /* desktop ID */
        Window window = client_list[i];
        if ((desktop = (unsigned long *)get_property(disp, window,
                       XA_CARDINAL, "_NET_WM_DESKTOP", NULL)) == NULL) {
            desktop = (unsigned long *)get_property(disp, window,
                                                    XA_CARDINAL, "_WIN_WORKSPACE", NULL);
        }

        // gchar is just char? what about the utf8?
        char *title_utf8 = get_window_title(disp, window); /* UTF8 */
        std::string title = title_utf8;
        g_free(title_utf8);

        if ((signed long)*desktop == -1) {
            // SKIPPING
            // here would be xfce4-panel, Dekstop for example
            printf("SKIPPING desktop %2ld, windows %ld ,title %s\n", (signed long)*desktop, client_list[i], title.c_str());
            continue;
        }

        XWindowAttributes attr;
        XClassHint classhint;
        XGetWindowAttributes(disp, window, &attr);

        std::string class_name;

        if (XGetClassHint(disp, window, &classhint)) {
            class_name = classhint.res_class;
            XFree(classhint.res_name);
            XFree(classhint.res_class);
        }

        t_window win = {
            .title = title,
            .class_name = class_name,
            .handle = client_list[i],
        };

        list.push_back(win);
//         printf("desktop %2ld, windows %ld ,title %s\n", (signed long)*desktop, client_list[i], title_utf8);

    }
    g_free(client_list);

    return list;
}

void focus_window(unsigned long window) {
    Display *disp;
    disp = XOpenDisplay(NULL);
    if (!disp) {
        fprintf(stderr, "Cannot open display.\n");
        return;
    }

    int ret = 0;
    ret = XRaiseWindow(disp, window);
    XFlush(disp);
}

void raise_window(unsigned long window) {
    Display *disp;
    disp = XOpenDisplay(NULL);
    if (!disp) {
        fprintf(stderr, "Cannot open display.\n");
        return;
    }

    int ret = 0;
    ret = XSetInputFocus(disp, window, RevertToParent, CurrentTime);
    XFlush(disp);
}

// see also https://github.com/dancor/wmctrl/blob/master/main.c#L1323

// int list_windows_old() {
//     Display *disp;
//     disp = XOpenDisplay(NULL);
//     if (!disp) {
//         fprintf(stderr, "Cannot open display.\n");
//         return EXIT_FAILURE;
//     }
//
//     Window *client_list;
//     unsigned long client_list_size;
//     int i;
//     int max_client_machine_len = 0;
//
//     if ((client_list = get_client_list(disp, &client_list_size)) == NULL) {
//         return EXIT_FAILURE;
//     }
//
//     /* find the longest client_machine name */
//     for (i = 0; i < client_list_size / sizeof(Window); i++) {
//         char *client_machine;
//         if ((client_machine = get_property(disp, client_list[i],
//                                            XA_STRING, (char*)"WM_CLIENT_MACHINE", NULL))) {
//             max_client_machine_len = strlen(client_machine);
//         }
//         free(client_machine);
//     }
//
//     /* print the list */
//     for (i = 0; i < client_list_size / sizeof(Window); i++) {
//         gchar is just char? what about the utf8?
//         gchar *title_utf8 = get_window_title(disp, client_list[i]); /* UTF8 */
//         gchar *client_machine;
//         unsigned long *pid;
//         unsigned long *desktop;
//         int x, y, junkx, junky;
//         unsigned int wwidth, wheight, bw, depth;
//         Window junkroot;
//
//         /* desktop ID */
//         if ((desktop = (unsigned long *)get_property(disp, client_list[i],
//                        XA_CARDINAL, "_NET_WM_DESKTOP", NULL)) == NULL) {
//             desktop = (unsigned long *)get_property(disp, client_list[i],
//                                                     XA_CARDINAL, "_WIN_WORKSPACE", NULL);
//         }
//
//         /* client machine */
//         client_machine = get_property(disp, client_list[i],
//                                       XA_STRING, "WM_CLIENT_MACHINE", NULL);
//
//         /* pid */
//         pid = (unsigned long *)get_property(disp, client_list[i],
//                                             XA_CARDINAL, "_NET_WM_PID", NULL);
//
//         /* geometry */
//         XGetGeometry(disp, client_list[i], &junkroot, &junkx, &junky,
//                      &wwidth, &wheight, &bw, &depth);
//         XTranslateCoordinates(disp, client_list[i], junkroot, junkx, junky,
//                               &x, &y, &junkroot);
//         printf("title %s\n", title_utf8);
//
//         /* special desktop ID -1 means "all desktops", so we
//            have to convert the desktop value to signed long */
//         printf("0x%.8lx %2ld", client_list[i],
//                desktop ? (signed long)*desktop : 0);
//
//
//
//         g_free(title_utf8);
//         g_free(desktop);
//         g_free(client_machine);
//         g_free(pid);
//     }
//     g_free(client_list);
//
//
// }

} // !wm
} // !aod
