// uuughhh sfml has stb 

#define STB_IMAGE_WRITE_IMPLEMENTATION
// we have it in this directory for now
// will see later on if I need this in more places
#include "stb_image_write.h"
#include <cstring>
#include <cstdlib>
#ifdef _WIN32
// uppercase Windows.h not found with mingw in linux??
#include <windows.h> // gl.h errors if not (void should be preceded.. etc)
#endif
#include <GL/gl.h>
#include "./gl.hpp"

namespace aod {
namespace gl {

void flipVertically(int width, int height, char *data) {
    char rgb[3];

    for (int y = 0; y < height / 2; ++y) {
        for (int x = 0; x < width; ++x) {
            int top = (x + y * width) * 3;
            int bottom = (x + (height - y - 1) * width) * 3;

            memcpy(rgb, data + top, sizeof(rgb));
            memcpy(data + top, data + bottom, sizeof(rgb));
            memcpy(data + bottom, rgb, sizeof(rgb));
        }
    }
}

// credit: https://github.com/vallentin/GLCollection/blob/master/screenshot.cpp
int save_screenshot(const char *filename) {
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);

    int x = viewport[0];
    int y = viewport[1];
    int width = viewport[2];
    int height = viewport[3];

    char *data = (char*) malloc((size_t) (width * height * 3)); // 3 components (R, G, B)

    if (!data)
        return 0;

    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glReadPixels(x, y, width, height, GL_RGB, GL_UNSIGNED_BYTE, data);

    flipVertically(width, height, data);

    int saved = stbi_write_png(filename, width, height, 3, data, 0);

    free(data);

    return saved;
}
}
}
