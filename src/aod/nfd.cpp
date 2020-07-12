#include <string>
#include <iostream>
#include <optional>
#include <nfd.h>

namespace aod {

namespace nfd {

std::optional<std::string> open_file() {
    nfdchar_t *out_path = NULL;
    nfdresult_t nfd_result = NFD_OpenDialog(NULL, NULL, &out_path);

    if (nfd_result == NFD_OKAY) {
        std::string path = out_path;
        free(out_path);
        return path;
    }

    if (nfd_result == NFD_CANCEL) {
//         puts("User pressed cancel.");
    } else {
        std::cerr << "NFD Error: " << NFD_GetError() << std::endl;
    }

    return {};
}

}
}

