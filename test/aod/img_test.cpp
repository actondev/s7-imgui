#include "gtest/gtest.h"
#include "aod/img/core.hpp"

#include <filesystem>
#include <iostream>
namespace fs = std::filesystem;
using std::cout, std::cerr, std::endl;


TEST(aod_img, equivalent) {
//     fs::path base_path = fs::current_path();
//     cout << "base path " << base_path << endl;
    ASSERT_TRUE(aod::img::are_equivalent("../../test/scheme/assets/sxs-wheel-snapshot.png", "../../test/scheme/assets/sxs-wheel-snapshot.png"));;
    ASSERT_FALSE(aod::img::are_equivalent("../../test/scheme/assets/sxs-wheel-snapshot.png", "../../test/scheme/assets/sxs-wheel-offset.png"));;
    ASSERT_FALSE(aod::img::are_equivalent("../../test/scheme/assets/sxs-wheel-offset.png", "../../test/scheme/assets/sxs-wheel-snapshot.png"));;
}

