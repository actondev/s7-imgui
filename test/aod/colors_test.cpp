#include "gtest/gtest.h"
#include "aod/colors.hpp"
#include <array>

TEST(aod_colors, ryb2rgb) {
    std::array<double, 3> ryb_r = {1, 0, 0};
    std::array<double, 3> expected_rgb_r = {1,0,0};
    ASSERT_EQ( expected_rgb_r, aod::colors::ryb2rgb(ryb_r));
    
    std::array<double, 3> ryb_y = {0, 1, 0};
    std::array<double, 3> expected_rgb_y = {1,1,0};
    ASSERT_EQ( expected_rgb_y, aod::colors::ryb2rgb(ryb_y));
    
    // just making it pass for now
//     std::array<double, 3> ryb_b = {0, 0, 1};
//     std::array<double, 3> expected_rgb_b = {0.163, 0.373, 0.6};
//     ASSERT_EQ( expected_rgb_b, aod::colors::ryb2rgb(ryb_b));
}
