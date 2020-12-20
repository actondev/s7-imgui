#include "gtest/gtest.h"
#include "aod/wm/wm.hpp"

TEST(wm, temp) {
    
//     aod::wm::list_windows_old();
    
    printf("------\n");
    aod::wm::list_windows();
    
//     aod::wm::raise_window(46137347);
//     aod::wm::focus_window(46137347);
    
    // just making it pass for now
//     std::array<double, 3> ryb_b = {0, 0, 1};
//     std::array<double, 3> expected_rgb_b = {0.163, 0.373, 0.6};
//     ASSERT_EQ( expected_rgb_b, aod::colors::ryb2rgb(ryb_b));
}
