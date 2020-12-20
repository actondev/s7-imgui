test_src = []

gtest_proj = subproject('gtest')
gtest_dep = gtest_proj.get_variable('gtest_dep')
gtest_main_dep = gtest_proj.get_variable('gtest_main_dep')
# gmock_dep = gtest_proj.get_variable('gmock_dep')
test_src += files(
  'aod/s7/repl_test.cpp',
  'aod/s7/foreign_primitives_test.cpp',
  'aod/s7/foreign_primitives_arr_test.cpp',
  'aod/s7/environments_test.cpp',
  'aod/s7/curl_test.cpp',
  'aod/colors_test.cpp',
  'aod/img_test.cpp',
  'aod/json_test.cpp',
  # window manager
  'aod/wm_test.cpp',
  'aod/s7/wm_test.cpp'
  # ---
  # getting to know how things work, maybe useful for later reference
  # 'aod/s7/ffi_test_raw.cpp',
)

gtest_all = executable(
  'gtest-all',
  test_src,
  dependencies : [
    gtest_main_dep,
    aod_dep,
    s7_dep,
  ],
)

gtest_valgrind = executable(
  'gtest-valgrind',
  files (
  'aod/s7/valgrind_s7_test.cpp',
  ),
  dependencies : [
    gtest_main_dep,
    aod_dep,
    s7_dep,
  ],
)

# generating a supperison file from this
# currently it's needed only for the nativefiledialog library
# gtk things I imagine (dl open etc etc)
valgrind_gen = executable(
  'valgrind-gen',
  files(
    'aod/valgrind_gen.cpp',
  ),
  dependencies : [
    aod_dep,
    s7_dep,
  ],
)

test('gtest tests', gtest_all)
