test_src = []

gtest_proj = subproject('gtest')
gtest_dep = gtest_proj.get_variable('gtest_dep')
gtest_main_dep = gtest_proj.get_variable('gtest_main_dep')
# gmock_dep = gtest_proj.get_variable('gmock_dep')
test_src += files(
  'aod/s7/repl_test.cpp',
  )

gtest_all = executable(
  'gtest-all',
  test_src,
  dependencies : [
    gtest_main_dep,
    aod_dep,
  ],
)
test('gtest tests', gtest_all)
