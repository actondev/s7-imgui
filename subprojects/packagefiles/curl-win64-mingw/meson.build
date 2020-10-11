project('curl-win64-mingw', 'c')

cc = meson.get_compiler('c')
message('here: ' + meson.current_source_dir())
libcurl_dep = declare_dependency(
  dependencies : cc.find_library('libcurl', dirs : meson.current_source_dir() + '/lib'),
  include_directories : include_directories('include'))
