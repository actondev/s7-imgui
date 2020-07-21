message('here, stb')
stb_lib = library(
  'stb',
  sources: files('stb/stb.c')
)
stb_dep = declare_dependency(
  link_with: stb_lib,
  include_directories: include_directories('stb'),
)
