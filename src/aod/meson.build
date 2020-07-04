# TODO as this grows, I should add conditionals:
# if s7 found, add these files
# if imgui found, ... and so on
aod_sources = files(
  's7.cpp',
  's7/imgui.cpp',
  's7/imgui_addons.cpp',
  's7/repl.cpp',
  'imgui/addons.cpp',

  # foreign types
  # 's7/c_primitives.cpp',
  's7/foreign_primitives.cpp',
  's7/foreign_primitives_arr.cpp',
)

aod_lib = library(
  'aod',
  # sources: aod_sources,
  include_directories: include_directories('..'),
  sources: aod_sources,
  dependencies : [
    dependency('s7', required: true),
    dependency('imgui', required: true),
  ]
  )

aod_dep = declare_dependency(
  link_with: aod_lib,
  # include from parent dir: to include "aod/s7.cpp" for example
  include_directories: include_directories('..'),
  )
