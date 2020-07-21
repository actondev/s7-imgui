# TODO as this grows, I should add conditionals:
# if s7 found, add these files
# if imgui found, ... and so on
aod_sources = files(
  's7.cpp',
  's7/imgui.cpp',
  's7/imgui_addons.cpp',
  's7/repl.cpp',
  'imgui/addons.cpp',
  's7/sdl.cpp',
  'sdl.cpp',
  'sdl/audio.cpp',
  'nfd.cpp',
  's7/nfd.cpp',
  's7/imgui_sdl.cpp',
  'colors.cpp',
  's7/colors.cpp',

  # foreign types
  's7/foreign_primitives.cpp',
  's7/foreign_primitives_arr.cpp',

  # img (testing for similarity)
  'img/core.cpp',
  's7/img.cpp',
)

gl_dep = dependency('gl', required : false)
if gl_dep.found()
  message('aod: gl_dep found, adding gl/gl.cpp')
  aod_sources += files(
    'gl/gl.cpp', # aod::gl::save_screenshot(const char* filename)
    's7/gl.cpp', # (gl/save-screenshot filename)
    )
endif

aod_lib = library(
  'aod',
  # sources: aod_sources,
  include_directories: include_directories('..'),
  sources: aod_sources,
  dependencies : [
    # dependency('s7', required: true),
    s7_dep,
    imgui_dep,
    gl_dep,
    sdl2_dep,
    nfd_dep,
    stb_dep, # (only for image read/write for now)
  ]
  )

aod_dep = declare_dependency(
  link_with: aod_lib,
  # include from parent dir: to include "aod/s7.cpp" for example
  include_directories: include_directories('..'),
  dependencies: [
    # hm.. this is for the include_directories
    # solving errors like "SDL.h" not found etc
    s7_dep,
    imgui_dep,
    gl_dep,
    sdl2_dep,
    nfd_dep,
    ]
  )
