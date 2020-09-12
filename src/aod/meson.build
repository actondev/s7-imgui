# TODO as this grows, I should add conditionals:
# if s7 found, add these files
# if imgui found, ... and so on
aod_sources = files(
  # TODO move things in subfolders
  # eg s7/s7.cpp, s7/s7.hpp for the "core" functionality
  's7.cpp',
  's7/imgui/imgui.cpp',
  's7/imgui/addons.cpp',
  's7/imgui/enums.cpp',
  's7/repl.cpp',
  # imgui
  'imgui/addons.cpp',
  'imgui/sfml.cpp',
  # sdl
#   's7/sdl.cpp',
#   'sdl.cpp',
#   'sdl/audio.cpp',
# 's7/imgui_sdl.cpp',

  'nfd.cpp',
  's7/nfd.cpp',
  'colors.cpp',
  's7/colors.cpp',
  's7/string.cpp',

  # foreign types
  's7/foreign_primitives.cpp',
  's7/foreign_primitives_arr.cpp',

  # img (testing for similarity)
  # TODO rename to img/img.cpp
  'img/core.cpp',
  's7/img.cpp',
  
  # midi
  'midi/midi.cpp',
  's7/midi.cpp',

  # net
#   'net/net_impl_sdl.cpp',
  'net/net_impl_sfml.cpp',
)

gl_dep = dependency('gl', required : false)
if gl_dep.found()
  message('aod: gl_dep found, adding gl/gl.cpp')
  aod_sources += files(
    'gl/gl.cpp', # aod::gl::save_screenshot(const char* filename)
    's7/gl.cpp', # (gl/save-screenshot filename)
    )
endif

aod_link_args = []
if system == 'windows'
  aod_link_args = ['Ws2_32.lib'] # for SFML network
endif
aod_lib = library(
  'aod',
  # sources: aod_sources,
  include_directories: include_directories('..'),
  sources: aod_sources,
  # link_args: ['-ldl'],
  link_args: aod_link_args,
  # pic: true,
  dependencies : [
    # dependency('s7', required: true),
    s7_dep,
    imgui_dep,
    gl_dep,
    nfd_dep,
    # ugh.. sfml has the stb in it.. so.. multiple definitions
    # the problem is cause I want to statically link SFML
    stb_dep, # (only for image read/write for now)
    sfml_dep,
  ]
  )

aod_includes = include_directories('..')
# aod_includes += sfml_includes

aod_dep = declare_dependency(
  link_with: aod_lib,
  link_args: aod_link_args,
  # include from parent dir: to include "aod/s7.hpp" for example
  include_directories: [aod_includes, sfml_includes],
  dependencies: [
    # hm.. this is for the include_directories
    # solving errors like "SDL.h" not found etcF
    s7_dep,
    imgui_dep,
    gl_dep,
    nfd_dep,
    sfml_dep,
    ]
  )
