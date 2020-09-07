includes = [include_directories('.')]

includes += include_directories('../src')

example_sdl_opengl = executable(
  'example-sdl-opengl',
  sources: files (
    'sdl_opengl.c',
  ),
  dependencies: [
    sdl2_dep,
    sdl2main_dep,
  ],
  install: true,
  gui_app: true,
  )


example_imgui = executable(
  'example_imgui',
  sources: files (
 'ex_imgui.cpp',
  ),
  dependencies: [
    sdl2_dep,
    sdl2main_dep, # note: this is not needed on linux, but on windows yes
    imgui_dep,
    ],
  install: true,
  gui_app: true,
)


example_sfml = executable(
  'ex_sfml',
  sources: files (
 'ex_sfml.cpp',
  ),
  link_args: ['-ldl'],
  dependencies: [
    dependency('threads'),
    sfml_system_dep,
    sfml_graphics_dep,
    sfml_window_dep,
    ],
  install: true,
  gui_app: true,
  )

example_sdl_net = executable(
  'example-sdl-net',
  sources: files (
    'sdl-net-tcp-socket.c',
  ),
  dependencies: [
    sdl2_dep,
    sdl2main_dep,
    sdl_net_dep,
    ],
  install: true,
)

# example_audio_player = executable(
#   'ex_audio_player',
#   sources: files (
#     'ex_audio_player.cpp',
#   ),
#   dependencies: [
#     sdl2_dep,
#     sdl2main_dep,
#     sdl_net_dep,
#     imgui_dep,
#     aod_dep,
#     s7_dep,
#     ],
#   install: true,
#   )


example_repl = executable(
  'repl',
  sources: files (
 's7-repl.cpp',
  ),
  dependencies: [
    sdl2_dep,
    aod_dep,
    ],
  install: true,
  )
