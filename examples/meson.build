includes = [include_directories('.')]

includes += include_directories('../src')

if sdl2_proj.found()
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
endif


if sdl2_proj.found()
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
endif


example_sfml = executable(
  'ex_sfml',
  sources: files (
 'ex_sfml.cpp',
  ),
  link_args: ['-ldl'],
  dependencies: [
    dependency('threads'),
    sfml_system_dep,
    sfml_window_dep,
    ],
  install: true,
  gui_app: true,
  )

if sdl2_proj.found()
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
endif


example_repl = executable(
  'repl',
  sources: files (
 's7-repl.cpp',
  ),
  dependencies: [
    aod_dep,
    ],
  install: true,
)

example_curl = executable(
  'ex_curl',
  sources : files(
    'ex_curl.cpp',
  ),
  dependencies: [
    curl_dep
    ]
)


example_s7_temp = executable(
  'ex_s7_temp',
  sources : files(
    'ex_s7_temp.cpp',
  ),
  dependencies: [
    aod_dep,
    ]
)
