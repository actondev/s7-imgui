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
 'imgui.cpp',
  ),
  dependencies: [
    sdl2_dep,
    sdl2main_dep, # note: this is not needed on linux, but on windows yes
    imgui_dep,
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

# example_nng = executable(
#   'example-nng',
#   sources: files (
#     'nng-tcp-socket.c',
#   ),
#   dependencies: [
#     nng_dep,
#     ],
#   install: true,
#   )

# if system == 'linux'
# example_s7_rel = executable(
#   's7-repl',
#   sources: files (
#     's7-repl.cpp'
#   ),
#   include_directories: includes,
#   dependencies: [
#     sdl2_dep,
#     s7_dep,
#   ],
#   link_args: ['-ldl', '-lm'],
#   install: true,
#   )
# endif

# example_tcp_server = executable(
#   'tcp_server',
#   sources: files (
#     'ex_tcp_server.cpp'
#   ),
#   include_directories: includes,
#   dependencies: [
#     sdl2_dep,
#     sdl_net_dep,
#   ],
#   # link_args: ['-ldl', '-lm'],
#   install: true,
#   )
