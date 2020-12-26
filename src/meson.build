sources = files(
  'main.cpp',
)

# can be filled by subdirs
includes = []

includes += include_directories('.')

subdir('lib')
subdir('aod')
subdir('s7bi')

app_dep = declare_dependency(
  include_directories: includes,
  sources: sources,
  dependencies: [aod_dep]
)
if not meson.is_subproject()

  #   gui_repl_sdl = executable(
  #     'gui_repl_sdl',
  #     sources: files(
  # 	'src/gui_repl.cpp',
  #     ),
  #     dependencies : [
  # 	sdl2_dep, # the core library
  # 	sdl2main_dep, # abstracts out the main thing: windows needs WinMain
  # 	sdl_net_dep,
  # 	imgui_dep,
  # 	s7_dep,
  # 	aod_dep,
  #     ],
  #     link_args: link_args,
  #     cpp_args: extra_args,
  #     gui_app: true, # needed on windows (int __cdecl invoke_main) : in VS /SUBSYSTEM windows
  #     install: true,
  #   )
  # endif

  gui_repl_sfml = executable(
    'gui_repl_sfml',
    sources: files(
      'gui_repl_sfml.cpp',
    ),
    dependencies : [
      imgui_dep,
      s7_dep,
      aod_dep,
    ],
    link_args: link_args,
    cpp_args: extra_args,
    gui_app: true, # needed on windows (int __cdecl invoke_main) : in VS /SUBSYSTEM windows
    install: true,
  )

  gui_repl_gflw = executable(
    'gui_repl_gflw',
    sources: files(
      'gui_repl_gflw.cpp',
    ),
    dependencies : [
      imgui_dep,
      s7_dep,
      aod_dep,
      gflw_dep,
    ],
    link_args: link_args,
    cpp_args: extra_args,
    gui_app: true, # needed on windows (int __cdecl invoke_main) : in VS /SUBSYSTEM windows
    install: true,
  )


  repl = executable(
    'repl',
    sources: files('repl.cpp'),
    dependencies: [
      aod_dep,
    ],
    install: true,
  )
else # we are in subproject => exposing the s7_imgui_dep
  s7_imgui_dep = declare_dependency(
    dependencies: [
    imgui_dep,
    s7_dep,
    aod_dep,
  ]
  )
endif
