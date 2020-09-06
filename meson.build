project('s7-imgui', 'cpp', 'c',
	default_options: ['default_library=static',
			  'cpp_std=c++17',
			 ],
       )

extra_args = []
system = host_machine.system()
compiler_id = meson.get_compiler('cpp').get_id()
if compiler_id == 'msvc'
  message('visual studio')
  # these flags are copied from an example project, don't know what's what
  # should comment later on what's what
  extra_args += ['/wd"4996"', '/wd"4250"', '/wd"4018"', '/wd"4267"', '/wd"4068"', '-D_DEBUG']
endif

if system == 'windows' and compiler_id != 'msvc'
  error('Building on windows requires the MSVC compiler. Other compilers are not supported at the moment.')
endif

link_args = []

# host or build machine?

# --- dependencies
s7_proj = subproject('s7')
s7_dep = s7_proj.get_variable('s7_dep')

sdl2_proj = subproject('sdl2')
sdl2_dep = sdl2_proj.get_variable('sdl2_dep')
sdl2main_dep = sdl2_proj.get_variable('sdl2main_dep')

# imgui looks for sdl
# https://mesonbuild.com/Reference-manual.html#dependency
meson.override_dependency('sdl', sdl2_dep)

imgui = subproject('imgui',
		   default_options: ['sdl=enabled',
				     'opengl2=enabled',
				    ])
imgui_dep = imgui.get_variable('imgui_dep')

sdl_net_proj = subproject('sdl_net')
sdl_net_dep = sdl_net_proj.get_variable('sdl_net_dep')

nfd_proj = subproject('nfd')
nfd_dep = nfd_proj.get_variable('nfd_dep')

# --- application
meson.override_dependency('s7', s7_dep)
meson.override_dependency('imgui', imgui_dep)

cmake = import('cmake')
sfml_opt = cmake.subproject_options()
# Call CMake with `-DSOME_OTHER_VAR=ON`
# skipping audio for now
sfml_opt.add_cmake_defines({'SFML_BUILD_AUDIO': false})

sfml_proj = cmake.subproject('SFML', options: sfml_opt)
# Fetch the dependency object
sfml_system_dep = sfml_proj.dependency('sfml-system')
sfml_graphics_dep = sfml_proj.dependency('sfml-graphics')
sfml_window_dep = sfml_proj.dependency('sfml-window')

subdir('src')

if not meson.is_subproject()
  # hm probably remove the app
  app = executable(
    's7-imgui',
    dependencies : [
      sdl2_dep, # the core library
      sdl2main_dep, # abstracts out the main thing: windows needs WinMain
      sdl_net_dep,
      imgui_dep,
      s7_dep,
      app_dep,
    ],
    link_args: link_args,
    cpp_args: extra_args,
    gui_app: true, # needed on windows (int __cdecl invoke_main) : in VS /SUBSYSTEM windows
    install: true,
  )

  gui_repl = executable(
    'gui_repl',
    sources: files(
      'src/gui_repl.cpp',
    ),
    dependencies : [
      sdl2_dep, # the core library
      sdl2main_dep, # abstracts out the main thing: windows needs WinMain
      sdl_net_dep,
      imgui_dep,
      s7_dep,
      aod_dep,
    ],
    link_args: link_args,
    cpp_args: extra_args,
    gui_app: true, # needed on windows (int __cdecl invoke_main) : in VS /SUBSYSTEM windows
    install: true,
  )

  gui_repl_sfml = executable(
    'gui_repl_sfml',
    sources: files(
      'src/gui_repl_sfml.cpp',
    ),
    dependencies : [
      imgui_dep,
      s7_dep,
      aod_dep,
      # sfml
      sfml_system_dep,
    sfml_graphics_dep,
    sfml_window_dep,
    ],
    link_args: link_args,
    cpp_args: extra_args,
    gui_app: true, # needed on windows (int __cdecl invoke_main) : in VS /SUBSYSTEM windows
    install: true,
  )
  
  repl = executable(
    'repl',
    sources: files('src/repl.cpp'),
    dependencies : [
      sdl2_dep, # the core library
      sdl_net_dep,
      imgui_dep,
      s7_dep,
      aod_dep,
    ],
    link_args: link_args,
    cpp_args: extra_args,
    install: true,
  )

  # TODO switch building the examples with a definition option
  subdir('examples')
  # TODO not build test if building for release
  subdir('test')
else # we are in subproject => exposing the s7_imgui_dep
  s7_imgui_dep = declare_dependency(
    dependencies: [
    sdl2_dep,
    sdl_net_dep,
    imgui_dep,
    s7_dep,
    aod_dep,
  ]
  )
endif
