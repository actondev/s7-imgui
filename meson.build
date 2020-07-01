project('s7-imgui', 'cpp', 'c',
	default_options: [
	  'default_library=static',
	]
       )

extra_args = []
if meson.get_compiler('cpp').get_id() == 'msvc'
  message('visual studio')
  # examples flags from the provided example
  extra_args += ['/wd"4996"', '/wd"4250"', '/wd"4018"', '/wd"4267"', '/wd"4068"', '-D_DEBUG']
endif

link_args = []

system = host_machine.system()
if system == 'windows'
  # just copied that from the provided examples
  # link_args += '/DYNAMICBASE "dsound.lib" "winmm.lib" "wininet.lib" "comctl32.lib" "Shlwapi.lib" "kernel32.lib" "user32.lib" "gdi32.lib" "winspool.lib" "comdlg32.lib" "advapi32.lib" "shell32.lib" "ole32.lib" "oleaut32.lib" "uuid.lib" "odbc32.lib" "odbccp32.lib"'
  # link_args += '/TLBID:1'
elif system == 'linux'
  link_args += [
    # '-lpthread',
    # '-lxcb',
    # '-ldl',
    # '-lGL',
    # '-lfontconfig',
    # '-lGLU',
    # '-lpulse',
    # '-lpulse-simple',
	       ]
  # added -lGL in sdl2 project
  # add_global_link_arguments('-lGL', language: 'cpp')
  # extra_args += ['-fpermissive']
endif

s7_proj = subproject('s7')
s7_dep = s7_proj.get_variable('s7_dep')
# src uses s7
meson.override_dependency('s7', s7_dep)
subdir('src')

sdl2_proj = subproject('sdl2')
sdl2_dep = sdl2_proj.get_variable('sdl2_dep')
sdl2main_dep = sdl2_proj.get_variable('sdl2main_dep')

# imgui searched for sdl
# https://mesonbuild.com/Reference-manual.html#dependency
meson.override_dependency('sdl', sdl2_dep)

imgui = subproject('imgui',
		   default_options: ['sdl=enabled',
				     'opengl2=enabled',
				    ])
imgui_dep = imgui.get_variable('imgui_dep')

sdl_net_proj = subproject('sdl_net')
sdl_net_dep = sdl_net_proj.get_variable('sdl_net_dep')

app = executable(
  's7-imgui',
  dependencies : [
    sdl2_dep, # the main library
    sdl2main_dep, # abstracts out the main thing: windos needs WinMain
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

# TODO switch building the examples with a definition option
subdir('examples')
subdir('test')
