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

link_args = []

# host or build machine?

# --- dependencies
s7_proj = subproject('s7')
s7_dep = s7_proj.get_variable('s7_dep')

mongoose_proj = subproject('mongoose')
mongoose_dep = mongoose_proj.get_variable('mongoose_dep')

# sdl_proj = subproject('sdl')
# sdl_dep = mongoose_proj.get_variable('sdl_dep')

sdl2_proj = subproject('sdl2', required: false)
if sdl2_proj.found()
  sdl2_dep = sdl2_proj.get_variable('sdl2_dep')
  sdl2main_dep = sdl2_proj.get_variable('sdl2main_dep')
  # imgui looks for sdl
  # https://mesonbuild.com/Reference-manual.html#dependency
  meson.override_dependency('sdl', sdl2_dep)
endif


imgui_opts = ['opengl2=enabled', 'sdl=enabled', 'gflw=enabled']

imgui = subproject('imgui',
		   default_options: imgui_opts)
imgui_dep = imgui.get_variable('imgui_dep')

nfd_proj = subproject('nfd')
nfd_dep = nfd_proj.get_variable('nfd_dep')

nlohmann_json_dep = subproject('nlohmann_json').get_variable('nlohmann_json_dep')

# --- application
meson.override_dependency('s7', s7_dep)
meson.override_dependency('imgui', imgui_dep)

cmake = import('cmake')
sfml_opt = cmake.subproject_options()

# audio:
# ||  Could NOT find VORBIS (missing: VORBIS_LIBRARIES VORBIS_INCLUDE_DIR
# ||  OGG_INCLUDE_DIR)
sfml_opt.add_cmake_defines({'SFML_BUILD_AUDIO': false,
			    'SFML_BUILD_GRAPHICS': false,
			    # 'SFML_STATIC_LIBRARIES': true,
			    'BUILD_SHARED_LIBS': false,})
sfml_opt.append_compile_args('cpp', '-fPIC')
# sfml_opt.append_link_args('-ldl', '-lz', '-lrt')

# TODO ad SFML.wrap
# TODO remove when meson has cmake support?
if meson.is_cross_build() and system == 'windows'
  # https://github.com/mesonbuild/meson/issues/7105
  message('TODO cross build & cmake!')
  # sfml_proj = cmake.subproject('SFML', options: sfml_opt,
  # cmake_options: '-DCMAKE_TOOLCHAIN_FILE=mingw.cmake')
  sfml_opt.add_cmake_defines({
				# 'CMAKE_TOOLCHAIN_FILE': '/home/actondev/dev/actondev/s7-reaper-repl/subprojects/s7_imgui/mingw.cmake',
				'TARGET_ARCH': 'x86_64-w64-mingw32',
				'CMAKE_SYSTEM_NAME': 'Windows',
				'CMAKE_C_COMPILER': 'x86_64-w64-mingw32-gcc',
				'CMAKE_CXX_COMPILER': 'x86_64-w64-mingw32-g++',
				'CMAKE_FIND_ROOT_PATH': '/usr/x86_64-w64-mingw32',
				'CMAKE_C_COMPILER_WORKS': 1,
				'CMAKE_CROSSCOMPILING': 1,
				'CMAKE_CXX_COMPILER_WORKS': 1,
				
			      })
endif

sfml_proj = cmake.subproject('SFML', options: sfml_opt)
# Fetch the dependency object
sfml_system_dep = sfml_proj.dependency('sfml-system')
# sfml_graphics_dep = sfml_proj.dependency('sfml-graphics')
sfml_window_dep = sfml_proj.dependency('sfml-window')
sfml_network_dep = sfml_proj.dependency('sfml-network')
# sfml_audio_dep = sfml_proj.dependency('sfml-audio')

sfml_dep = [sfml_system_dep,
	    # we don't need the graphics
	    # and we save ~4mb :)
	    # sfml_graphics_dep,
	    sfml_window_dep,
	    sfml_network_dep,
	    # sfml_audio_dep,
	   ]


# curl

curl_opt = cmake.subproject_options()
if meson.is_cross_build() and system == 'windows'
  curl_opt.add_cmake_defines({
				# 'CMAKE_TOOLCHAIN_FILE': '/home/actondev/dev/actondev/s7-reaper-repl/subprojects/s7_imgui/mingw.cmake',
				'TARGET_ARCH': 'x86_64-w64-mingw32',
				'CMAKE_SYSTEM_NAME': 'Windows',
				'CMAKE_C_COMPILER': 'x86_64-w64-mingw32-gcc',
				'CMAKE_CXX_COMPILER': 'x86_64-w64-mingw32-g++',
				'CMAKE_FIND_ROOT_PATH': '/usr/x86_64-w64-mingw32',
				'CMAKE_C_COMPILER_WORKS': 1,
				'CMAKE_CROSSCOMPILING': 1,
				'CMAKE_CXX_COMPILER_WORKS': 1,
				'CMAKE_BUILD_WITH_INSTALL_RPATH': 1,
				
			      })
endif

if system == 'windows' and compiler_id == 'gcc'
  # using prebuilt binaries
  # https://mesonbuild.com/Shipping-prebuilt-binaries-as-wraps.html

  # TODO architecture: win32 ?
  curl_proj = subproject('curl-win64-mingw')
  curl_dep = curl_proj.get_variable('libcurl_dep')

  # we just need the dlls from here, to copy them later on next to the .exe
  # libcrypto-1_1-x64.dll
  # libssl-1_1-x64.dll
  openssll_proj = subproject('openssl-win64-mingw')
elif system == 'linux'
  curl_proj = cmake.subproject('curl', options: curl_opt)
  curl_dep = curl_proj.dependency('libcurl')
endif


# gflw
gflw_opt = cmake.subproject_options()
gflw_proj = cmake.subproject('gflw', options: gflw_opt)

message('CMaket targets:\n - ' + '\n - '.join(gflw_proj.target_list()))
gflw_dep = gflw_proj.dependency('glfw')

subdir('src')



if not meson.is_subproject()
  # TODO switch building the examples with a definition option
  subdir('examples')
  # TODO not build test if building for release
  subdir('test')
endif
