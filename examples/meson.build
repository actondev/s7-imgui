includes = [include_directories('.')]

includes += include_directories('../src')

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

example_imgui_sfml = executable(
    'ex-imgui-sfml',
    sources: files ('ex-imgui-sfml.cpp'),
    dependencies: [
    aod_dep,
    ],
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
