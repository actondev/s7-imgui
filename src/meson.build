sources = files(
  'main.cpp',
)

# can be filled by subdirs
includes = []

includes += include_directories('.')

app_dep = declare_dependency(
  include_directories: includes,
  sources: sources
)

aod_dep = declare_dependency(
  include_directories: includes,
  # for now i'm just having header only files
  sources: [],
  dependencies: [
    s7_dep,
  ],
  )
