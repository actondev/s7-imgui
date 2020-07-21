sources = files(
  'main.cpp',
)

# can be filled by subdirs
includes = []

includes += include_directories('.')

subdir('lib')
subdir('aod')

app_dep = declare_dependency(
  include_directories: includes,
  sources: sources,
  dependencies: [aod_dep]
)
