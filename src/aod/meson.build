message('in aod')

aod_sources = files(
  's7.cpp',
  's7/c_primitives.cpp',
)

aod_lib = library(
  'aod',
  # sources: aod_sources,
  include_directories: include_directories('..'),
  sources: aod_sources,
  dependencies : [
    dependency('s7', required: true),
  ]
  )

aod_dep = declare_dependency(
   # one up, doing include "aod/s7.hpp" etc
  link_with: aod_lib,
  include_directories: include_directories('..'),
  # pf.. i'm confused about the dependencies..
  # library vs declare_dependency
  dependencies : [
    # if i skip this, the tests that include s7.h throw an error
    dependency('s7', required: true),
  ]
  )
