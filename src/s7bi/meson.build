s7bi_lib = library(
  's7bi',
  include_directories: [
    include_directories('.'),
  ],
  dependencies: [
    s7_dep,
    ]
)

# TODO if enabled mongoose
subdir('mongoose')

s7bi_dep = declare_dependency(
  dependencies: [
  s7_dep,
  # TODO
  s7bi_mongoose_dep,
  ]
  )
