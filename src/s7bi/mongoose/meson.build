s7bi_mongoose_lib = library(
  's7bi_mongoose',
  sources: [
    files('s7bi_mongoose.cpp')
  ],
  dependencies: [
    s7_dep,
    mongoose_dep
  ]
)
s7bi_mongoose_dep = declare_dependency(
  link_with: s7bi_mongoose_lib,
  dependencies: [
    # hm.. these dependencies mean to make the parent project
    # have access to s7.h etc?
    # s7_dep,
    # mongoose_dep,
  ]
)
