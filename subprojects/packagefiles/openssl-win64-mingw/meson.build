project('openssl-win64-mingw', 'c')

cc = meson.get_compiler('c')
# actually we just need the dll...

libcrypto_dep = declare_dependency(
  dependencies : cc.find_library('libcrypto', dirs : meson.current_source_dir() + '/lib'))

libssl_dep = declare_dependency(
  dependencies : cc.find_library('libssl', dirs : meson.current_source_dir() + '/lib'))
