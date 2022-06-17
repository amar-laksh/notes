project('eop', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3', 'cpp_std=c++17'])

# These arguments are only used to build the shared library
# not the executables that use the library.
lib_args = ['-DBUILDING_EOP']

shlib = shared_library('eop', 'eop.cpp',
  install : true,
  cpp_args : lib_args,
  gnu_symbol_visibility : 'hidden',
)

# library dependencies
threads = dependency('threads', static: true)

test_exe = executable('eop_sols', 'eop_sols.cpp',
  link_with : shlib,
  dependencies: [threads])
test('eop', test_exe)

# Make this library usable as a Meson subproject.
eop_dep = declare_dependency(
  include_directories: include_directories('.'),
  link_with : shlib,
  )

# Make this library usable from the system's
# package manager.
install_headers('eop.hpp', subdir : 'eop')

pkg_mod = import('pkgconfig')
pkg_mod.generate(
  name : 'eop',
  filebase : 'eop',
  description : 'Problems thought and solved using EOP',
  subdirs : 'eop',
  libraries : shlib,
  version : '0.1',
)
