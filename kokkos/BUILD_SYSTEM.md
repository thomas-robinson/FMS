# FMS Kokkos Build System Integration

## Overview

This document describes how Kokkos support has been integrated into both the 
CMake and Autotools build systems for FMS.

## CMake Integration

### CMakeLists.txt Changes

The main `CMakeLists.txt` has been modified to include Kokkos support:

1. **Added Build Option**
   ```cmake
   option(ENABLE_KOKKOS "Build FMS with Kokkos support" OFF)
   ```
   - Default is OFF for backward compatibility
   - Can be enabled with `-DENABLE_KOKKOS=on`

2. **Added Kokkos Find Module**
   ```cmake
   if (ENABLE_KOKKOS)
     find_package(Kokkos REQUIRED)
     message(STATUS "Kokkos found: ${Kokkos_DIR}")
     list(APPEND fms_defs ENABLE_KOKKOS)
   endif ()
   ```
   - Uses CMake's `find_package()` to locate Kokkos
   - Adds `ENABLE_KOKKOS` to compiler definitions

3. **Added Source Files**
   ```cmake
   if (ENABLE_KOKKOS)
     list(APPEND fms_fortran_src_files
       kokkos/fms_kokkos.F90
     )
     list(APPEND fms_c_src_files
       kokkos/fms_kokkos_binding.cpp
     )
   endif ()
   ```

4. **Added Include Directories**
   ```cmake
   $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/kokkos/include>
   ```

5. **Added Library Linking**
   ```cmake
   if(ENABLE_KOKKOS)
     target_link_libraries(${libTgt} PRIVATE Kokkos::kokkos)
   endif()
   ```

### Directory Structure

```
kokkos/
├── CMakeLists.txt (if subdirectory-based build)
├── fms_kokkos.F90
├── fms_kokkos_binding.cpp
├── Makefile.am
├── include/
│   ├── Makefile.am
│   ├── fms_kokkos_binding.h
│   ├── mpp_kokkos_comm.inc
│   └── mpp_kokkos_transmit.inc
└── README.md
```

## Autotools Integration

### configure.ac Changes

The `configure.ac` file has been modified to support Kokkos configuration:

1. **Added Command-line Option**
   ```sh
   AC_ARG_WITH([kokkos],
     [AS_HELP_STRING([--with-kokkos=PATH],
       [Build with Kokkos support, optionally specify the installation prefix. (Default no)])])
   ```
   - Default is `no` (Kokkos optional)
   - Can be enabled with `--with-kokkos` or `--with-kokkos=/path/to/kokkos`

2. **Added Kokkos Detection**
   ```sh
   if test "x$with_kokkos" != "xno"; then
     AC_CHECK_HEADERS([Kokkos_Core.hpp], ...)
     AC_SEARCH_LIBS([kokkos_initialize], [kokkos], ...)
     AC_DEFINE([ENABLE_KOKKOS], [1], [Kokkos support enabled])
     AM_CONDITIONAL([ENABLE_KOKKOS], true)
   else
     AM_CONDITIONAL([ENABLE_KOKKOS], false)
   fi
   ```

3. **Environment Variables**
   Users can set these for Kokkos:
   - `CXXFLAGS`: C++ compiler flags
   - `CXXLIBS`: C++ libraries
   - `Kokkos_ROOT` or `with_kokkos`: Kokkos installation path

### Makefile.am Changes

Main `Makefile.am`:
- Added `kokkos` to `SUBDIRS` list for recursive builds

`kokkos/Makefile.am`:
```make
if ENABLE_KOKKOS
  AM_FCFLAGS += -DENABLE_KOKKOS
  AM_CXXFLAGS += -DENABLE_KOKKOS
endif

noinst_LTLIBRARIES = libkokkos.la
libkokkos_la_SOURCES = \
  fms_kokkos.F90 \
  fms_kokkos_binding.cpp
```

Test `test_fms/kokkos/Makefile.am`:
```make
if ENABLE_KOKKOS
  AM_FCFLAGS += -DENABLE_KOKKOS
endif

check_PROGRAMS = test_fms_kokkos test_fms_kokkos_views
TESTS = test_fms_kokkos.sh test_fms_kokkos_views.sh
```

## Building with Different Configurations

### CMake with Kokkos

```bash
# Using Kokkos installation in standard location
cmake -DENABLE_KOKKOS=on -DNetCDF_ROOT=/opt/netcdf ..

# Using custom Kokkos installation
cmake -DENABLE_KOKKOS=on \
      -Dkokkos_ROOT=/home/user/kokkos-install \
      -DNetCDF_ROOT=/opt/netcdf ..

# With CUDA support (Kokkos must be built with CUDA)
cmake -DENABLE_KOKKOS=on \
      -Dkokkos_ROOT=/opt/kokkos-cuda \
      -DNetCDF_ROOT=/opt/netcdf ..
```

### Autotools with Kokkos

```bash
# Using Kokkos in standard location
./configure --with-kokkos

# Using custom Kokkos installation
./configure --with-kokkos=/home/user/kokkos-install

# With additional compiler flags
./configure --with-kokkos=/opt/kokkos \
           CXXFLAGS="-O3 -march=native"
```

### Building Without Kokkos

```bash
# CMake (default)
cmake ..
make

# Autotools (default)
./configure
make
```

## Preprocessor Directives

The code uses `#ifdef ENABLE_KOKKOS` and `#ifndef ENABLE_KOKKOS` for conditional compilation:

```fortran
#ifdef ENABLE_KOKKOS
  ! Kokkos-specific implementation
  use mpi
  call kokkos_initialize()
#else
  ! Stub implementation
  call kokkos_initialize_stub()
#endif
```

This ensures:
1. Kokkos code is only compiled when enabled
2. Stub implementations provide API compatibility
3. No runtime errors when Kokkos is not available

## Compiler Requirements

### Minimum Versions

- **GCC**: 8.0 or later (C++11 or later required)
- **Intel**: 19.0 or later
- **NVIDIA nvcc**: CUDA Toolkit 11.0 or later (for GPU support)
- **AMD rocm**: ROCm 3.5 or later (for AMD GPU support)

### Fortran Compiler

- **GFortran**: 8.0 or later
- **Intel Fortran**: 19.0 or later

## Linking and Dependencies

When Kokkos is enabled, the following linking occurs:

1. **Fortran Module** depends on:
   - `fms_kokkos.F90` (Fortran ISO C bindings)
   - `iso_c_binding` (Fortran intrinsic)
   - `platform_mod` (FMS platform definitions)

2. **C++ Binding** depends on:
   - `Kokkos::kokkos` (CMake target or library)
   - C++ standard library

3. **FMS Library** links:
   - `kokkos/fms_kokkos.F90`
   - `kokkos/fms_kokkos_binding.cpp`
   - `Kokkos::kokkos` (if available)

## Testing Integration

### CTest (CMake)

Tests are automatically registered with CTest:
```bash
cd build
ctest -L kokkos -V  # Run only Kokkos tests
ctest -V            # Run all tests
```

### Autotools

Tests are run with:
```bash
make check
```

All test scripts must be executable and return 0 on success, non-zero on failure.

## Conditional Features

### When Kokkos is Enabled

The following Kokkos-specific features are available:

1. GPU acceleration support (if Kokkos built with GPU backend)
2. Advanced parallel patterns
3. Hierarchical parallelism
4. Team-based execution
5. Custom reductions

### When Kokkos is Disabled

The following are available:

1. Stub implementations of all Kokkos functions
2. Full API compatibility (no compilation errors)
3. Traditional CPU-only execution via MPI
4. Smaller binary size
5. No additional dependencies

## Future Build System Improvements

Potential enhancements:

1. **Separate Kokkos Library**: Build as separate libfms_kokkos
2. **Optional Installation**: Include/exclude Kokkos headers at install time
3. **FindFMS Module**: CMake module for downstream projects
4. **Version Detection**: Kokkos version compatibility checking
5. **SYCL Support**: DPC++ backend for Intel GPUs
6. **HIP Support**: ROCm backend for AMD GPUs

## Troubleshooting Build Issues

### CMake Cannot Find Kokkos

```bash
# Solution: Explicitly set kokkos_ROOT
cmake -Dkokkos_ROOT=/path/to/kokkos ...

# Or set environment variable
export kokkos_ROOT=/path/to/kokkos
cmake ..
```

### Missing Kokkos_Core.hpp

```bash
# Solution: Check Kokkos installation
ls /path/to/kokkos/include/Kokkos_Core.hpp

# Or install Kokkos headers
make install -C kokkos-build
```

### Linking Errors

```bash
# Solution: Add Kokkos lib directory to LDFLAGS
export LDFLAGS="-L/path/to/kokkos/lib"
./configure --with-kokkos=/path/to/kokkos
```

## CI/CD Integration

The repository includes automated CI/CD workflows:

- **github_cmake_kokkos.yml**: Tests Kokkos builds with CMake
- Tests both with and without Kokkos enabled
- Ensures no regressions in either configuration

## References

- [CMake Documentation](https://cmake.org/documentation/)
- [Autotools Documentation](https://www.gnu.org/software/autoconf/)
- [Kokkos CMake Integration](https://kokkos.github.io/kokkos-core-wiki/building/)
