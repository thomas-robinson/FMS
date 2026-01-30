# Kokkos Integration Summary

## Overview

This document provides a comprehensive summary of the Kokkos integration added to 
the GFDL Flexible Modeling System (FMS).

## What Was Implemented

### 1. Core Kokkos Module (`kokkos/`)

**Files Created:**
- `fms_kokkos.F90` - Main Fortran module with C interoperability bindings
- `fms_kokkos_binding.cpp` - C++ implementation wrapping Kokkos
- `fms_kokkos_binding.h` - C++ header for bindings
- `Makefile.am` - Autotools configuration for kokkos directory
- `include/Makefile.am` - Include directory configuration

**Key Features:**
- Initialization/finalization of Kokkos runtime
- Execution space queries
- View creation and management (1D, 2D, 3D arrays in multiple types)
- Parallel operations (for, reduce with sum/min/max)
- Deep copy between views
- Thread count and configuration queries
- Synchronization primitive

### 2. Communication Patterns

**Files Created:**
- `kokkos/include/mpp_kokkos_comm.inc` - Collective communication patterns
  - Broadcast with Kokkos synchronization
  - Allreduce (sum, min, max)
  - Global reduction operations
  
- `kokkos/include/mpp_kokkos_transmit.inc` - Point-to-point operations
  - Send/receive with synchronization
  - Sendrecv operations
  - Alltoall patterns

### 3. Build System Integration

**CMakeLists.txt Changes:**
- Added `ENABLE_KOKKOS` option (default: OFF)
- Added Kokkos find_package integration
- Added conditional source file inclusion
- Added Kokkos include directories
- Added Kokkos linking to library targets
- Added compiler definition propagation

**configure.ac Changes:**
- Added `--with-kokkos` configuration option
- Added Kokkos detection for C++ headers and libraries
- Added AM_CONDITIONAL for ENABLE_KOKKOS
- Support for both standard and custom Kokkos installations

**Makefile.am Changes:**
- Added kokkos to SUBDIRS in main Makefile.am
- Added conditional compilation flags
- Added kokkos to test_fms subdirectories

### 4. Unit Tests

**Test Files Created:**
- `test_fms/kokkos/test_fms_kokkos.F90` - Basic functionality tests
  - Initialization/finalization
  - Execution space queries
  - Thread count queries
  - Configuration printing
  
- `test_fms/kokkos/test_fms_kokkos_views.F90` - View and memory tests
  - 1D, 2D, 3D view creation
  - Multiple data types (r8, r4, integer)
  - Synchronization operations
  
- `test_fms/kokkos/test_fms_kokkos.sh` - Shell test wrapper
- `test_fms/kokkos/test_fms_kokkos_views.sh` - Shell test wrapper
- `test_fms/kokkos/Makefile.am` - Test build configuration

### 5. Continuous Integration

**GitHub Actions Workflow:**
- `.github/workflows/github_cmake_kokkos.yml`
  - Builds with Kokkos enabled
  - Builds without Kokkos (regression testing)
  - Tests Kokkos functionality
  - Supports ARM architecture builds

### 6. Documentation

**Created Documentation Files:**
- `kokkos/README.md` - User-facing guide
  - Features and capabilities
  - Build instructions (CMake and Autotools)
  - Usage examples
  - Testing information
  - Architecture overview
  - Performance considerations
  
- `kokkos/BUILD_SYSTEM.md` - Build system integration details
  - CMake and Autotools changes
  - Building with different configurations
  - Preprocessor directives
  - Compiler requirements
  - Troubleshooting
  
- `kokkos/DEVELOPER_GUIDE.md` - Developer-focused documentation
  - Architecture details
  - How to add new features
  - Type system and generics
  - Memory management patterns
  - Testing strategy
  - Performance considerations
  - Debugging guide

## Design Principles

### 1. Backward Compatibility

All Kokkos code is optional and gated with `#ifdef ENABLE_KOKKOS`:
- FMS builds and works without Kokkos installed
- Stub implementations provide API compatibility
- No linker dependencies when disabled
- Existing code continues to work unchanged

### 2. Integration with Existing FMS Architecture

Follows FMS design patterns:
- Main interfaces in `fms_kokkos_mod` (like `fms_mod`)
- Implementation in .inc files (like mpp module)
- Communication patterns integrated with MPI
- Uses existing platform and type definitions

### 3. C++ and Fortran Interoperability

- Uses ISO C binding for Fortran-C++ communication
- Type-safe parameter passing
- Pointer management through c_ptr
- No manual memory management complexity

### 4. Flexible Execution Space Support

Supports multiple Kokkos execution spaces:
- HOST (CPU-only)
- CUDA (NVIDIA GPUs)
- OpenMP (Shared memory)
- HIP (AMD GPUs)
- SYCL (Intel and other GPUs)

## File Structure

```
FMS/
├── kokkos/                              # New Kokkos module
│   ├── fms_kokkos.F90                   # Main Fortran module
│   ├── fms_kokkos_binding.cpp           # C++ implementation
│   ├── Makefile.am                      # Autotools config
│   ├── README.md                        # User documentation
│   ├── BUILD_SYSTEM.md                  # Build system guide
│   ├── DEVELOPER_GUIDE.md               # Developer guide
│   └── include/
│       ├── fms_kokkos_binding.h         # C++ header
│       ├── mpp_kokkos_comm.inc          # Communication patterns
│       ├── mpp_kokkos_transmit.inc      # Point-to-point patterns
│       └── Makefile.am                  # Include config
│
├── test_fms/kokkos/                     # New test suite
│   ├── test_fms_kokkos.F90              # Basic tests
│   ├── test_fms_kokkos.sh               # Test script
│   ├── test_fms_kokkos_views.F90        # View tests
│   ├── test_fms_kokkos_views.sh         # Test script
│   └── Makefile.am                      # Test config
│
├── CMakeLists.txt                       # Updated
├── configure.ac                         # Updated
├── Makefile.am                          # Updated
│
└── .github/workflows/
    └── github_cmake_kokkos.yml          # New CI workflow
```

## How to Build

### Quick Start with CMake

```bash
cd FMS
mkdir build && cd build

# Without Kokkos (default)
cmake ..

# With Kokkos
cmake -DENABLE_KOKKOS=on ..

make -j 4
ctest
```

### With Autotools

```bash
cd FMS

# Without Kokkos
./configure --enable-setting-flags
make

# With Kokkos
./configure --with-kokkos --enable-setting-flags
make
```

## Using Kokkos in FMS

### From Fortran Code

```fortran
use fms_kokkos_mod, only: kokkos_init, kokkos_finalize

! Initialize Kokkos
call kokkos_init()

! Use Kokkos features
! ... computation and communication ...

! Finalize
call kokkos_finalize()
```

### Integration with MPI

```fortran
! Before MPI operations
call kokkos_synchronize()

! MPI communication
call mpp_broadcast(data, length, from_pe)

! After MPI operations
call kokkos_synchronize()
```

## Testing

### Run Tests

```bash
# With CMake
cd build
ctest -L kokkos -V

# With Autotools
make check
```

### Test Coverage

- Basic Kokkos initialization
- Configuration queries
- View creation (1D, 2D, 3D)
- Multiple data types (r4, r8, integer)
- Synchronization
- Memory operations

## Future Enhancements

Potential areas for expansion:

1. **Advanced View Types**
   - Custom layouts
   - Unstructured views
   - View slicing operations

2. **Team Parallelism**
   - Hierarchical parallel patterns
   - Thread teams
   - Team reductions

3. **Domain Decomposition**
   - GPU-aware halo exchanges
   - Overlapping computation and communication
   - Asynchronous operations

4. **Performance Analysis**
   - Built-in profiling
   - Performance counters
   - Timeline generation

5. **GPU-Optimized Kernels**
   - FFT operations on GPU
   - Stencil computations
   - Matrix operations

## Known Limitations

1. **Stub Implementation Only**: Current Kokkos module provides interfaces only.
   Full Kokkos parallelization of kernels requires user implementation.

2. **Memory Views**: View operations are currently stubs; GPU memory allocation
   requires Kokkos library to be compiled and linked.

3. **Type Support**: Limited to real(r8), real(r4), and integer types.
   Complex and other types can be added as needed.

4. **MPI Integration**: Currently maintains MPI for inter-process communication.
   GPU-aware MPI can be added when needed.

## Compliance and Standards

- **Apache License 2.0**: All files include standard FMS header
- **C99/C++11**: Minimum standards used
- **Fortran 2003**: ISO C binding support
- **CMake 3.12+**: Required for CMake builds
- **Autotools**: Full autoconf/automake support

## Contributing

To extend the Kokkos integration:

1. Follow the FMS Code Style Guide
2. Include Apache License header in new files
3. Write comprehensive unit tests
4. Update documentation
5. Test with `ENABLE_KOKKOS=on` and `off`
6. Submit pull request with detailed description

## Support and Contact

For questions about the Kokkos integration:
- GitHub Issues: https://github.com/NOAA-GFDL/FMS/issues
- FMS Website: https://www.gfdl.noaa.gov/fms
- Kokkos Documentation: https://kokkos.github.io/kokkos-core-wiki/

## Acknowledgments

This Kokkos integration was developed following the architecture and design 
principles of the GFDL Flexible Modeling System, maintaining backward 
compatibility and integration with existing FMS modules.

## License

All code and documentation is released under the Apache License 2.0, 
consistent with the FMS library.

---

## Checklist for Integration Verification

- [x] Fortran module created with proper interfaces
- [x] C++ binding layer implemented
- [x] CMake integration complete
- [x] Autotools integration complete
- [x] Build system supports both enabled and disabled Kokkos
- [x] Unit tests created and integrated
- [x] GitHub CI workflow added
- [x] Documentation complete (user guide, build guide, developer guide)
- [x] Apache License headers on all new files
- [x] Backward compatibility maintained
- [x] No mandatory Kokkos dependencies
- [x] Stub implementations for API compatibility

## Version Information

- **FMS Version**: 2026.01
- **Kokkos Support**: Initial integration
- **Fortran Standard**: 2003 (ISO C binding)
- **C++ Standard**: C++11 minimum
- **CMake Version**: 3.12+
- **Integration Date**: January 2026
