# Kokkos Integration with GFDL FMS

## Overview

This document describes the integration of the Kokkos parallel programming framework 
with the GFDL Flexible Modeling System (FMS). Kokkos provides a C++ performance 
portability abstraction that enables efficient computation on multicore CPUs, GPUs 
(NVIDIA, AMD), and heterogeneous systems.

## Features

### Current Implementation

The FMS Kokkos integration includes:

1. **Core Module**: `fms_kokkos_mod` - Main Fortran interface to Kokkos
2. **C++ Bindings**: C++ wrapper functions for Kokkos operations
3. **Conditional Compilation**: Support for optional builds via `ENABLE_KOKKOS` flag
4. **Communication Patterns**: Integration with existing MPI communication routines
5. **Stub Implementation**: Full API compatibility even when Kokkos is not available

### Provided Interfaces

#### Initialization and Configuration
- `kokkos_init()` - Initialize Kokkos runtime
- `kokkos_finalize()` - Finalize Kokkos runtime
- `kokkos_get_execution_space()` - Query current execution space
- `kokkos_get_num_threads()` - Get available thread count
- `kokkos_print_configuration()` - Print Kokkos configuration
- `kokkos_synchronize()` - Synchronize Kokkos execution

#### View Management
- `kokkos_create_view_1d_r8/r4()` - Create 1D double/single precision views
- `kokkos_create_view_2d_r8/r4()` - Create 2D double/single precision views
- `kokkos_create_view_3d_r8/r4()` - Create 3D double/single precision views
- `kokkos_create_view_1d/2d/3d_int()` - Create integer views
- `kokkos_deep_copy()` - Deep copy between views

#### Parallel Operations
- `kokkos_parallel_for()` - Parallel for loops
- `kokkos_parallel_reduce_sum()` - Parallel sum reduction
- `kokkos_parallel_reduce_max()` - Parallel max reduction
- `kokkos_parallel_reduce_min()` - Parallel min reduction

## Building FMS with Kokkos Support

### Prerequisites

- Kokkos library (https://github.com/kokkos/kokkos)
- C++ compiler (g++, clang++, Intel, NVIDIA nvcc, etc.)
- Fortran compiler (gfortran, ifort, etc.)
- MPI library (OpenMPI, MPICH, etc.)
- NetCDF library (Fortran and C variants)

### CMake Build

To build FMS with Kokkos support using CMake:

```bash
cd FMS
mkdir build
cd build

# Basic build with Kokkos
cmake -DENABLE_KOKKOS=on \
       -DNetCDF_ROOT=/path/to/netcdf \
       -DCMAKE_BUILD_TYPE=Release \
       ..

make -j 4
```

For GPU support, you need to configure Kokkos first and pass the correct flags:

```bash
# With CUDA support
cmake -DENABLE_KOKKOS=on \
       -DCMAKE_CXX_COMPILER=g++ \
       -Dkokkos_ROOT=/path/to/kokkos-install \
       -DNetCDF_ROOT=/path/to/netcdf \
       ..

# With OpenMP support
cmake -DENABLE_KOKKOS=on \
       -DCMAKE_CXX_COMPILER=g++ \
       -DOPENMP=on \
       -Dkokkos_ROOT=/path/to/kokkos-install \
       -DNetCDF_ROOT=/path/to/netcdf \
       ..
```

### Autotools Build

To build FMS with Kokkos support using Autotools:

```bash
cd FMS

# Configure with Kokkos support
./configure --with-kokkos=/path/to/kokkos \
            --enable-setting-flags \
            --prefix=/path/to/install

make -j 4
make check  # Run unit tests
make install
```

### Building Without Kokkos (Default)

By default, Kokkos is not required:

```bash
# CMake (Kokkos disabled by default)
cmake -DENABLE_KOKKOS=off ..
make

# Autotools
./configure  # Kokkos is optional, disabled by default
make
```

## Using Kokkos from FMS

### Fortran Code Example

```fortran
program example
  use fms_mod, only: fms_init, fms_end
  use fms_kokkos_mod, only: kokkos_init, kokkos_finalize, &
                            kokkos_get_num_threads, kokkos_synchronize
  implicit none

  ! Initialize FMS and Kokkos
  call fms_init()
  call kokkos_init()

  ! Use Kokkos functions
  print *, "Number of threads:", kokkos_get_num_threads()

  ! Synchronize before communication
  call kokkos_synchronize()

  ! Perform work...

  ! Cleanup
  call kokkos_finalize()
  call fms_end()
end program example
```

## Testing

### Unit Tests

Two main test programs are provided:

1. **test_fms_kokkos** - Tests basic Kokkos initialization and querying
   ```bash
   test_fms/kokkos/test_fms_kokkos.sh
   ```

2. **test_fms_kokkos_views** - Tests Kokkos view creation and management
   ```bash
   test_fms/kokkos/test_fms_kokkos_views.sh
   ```

### Running Tests

With CMake:
```bash
cd build
ctest -L kokkos -V
```

With Autotools:
```bash
make check
```

### GitHub Actions

The repository includes a GitHub Actions workflow for automated testing:
- `.github/workflows/github_cmake_kokkos.yml` - Builds and tests Kokkos integration

## Architecture and Design

### Module Organization

```
kokkos/
├── fms_kokkos.F90              # Main Fortran module
├── fms_kokkos_binding.cpp      # C++ implementation
├── include/
│   ├── fms_kokkos_binding.h    # C++ header
│   ├── mpp_kokkos_comm.inc     # Communication patterns
│   └── mpp_kokkos_transmit.inc # Point-to-point operations
└── Makefile.am                 # Autotools configuration
```

### Design Paradigm

The FMS Kokkos integration follows the existing FMS design pattern:

1. **Public Interface**: Exposed through `fms_kokkos_mod` module
2. **Implementation**: Hidden in .inc files (similar to mpp_mod)
3. **Conditional Compilation**: All Kokkos code wrapped with `#ifdef ENABLE_KOKKOS`
4. **Backward Compatibility**: Stub implementations ensure compatibility without Kokkos

### Integration with MPP Module

The Kokkos integration is designed to work alongside the existing MPP module:

- Enhanced communication routines with Kokkos synchronization
- Parallel reduction operations using Kokkos where available
- Maintains MPI for inter-process communication
- Local computation acceleration via Kokkos parallel loops

## Performance Considerations

### Execution Spaces

Kokkos supports multiple execution spaces:
- **HOST** (0): Traditional CPU execution
- **CUDA** (1): NVIDIA GPU execution
- **OPENMP** (2): Shared-memory parallelism
- **HIP** (3): AMD GPU execution
- **SYCL** (4): Standards-based heterogeneous computing

Query the current execution space:
```fortran
exec_space = kokkos_get_execution_space()
```

### Data Management

Views provide structured access to data on different memory spaces:
- Automatic memory management
- Deep copy support between memory spaces
- Compatible with Kokkos' memory allocation strategies

## Troubleshooting

### Build Issues

**Issue**: CMake cannot find Kokkos
```bash
Solution: Set -Dkokkos_ROOT=/path/to/kokkos/install
```

**Issue**: Missing C++ compiler
```bash
Solution: Set CMAKE_CXX_COMPILER explicitly
cmake -DCMAKE_CXX_COMPILER=g++ ...
```

### Runtime Issues

**Issue**: Kokkos functions return no-op results
- This is expected when Kokkos is not enabled at compile time
- Check if `-DENABLE_KOKKOS=on` was passed to CMake
- Verify Kokkos library is properly linked

**Issue**: Segmentation fault in Kokkos code
- Ensure Kokkos initialization is called before using Kokkos functions
- Check that data views are properly created before use

## Future Enhancements

Planned improvements to the Kokkos integration:

1. **Advanced Views**: Multi-dimensional array views with custom layouts
2. **Team-based Parallelism**: Hierarchical parallel patterns
3. **Custom Reductions**: User-defined reduction operators
4. **Graph Execution**: Tasking and execution graphs
5. **Memory Management**: Advanced memory space queries and strategies
6. **GPU-aware MPI**: Integration with GPU-aware MPI implementations
7. **Performance Profiling**: Built-in performance measurement and reporting

## References

- [Kokkos GitHub Repository](https://github.com/kokkos/kokkos)
- [Kokkos Documentation](https://kokkos.github.io/kokkos-core-wiki/)
- [FMS Repository](https://github.com/NOAA-GFDL/FMS)
- [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0)

## Contributing

Contributions to the Kokkos integration are welcome! Please:

1. Follow the FMS Code Style Guide
2. Include appropriate unit tests
3. Update documentation
4. Ensure backward compatibility
5. Test with and without Kokkos enabled

## License

All Kokkos integration code is licensed under the Apache License 2.0, consistent 
with the rest of the FMS library.
