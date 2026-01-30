# FMS Kokkos Developer Guide

## Architecture Overview

The FMS Kokkos integration provides a modern performance-portable parallel programming 
framework for earth system models. This guide explains the architecture and how to extend it.

## Module Structure

### Main Module: `fms_kokkos_mod`

Location: `kokkos/fms_kokkos.F90`

This is the primary Fortran module that users interact with. It provides:

1. **Initialization/Finalization**
   ```fortran
   subroutine kokkos_init()
   subroutine kokkos_finalize()
   ```

2. **Configuration Queries**
   ```fortran
   function kokkos_get_execution_space() result(space)
   function kokkos_get_num_threads() result(nthreads)
   ```

3. **Parallel Operations**
   ```fortran
   subroutine kokkos_parallel_for(label, n)
   function kokkos_parallel_reduce_sum(label, n) result(sum_result)
   ```

4. **Memory Management**
   ```fortran
   function kokkos_create_view_2d_r8(label, n1, n2) result(view_ptr)
   subroutine kokkos_deep_copy(dst, src)
   ```

### C++ Binding Layer

Location: `kokkos/fms_kokkos_binding.cpp` and `kokkos/include/fms_kokkos_binding.h`

The C++ layer provides:

1. **Kokkos Runtime Management**
   ```cpp
   void kokkos_initialize(void) { Kokkos::initialize(); }
   void kokkos_finalize(void) { Kokkos::finalize(); }
   ```

2. **Execution Space Query**
   ```cpp
   int kokkos_get_execution_space(void) {
     // Check DefaultExecutionSpace and return ID
   }
   ```

3. **Fence/Synchronization**
   ```cpp
   void kokkos_synchronize(void) { Kokkos::fence(); }
   ```

### Communication Include Files

1. **mpp_kokkos_comm.inc**: Broadcast, reduce, and collective operations
2. **mpp_kokkos_transmit.inc**: Point-to-point send/receive operations

These files follow the FMS pattern of using preprocessing and included files 
to generate multiple type-specific implementations.

## Adding New Kokkos Features

### Step 1: Define Fortran Interface

Add to `fms_kokkos_mod`:

```fortran
interface
  subroutine c_kokkos_new_function() bind(C, name='kokkos_new_function')
    use iso_c_binding
  end subroutine c_kokkos_new_function
end interface

subroutine kokkos_new_function()
  call c_kokkos_new_function()
end subroutine kokkos_new_function
```

### Step 2: Implement C++ Wrapper

Add to `fms_kokkos_binding.cpp`:

```cpp
extern "C" {
#ifdef ENABLE_KOKKOS
  void kokkos_new_function(void) {
    // Implementation using Kokkos API
  }
#else
  void kokkos_new_function(void) {
    // Stub implementation
  }
#endif
}
```

### Step 3: Update Header

Add declaration to `fms_kokkos_binding.h`:

```cpp
void kokkos_new_function(void);
```

### Step 4: Add Unit Tests

Create test in `test_fms/kokkos/`:

```fortran
subroutine test_kokkos_new_function()
  call kokkos_new_function()
  ! Assert success
end subroutine
```

### Step 5: Update Documentation

Add to `kokkos/README.md` under appropriate section.

## Type System and Generics

The Kokkos module supports multiple types using Fortran's generic interfaces:

```fortran
! For each type combination:
function kokkos_create_view_1d_r8(label, n1) result(view_ptr)
  character(len=*), intent(in) :: label
  integer(i8_kind), intent(in) :: n1
  type(c_ptr) :: view_ptr
end function

function kokkos_create_view_1d_r4(label, n1) result(view_ptr)
  ! Single precision version
end function
```

When adding new operations, provide multiple type variants:
- `*_r8`: Double precision real
- `*_r4`: Single precision real
- `*_int`: Integer
- `*_i8`: 64-bit integer
- `*_i4`: 32-bit integer

## Memory Management Patterns

### View Creation

Views are created with descriptive labels:
```fortran
! Double precision 2D array
view = kokkos_create_view_2d_r8("my_field", nx, ny)

! Integer 3D array
int_view = kokkos_create_view_3d_int("my_counters", nx, ny, nz)
```

### Deep Copy

Transfer data between views (e.g., host to device):
```fortran
call kokkos_deep_copy(device_view, host_view)
```

### Synchronization

Ensure all pending operations complete:
```fortran
call kokkos_synchronize()
```

## Parallel Patterns

### Parallel For

```fortran
! Simple parallel loop
call kokkos_parallel_for("compute", n)

! Implementation should use Kokkos::parallel_for
! Kokkos::parallel_for(Kokkos::RangePolicy<>(0, n), [=](int i) {
!   // Loop body
! });
```

### Parallel Reduce

```fortran
! Sum reduction
result = kokkos_parallel_reduce_sum("my_sum", n)

! Implementation uses Kokkos::parallel_reduce with SUM operation
```

## Conditional Compilation

All Kokkos-specific code is wrapped:

```fortran
#ifdef ENABLE_KOKKOS
  ! Production Kokkos code
#else
  ! Stub implementation
#endif
```

This ensures:
1. Code compiles without Kokkos dependency
2. API remains identical
3. Runtime behavior differs appropriately
4. No linker errors without Kokkos library

## Testing Strategy

### Unit Tests Structure

Each test file:
1. Tests a specific feature
2. Reports PASS/FAIL
3. Returns 0 (success) or 1 (failure)
4. Includes timing information if relevant

### Test Template

```fortran
program test_kokkos_feature
  use fms_kokkos_mod
  use platform_mod
  implicit none

  logical :: success = .true.

  ! Initialize
  call kokkos_init()

  ! Test feature
  call test_feature()

  ! Finalize
  call kokkos_finalize()

  ! Report
  if (success) then
    stop 0
  else
    stop 1
  endif

contains
  subroutine test_feature()
    ! Test implementation
  end subroutine
end program
```

## Integration with MPP Module

The Kokkos module integrates with the existing MPP module for communication:

1. **Before Communication**: Call `kokkos_synchronize()`
2. **Use MPI**: Standard MPI calls via `mpp_` routines
3. **After Communication**: Call `kokkos_synchronize()`

This pattern ensures GPU/accelerator data is in host memory for MPI operations.

## Performance Considerations

### Execution Space Selection

Different execution spaces have different characteristics:

| Space | Use Case | Notes |
|-------|----------|-------|
| HOST | CPU-only systems | Always available |
| OPENMP | Shared-memory parallelism | Good CPU utilization |
| CUDA | NVIDIA GPUs | Requires NVIDIA hardware |
| HIP | AMD GPUs | Requires AMD hardware |
| SYCL | Intel/generic GPUs | Portable GPU support |

### Memory Space Considerations

- **HOST**: CPU main memory
- **DEVICE**: GPU or accelerator memory
- **CUDA_UVM**: NVIDIA Unified Virtual Memory

### Reducing Synchronization

Minimize `kokkos_synchronize()` calls:
- Batch operations where possible
- Synchronize only before data transfer
- Use Kokkos views for on-device operations

## Common Patterns and Anti-patterns

### Good Pattern: Batch Operations

```fortran
call kokkos_init()

! Multiple operations without sync
call kokkos_parallel_for("loop1", 1000_i8_kind)
call kokkos_parallel_for("loop2", 1000_i8_kind)
call kokkos_parallel_for("loop3", 1000_i8_kind)

! Single sync at communication point
call kokkos_synchronize()
call mpp_sum(data)
call kokkos_finalize()
```

### Anti-pattern: Excessive Synchronization

```fortran
! DON'T: Sync after every operation
call kokkos_parallel_for("loop1", 1000_i8_kind)
call kokkos_synchronize()
call kokkos_parallel_for("loop2", 1000_i8_kind)
call kokkos_synchronize()
```

## Debugging

### Enable Debug Output

Compile with debug flags:
```bash
cmake -DCMAKE_BUILD_TYPE=Debug -DENABLE_KOKKOS=on ..
```

### Query Configuration

```fortran
call kokkos_print_configuration()
print *, "Execution space:", kokkos_get_execution_space()
print *, "Num threads:", kokkos_get_num_threads()
```

### GPU Debugging

For CUDA or HIP:
```bash
# NVIDIA
export CUDA_LAUNCH_BLOCKING=1
export CUDA_DEVICE_ORDER=PCI_BUS_ID

# AMD
export HSA_DEBUG=1
```

## Building and Testing Locally

### CMake Developer Workflow

```bash
# Configure with Kokkos
cmake -DENABLE_KOKKOS=on -DCMAKE_BUILD_TYPE=Debug ..

# Build
make -j 4

# Test
ctest -L kokkos -V --output-on-failure

# Rebuild after changes
make -j 4 && ctest -L kokkos -V
```

### Autotools Developer Workflow

```bash
# Configure
./configure --with-kokkos=/path/to/kokkos

# Build
make -j 4

# Test
make check

# Rebuild
autoreconf -i
./configure --with-kokkos=/path/to/kokkos
make -j 4
```

## Contribution Checklist

When adding new Kokkos features:

- [ ] Add Fortran interface to `fms_kokkos.F90`
- [ ] Implement C++ wrapper in `fms_kokkos_binding.cpp`
- [ ] Add C declaration to `fms_kokkos_binding.h`
- [ ] Include both `#ifdef ENABLE_KOKKOS` and stub implementation
- [ ] Write unit test in `test_fms/kokkos/`
- [ ] Update shell test script
- [ ] Add test to `Makefile.am`
- [ ] Document in `README.md`
- [ ] Update `BUILD_SYSTEM.md` if build changes
- [ ] Run full test suite
- [ ] Test with Kokkos enabled and disabled
- [ ] Update this developer guide

## References

- [Kokkos API Documentation](https://kokkos.github.io/kokkos-core-wiki/)
- [ISO C Binding in Fortran](https://gcc.gnu.org/wiki/Fortran_Bind_C)
- [FMS Developer Documentation](https://www.gfdl.noaa.gov/fms)
- [CMake Best Practices](https://cliutils.gitlab.io/modern-cmake/)
- [Autotools Documentation](https://www.gnu.org/software/autoconf/manual/)

## Support

For questions or issues:
1. Check the FMS GitHub issues: https://github.com/NOAA-GFDL/FMS/issues
2. Consult Kokkos documentation: https://kokkos.github.io/
3. Contact GFDL Modeling Systems Group
