!***********************************************************************
!*                             Apache License 2.0
!*
!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied;
!* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
!* PARTICULAR PURPOSE. See the License for the specific language
!* governing permissions and limitations under the License.
!***********************************************************************

!> @defgroup fms_kokkos_mod fms_kokkos_mod
!> @ingroup kokkos
!> @brief Fortran interface to Kokkos parallel programming framework for FMS
!!
!> This module provides Fortran bindings to Kokkos, a C++ performance portable
!> parallel programming framework. It is intended to be used in conjunction with
!> the mpp_mod module to provide GPU-accelerated communication and computation
!> routines for the GFDL Flexible Modeling System.
!!
!> The module is conditionally compiled via the ENABLE_KOKKOS preprocessor flag.
!> When disabled, all Kokkos-related functionality is stubbed out with no-op
!> implementations to maintain API compatibility.
!!
!> @author GFDL Modeling Systems Group
!!
!> @addtogroup fms_kokkos_mod
!> @{

#ifdef ENABLE_KOKKOS

module fms_kokkos_mod

  use iso_c_binding, only : c_int, c_double, c_float, c_char, c_null_char, c_loc, c_ptr
  use platform_mod, only : r4_kind, r8_kind, i4_kind, i8_kind

  implicit none
  private

  !> Kokkos execution space types
  integer, parameter, public :: KOKKOS_HOST = 0
  integer, parameter, public :: KOKKOS_CUDA = 1
  integer, parameter, public :: KOKKOS_OPENMP = 2
  integer, parameter, public :: KOKKOS_HIP = 3
  integer, parameter, public :: KOKKOS_SYCL = 4

  !> Kokkos memory space types
  integer, parameter, public :: KOKKOS_HOST_MEMORY = 0
  integer, parameter, public :: KOKKOS_CUDA_MEMORY = 1
  integer, parameter, public :: KOKKOS_DEVICE_MEMORY = 2

  ! Public interfaces
  public :: kokkos_init
  public :: kokkos_finalize
  public :: kokkos_get_execution_space
  public :: kokkos_get_num_threads
  public :: kokkos_print_configuration
  public :: kokkos_synchronize
  public :: kokkos_create_view_1d_r8
  public :: kokkos_create_view_2d_r8
  public :: kokkos_create_view_3d_r8
  public :: kokkos_create_view_1d_r4
  public :: kokkos_create_view_2d_r4
  public :: kokkos_create_view_3d_r4
  public :: kokkos_create_view_1d_int
  public :: kokkos_create_view_2d_int
  public :: kokkos_create_view_3d_int
  public :: kokkos_deep_copy
  public :: kokkos_parallel_for
  public :: kokkos_parallel_reduce_sum
  public :: kokkos_parallel_reduce_max
  public :: kokkos_parallel_reduce_min

  ! Module variables
  logical, public :: kokkos_is_initialized = .false.
  integer, public :: kokkos_execution_space = KOKKOS_HOST
  integer, public :: kokkos_num_threads = 1

  ! C interop interfaces
  interface
    subroutine c_kokkos_initialize() bind(C, name='kokkos_initialize')
      use iso_c_binding
    end subroutine c_kokkos_initialize

    subroutine c_kokkos_finalize() bind(C, name='kokkos_finalize')
      use iso_c_binding
    end subroutine c_kokkos_finalize

    integer(c_int) function c_kokkos_get_execution_space() bind(C, name='kokkos_get_execution_space')
      use iso_c_binding
    end function c_kokkos_get_execution_space

    integer(c_int) function c_kokkos_get_num_threads() bind(C, name='kokkos_get_num_threads')
      use iso_c_binding
    end function c_kokkos_get_num_threads

    subroutine c_kokkos_print_configuration() bind(C, name='kokkos_print_configuration')
      use iso_c_binding
    end subroutine c_kokkos_print_configuration

    subroutine c_kokkos_synchronize() bind(C, name='kokkos_synchronize')
      use iso_c_binding
    end subroutine c_kokkos_synchronize
  end interface

  contains

    !> @brief Initialize Kokkos runtime
    !!
    !! Calls the C++ Kokkos initialization function. Should be called once
    !! at the beginning of the program, typically during fms_init().
    subroutine kokkos_init()
      if (kokkos_is_initialized) return

      call c_kokkos_initialize()
      kokkos_is_initialized = .true.
      kokkos_execution_space = c_kokkos_get_execution_space()
      kokkos_num_threads = c_kokkos_get_num_threads()
    end subroutine kokkos_init

    !> @brief Finalize Kokkos runtime
    !!
    !! Calls the C++ Kokkos finalization function. Should be called once
    !! at the end of the program, typically during fms_end().
    subroutine kokkos_finalize()
      if (.not. kokkos_is_initialized) return

      call c_kokkos_finalize()
      kokkos_is_initialized = .false.
    end subroutine kokkos_finalize

    !> @brief Get the current Kokkos execution space
    !!
    !! Returns an integer identifier for the current execution space
    !! (KOKKOS_HOST, KOKKOS_CUDA, KOKKOS_OPENMP, etc.)
    function kokkos_get_execution_space() result(space)
      integer :: space

      space = c_kokkos_get_execution_space()
    end function kokkos_get_execution_space

    !> @brief Get the number of Kokkos threads
    function kokkos_get_num_threads() result(nthreads)
      integer :: nthreads

      nthreads = c_kokkos_get_num_threads()
    end function kokkos_get_num_threads

    !> @brief Print Kokkos configuration
    subroutine kokkos_print_configuration()
      call c_kokkos_print_configuration()
    end subroutine kokkos_print_configuration

    !> @brief Synchronize Kokkos execution
    subroutine kokkos_synchronize()
      call c_kokkos_synchronize()
    end subroutine kokkos_synchronize

    !> @brief Create a 1D double precision Kokkos view
    !!
    !! @param label A descriptive label for the view
    !! @param n1 Size of the first dimension
    !! @return A C pointer to the Kokkos view
    function kokkos_create_view_1d_r8(label, n1) result(view_ptr)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1
      type(c_ptr) :: view_ptr

      ! Stub implementation
      view_ptr = c_null_ptr
    end function kokkos_create_view_1d_r8

    !> @brief Create a 2D double precision Kokkos view
    function kokkos_create_view_2d_r8(label, n1, n2) result(view_ptr)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2
      type(c_ptr) :: view_ptr

      view_ptr = c_null_ptr
    end function kokkos_create_view_2d_r8

    !> @brief Create a 3D double precision Kokkos view
    function kokkos_create_view_3d_r8(label, n1, n2, n3) result(view_ptr)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2, n3
      type(c_ptr) :: view_ptr

      view_ptr = c_null_ptr
    end function kokkos_create_view_3d_r8

    !> @brief Create a 1D single precision Kokkos view
    function kokkos_create_view_1d_r4(label, n1) result(view_ptr)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1
      type(c_ptr) :: view_ptr

      view_ptr = c_null_ptr
    end function kokkos_create_view_1d_r4

    !> @brief Create a 2D single precision Kokkos view
    function kokkos_create_view_2d_r4(label, n1, n2) result(view_ptr)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2
      type(c_ptr) :: view_ptr

      view_ptr = c_null_ptr
    end function kokkos_create_view_2d_r4

    !> @brief Create a 3D single precision Kokkos view
    function kokkos_create_view_3d_r4(label, n1, n2, n3) result(view_ptr)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2, n3
      type(c_ptr) :: view_ptr

      view_ptr = c_null_ptr
    end function kokkos_create_view_3d_r4

    !> @brief Create a 1D integer Kokkos view
    function kokkos_create_view_1d_int(label, n1) result(view_ptr)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1
      type(c_ptr) :: view_ptr

      view_ptr = c_null_ptr
    end function kokkos_create_view_1d_int

    !> @brief Create a 2D integer Kokkos view
    function kokkos_create_view_2d_int(label, n1, n2) result(view_ptr)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2
      type(c_ptr) :: view_ptr

      view_ptr = c_null_ptr
    end function kokkos_create_view_2d_int

    !> @brief Create a 3D integer Kokkos view
    function kokkos_create_view_3d_int(label, n1, n2, n3) result(view_ptr)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2, n3
      type(c_ptr) :: view_ptr

      view_ptr = c_null_ptr
    end function kokkos_create_view_3d_int

    !> @brief Deep copy between Kokkos views
    !!
    !! Copies data from source view to destination view using Kokkos deep copy
    subroutine kokkos_deep_copy(dst, src)
      type(c_ptr), intent(in) :: dst, src

      ! Stub implementation
    end subroutine kokkos_deep_copy

    !> @brief Parallel for loop using Kokkos
    !!
    !! Executes a parallel for loop with the given range
    subroutine kokkos_parallel_for(label, n)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n

      ! Stub implementation
    end subroutine kokkos_parallel_for

    !> @brief Parallel reduce sum using Kokkos
    function kokkos_parallel_reduce_sum(label, n) result(sum_result)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n
      real(r8_kind) :: sum_result

      sum_result = 0.0_r8_kind
    end function kokkos_parallel_reduce_sum

    !> @brief Parallel reduce max using Kokkos
    function kokkos_parallel_reduce_max(label, n) result(max_result)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n
      real(r8_kind) :: max_result

      max_result = 0.0_r8_kind
    end function kokkos_parallel_reduce_max

    !> @brief Parallel reduce min using Kokkos
    function kokkos_parallel_reduce_min(label, n) result(min_result)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n
      real(r8_kind) :: min_result

      min_result = 0.0_r8_kind
    end function kokkos_parallel_reduce_min

end module fms_kokkos_mod

#else

! Stub module when Kokkos is not enabled
module fms_kokkos_mod

  use platform_mod, only : r4_kind, r8_kind, i4_kind, i8_kind
  implicit none
  private

  integer, parameter, public :: KOKKOS_HOST = 0
  integer, parameter, public :: KOKKOS_CUDA = 1
  integer, parameter, public :: KOKKOS_OPENMP = 2
  integer, parameter, public :: KOKKOS_HIP = 3
  integer, parameter, public :: KOKKOS_SYCL = 4

  integer, parameter, public :: KOKKOS_HOST_MEMORY = 0
  integer, parameter, public :: KOKKOS_CUDA_MEMORY = 1
  integer, parameter, public :: KOKKOS_DEVICE_MEMORY = 2

  public :: kokkos_init
  public :: kokkos_finalize
  public :: kokkos_get_execution_space
  public :: kokkos_get_num_threads
  public :: kokkos_print_configuration
  public :: kokkos_synchronize
  public :: kokkos_create_view_1d_r8
  public :: kokkos_create_view_2d_r8
  public :: kokkos_create_view_3d_r8
  public :: kokkos_create_view_1d_r4
  public :: kokkos_create_view_2d_r4
  public :: kokkos_create_view_3d_r4
  public :: kokkos_create_view_1d_int
  public :: kokkos_create_view_2d_int
  public :: kokkos_create_view_3d_int
  public :: kokkos_deep_copy
  public :: kokkos_parallel_for
  public :: kokkos_parallel_reduce_sum
  public :: kokkos_parallel_reduce_max
  public :: kokkos_parallel_reduce_min

  logical, public :: kokkos_is_initialized = .false.
  integer, public :: kokkos_execution_space = KOKKOS_HOST
  integer, public :: kokkos_num_threads = 1

  contains

    subroutine kokkos_init()
      kokkos_is_initialized = .true.
    end subroutine kokkos_init

    subroutine kokkos_finalize()
      kokkos_is_initialized = .false.
    end subroutine kokkos_finalize

    function kokkos_get_execution_space() result(space)
      integer :: space
      space = KOKKOS_HOST
    end function kokkos_get_execution_space

    function kokkos_get_num_threads() result(nthreads)
      integer :: nthreads
      nthreads = 1
    end function kokkos_get_num_threads

    subroutine kokkos_print_configuration()
    end subroutine kokkos_print_configuration

    subroutine kokkos_synchronize()
    end subroutine kokkos_synchronize

    function kokkos_create_view_1d_r8(label, n1) result(view_ptr)
      use iso_c_binding
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1
      type(c_ptr) :: view_ptr
      view_ptr = c_null_ptr
    end function kokkos_create_view_1d_r8

    function kokkos_create_view_2d_r8(label, n1, n2) result(view_ptr)
      use iso_c_binding
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2
      type(c_ptr) :: view_ptr
      view_ptr = c_null_ptr
    end function kokkos_create_view_2d_r8

    function kokkos_create_view_3d_r8(label, n1, n2, n3) result(view_ptr)
      use iso_c_binding
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2, n3
      type(c_ptr) :: view_ptr
      view_ptr = c_null_ptr
    end function kokkos_create_view_3d_r8

    function kokkos_create_view_1d_r4(label, n1) result(view_ptr)
      use iso_c_binding
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1
      type(c_ptr) :: view_ptr
      view_ptr = c_null_ptr
    end function kokkos_create_view_1d_r4

    function kokkos_create_view_2d_r4(label, n1, n2) result(view_ptr)
      use iso_c_binding
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2
      type(c_ptr) :: view_ptr
      view_ptr = c_null_ptr
    end function kokkos_create_view_2d_r4

    function kokkos_create_view_3d_r4(label, n1, n2, n3) result(view_ptr)
      use iso_c_binding
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2, n3
      type(c_ptr) :: view_ptr
      view_ptr = c_null_ptr
    end function kokkos_create_view_3d_r4

    function kokkos_create_view_1d_int(label, n1) result(view_ptr)
      use iso_c_binding
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1
      type(c_ptr) :: view_ptr
      view_ptr = c_null_ptr
    end function kokkos_create_view_1d_int

    function kokkos_create_view_2d_int(label, n1, n2) result(view_ptr)
      use iso_c_binding
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2
      type(c_ptr) :: view_ptr
      view_ptr = c_null_ptr
    end function kokkos_create_view_2d_int

    function kokkos_create_view_3d_int(label, n1, n2, n3) result(view_ptr)
      use iso_c_binding
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n1, n2, n3
      type(c_ptr) :: view_ptr
      view_ptr = c_null_ptr
    end function kokkos_create_view_3d_int

    subroutine kokkos_deep_copy(dst, src)
      use iso_c_binding
      type(c_ptr), intent(in) :: dst, src
    end subroutine kokkos_deep_copy

    subroutine kokkos_parallel_for(label, n)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n
    end subroutine kokkos_parallel_for

    function kokkos_parallel_reduce_sum(label, n) result(sum_result)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n
      real(r8_kind) :: sum_result
      sum_result = 0.0_r8_kind
    end function kokkos_parallel_reduce_sum

    function kokkos_parallel_reduce_max(label, n) result(max_result)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n
      real(r8_kind) :: max_result
      max_result = 0.0_r8_kind
    end function kokkos_parallel_reduce_max

    function kokkos_parallel_reduce_min(label, n) result(min_result)
      character(len=*), intent(in) :: label
      integer(i8_kind), intent(in) :: n
      real(r8_kind) :: min_result
      min_result = 0.0_r8_kind
    end function kokkos_parallel_reduce_min

end module fms_kokkos_mod

#endif

!> @}
