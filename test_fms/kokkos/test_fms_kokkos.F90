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

!> @file test_fms_kokkos.F90
!> @brief Unit tests for FMS Kokkos integration module

program test_fms_kokkos

  use fms_kokkos_mod
  use platform_mod
  implicit none

  logical :: success = .true.
  integer :: num_threads
  integer :: exec_space
  character(len=256) :: test_name

  ! Initialize test
  print *, ""
  print *, "=========================================="
  print *, "Testing FMS Kokkos Module"
  print *, "=========================================="
  print *, ""

  ! Test 1: Kokkos initialization
  test_name = "Test 1: Kokkos initialization"
  print *, "Running: " // trim(test_name)
  call test_kokkos_init()
  if (kokkos_is_initialized) then
    print *, "PASS: " // trim(test_name)
  else
    print *, "FAIL: " // trim(test_name)
    success = .false.
  endif

  ! Test 2: Get execution space
  test_name = "Test 2: Get execution space"
  print *, "Running: " // trim(test_name)
  exec_space = kokkos_get_execution_space()
  print *, "Execution space: ", exec_space
  if (exec_space >= 0 .and. exec_space <= 4) then
    print *, "PASS: " // trim(test_name)
  else
    print *, "FAIL: " // trim(test_name)
    success = .false.
  endif

  ! Test 3: Get number of threads
  test_name = "Test 3: Get number of threads"
  print *, "Running: " // trim(test_name)
  num_threads = kokkos_get_num_threads()
  print *, "Number of threads: ", num_threads
  if (num_threads > 0) then
    print *, "PASS: " // trim(test_name)
  else
    print *, "FAIL: " // trim(test_name)
    success = .false.
  endif

  ! Test 4: Print configuration
  test_name = "Test 4: Print Kokkos configuration"
  print *, "Running: " // trim(test_name)
  call kokkos_print_configuration()
  print *, "PASS: " // trim(test_name)

  ! Test 5: Kokkos finalization
  test_name = "Test 5: Kokkos finalization"
  print *, "Running: " // trim(test_name)
  call kokkos_finalize()
  if (.not. kokkos_is_initialized) then
    print *, "PASS: " // trim(test_name)
  else
    print *, "FAIL: " // trim(test_name)
    success = .false.
  endif

  ! Print final results
  print *, ""
  print *, "=========================================="
  if (success) then
    print *, "All tests PASSED"
    stop 0
  else
    print *, "Some tests FAILED"
    stop 1
  endif

contains

  subroutine test_kokkos_init()
    call kokkos_init()
  end subroutine test_kokkos_init

end program test_fms_kokkos
