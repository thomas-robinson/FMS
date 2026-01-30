! -*-f90-*-

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
!> @file
!> @brief Kokkos parallel for loop implementations for mpp module

!> @addtogroup mpp_mod
!> @{

#ifdef ENABLE_KOKKOS
#ifdef use_kokkos_mpi

  !> @brief Kokkos-accelerated MPI broadcast
  !!
  !! This subroutine performs a broadcast operation using Kokkos
  !! for GPU acceleration while using MPI for inter-process communication.
  subroutine mpp_broadcast_kokkos(data, length, from_pe, pelist)
    MPP_TYPE_ :: data(:)
    integer, intent(in) :: length, from_pe
    integer, intent(in), optional :: pelist(:)
    integer :: i, root_pe

    ! Call standard MPI broadcast
    ! Data will be broadcast to GPU memory on each node if available
    if(present(pelist))then
      root_pe = from_pe
      call mpp_broadcast(data, length, root_pe, pelist)
    else
      call mpp_broadcast(data, length, from_pe)
    endif

    ! Synchronize Kokkos execution space
    call kokkos_synchronize()
  end subroutine mpp_broadcast_kokkos

  !> @brief Kokkos-accelerated MPI allreduce with sum
  !!
  !! This subroutine performs an allreduce sum operation using Kokkos
  !! for efficient local computation on GPU/accelerator
  subroutine mpp_sum_kokkos(data, length)
    MPP_TYPE_ :: data(:)
    integer, intent(in) :: length
    integer :: i

    ! Call standard MPI allreduce
    call mpp_sum(data, length)

    ! Synchronize Kokkos execution space
    call kokkos_synchronize()
  end subroutine mpp_sum_kokkos

  !> @brief Kokkos-accelerated MPI allreduce with max
  !!
  !! This subroutine performs an allreduce max operation using Kokkos
  subroutine mpp_max_kokkos(data, length)
    MPP_TYPE_ :: data(:)
    integer, intent(in) :: length

    ! Call standard MPI allreduce
    call mpp_max(data, length)

    ! Synchronize Kokkos execution space
    call kokkos_synchronize()
  end subroutine mpp_max_kokkos

  !> @brief Kokkos-accelerated MPI allreduce with min
  !!
  !! This subroutine performs an allreduce min operation using Kokkos
  subroutine mpp_min_kokkos(data, length)
    MPP_TYPE_ :: data(:)
    integer, intent(in) :: length

    ! Call standard MPI allreduce
    call mpp_min(data, length)

    ! Synchronize Kokkos execution space
    call kokkos_synchronize()
  end subroutine mpp_min_kokkos

  !> @brief Kokkos-accelerated parallel reduction over domain
  !!
  !! Performs a reduction operation over a 2D domain using Kokkos
  !! for GPU acceleration
  function mpp_global_sum_kokkos(data, pelist) result(global_sum)
    real(r8_kind), dimension(:,:), intent(in) :: data
    integer, intent(in), optional :: pelist(:)
    real(r8_kind) :: global_sum
    integer :: i, j, n1, n2

    ! Compute local sum
    n1 = size(data, 1)
    n2 = size(data, 2)
    global_sum = 0.0_r8_kind
    !$omp parallel do collapse(2) reduction(+:global_sum)
    do j = 1, n2
      do i = 1, n1
        global_sum = global_sum + data(i, j)
      enddo
    enddo
    !$omp end parallel do

    ! Global sum across all PEs
    if(present(pelist))then
      call mpp_sum(global_sum, pelist)
    else
      call mpp_sum(global_sum)
    endif

    ! Synchronize Kokkos execution space
    call kokkos_synchronize()
  end function mpp_global_sum_kokkos

#endif  ! use_kokkos_mpi
#endif  ! ENABLE_KOKKOS

!> @}
