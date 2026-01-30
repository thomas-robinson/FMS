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
!> @brief Kokkos point-to-point communication implementations

!> @addtogroup mpp_mod
!> @{

#ifdef ENABLE_KOKKOS
#ifdef use_kokkos_mpi

  !> @brief Kokkos-accelerated send operation
  !!
  !! Sends data to a remote PE using MPI with Kokkos synchronization
  subroutine mpp_send_kokkos(to_pe, data, length)
    integer, intent(in) :: to_pe
    MPP_TYPE_ :: data(:)
    integer, intent(in) :: length

    ! Synchronize Kokkos before sending
    call kokkos_synchronize()

    ! Call standard MPI send
    call mpp_send(to_pe, data, length)
  end subroutine mpp_send_kokkos

  !> @brief Kokkos-accelerated receive operation
  !!
  !! Receives data from a remote PE using MPI with Kokkos synchronization
  subroutine mpp_recv_kokkos(from_pe, data, length)
    integer, intent(in) :: from_pe
    MPP_TYPE_ :: data(:)
    integer, intent(in) :: length

    ! Call standard MPI receive
    call mpp_recv(from_pe, data, length)

    ! Synchronize Kokkos after receiving
    call kokkos_synchronize()
  end subroutine mpp_recv_kokkos

  !> @brief Kokkos-accelerated sendrecv operation
  !!
  !! Simultaneously sends to one PE and receives from another using MPI
  subroutine mpp_sendrecv_kokkos(to_pe, send_data, send_length, &
                                   from_pe, recv_data, recv_length)
    integer, intent(in) :: to_pe, from_pe
    MPP_TYPE_ :: send_data(:), recv_data(:)
    integer, intent(in) :: send_length, recv_length

    ! Synchronize Kokkos before communication
    call kokkos_synchronize()

    ! Call standard MPI sendrecv
    call mpp_sendrecv(to_pe, send_data, send_length, &
                      from_pe, recv_data, recv_length)

    ! Synchronize Kokkos after communication
    call kokkos_synchronize()
  end subroutine mpp_sendrecv_kokkos

  !> @brief Kokkos-accelerated alltoall operation
  !!
  !! All-to-all communication pattern using MPI
  subroutine mpp_alltoall_kokkos(send_data, send_size, &
                                  recv_data, recv_size)
    MPP_TYPE_ :: send_data(:), recv_data(:)
    integer, intent(in) :: send_size, recv_size

    ! Synchronize Kokkos before communication
    call kokkos_synchronize()

    ! Call standard MPI alltoall
    call mpp_alltoall(send_data, send_size, recv_data, recv_size)

    ! Synchronize Kokkos after communication
    call kokkos_synchronize()
  end subroutine mpp_alltoall_kokkos

#endif  ! use_kokkos_mpi
#endif  ! ENABLE_KOKKOS

!> @}
