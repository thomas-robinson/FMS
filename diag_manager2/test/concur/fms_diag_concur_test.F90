program diag_concur

   use fms_diag_concur_mod, only:fms_write_diag_comm,diag_comm_init,fms_diag_comm_type
   include 'mpif.h'
!   use mpi_f08
   integer rank, size, ierror, tag, status(MPI_STATUS_SIZE), ierr
   integer :: local_group, local_rank, group_world
   integer :: mycomm
   integer, dimension(:),allocatable :: all_groups, all_ranks
   integer :: ngroups,ming
   integer, dimension(:), allocatable :: ng  
  type(fms_diag_comm_type) :: diag_comm
   call MPI_INIT(ierror)
!write (6,*) "call diag_comm_init()"
   call diag_comm_init(diag_comm)
!write (6,*) "call fms_write_diag_comm()"
   call fms_write_diag_comm(diag_comm)
 
   call MPI_FINALIZE(ierror)

end program diag_concur
