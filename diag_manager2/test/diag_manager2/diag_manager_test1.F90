program diag_manager_test1
use fms_diag_manager2_mod
!include 'mpif.h'

integer :: ierr
   call MPI_INIT(ierr)
   call fms_diag_manager_init() 
   call MPI_FINALIZE(ierr)
end program diag_manager_test1
