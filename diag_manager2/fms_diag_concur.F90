module fms_diag_concur_mod
   use fms_diag_data_mod, only: diag_error, fatal, warning, note
   use mpi
   implicit none

!   include 'mpif.h'



type fms_diag_comm_type
     private
     integer :: comm
     integer :: local_pe
     integer :: local_sz
     integer :: global_pe
     integer :: group
     integer :: parent_comm
     logical :: on_diag_list
end type

!> Overwrite the == to compare fms_diag_comm_type variables
interface operator (.eq.)
     module procedure eq_undef_diag_comm
end interface

integer,parameter :: undef_comm=-999 

type(fms_diag_comm_type) :: undef_diag_comm !< The undefined diag communicator 
logical :: fms_diag_concur_init = .false. !< Set to true after diag_comm_split is run
logical :: diag_comm_exist = .false. !< Set to true if there is a diagnostic communicator set up

contains
!> \brief Sets up the diag manager communicator for using diagnostics as a service
subroutine diag_comm_init (diag_comm,diag_pes,parent_comm)
 type(fms_diag_comm_type),intent(out)   :: diag_comm !< The diag communicator
 integer, intent(in)                    :: diag_pes 
 integer, intent(in), optional :: parent_comm !< The parent communicator
!> Initialize the communicator type as undefined
 if (fms_diag_concur_init) return
!> Initialize the diag_comm sent in
 call diag_set_comm (diag_comm,undef_comm,undef_comm,undef_comm,undef_comm,undef_comm,undef_comm,.false.)
 call diag_set_comm (undef_diag_comm,undef_comm,undef_comm,undef_comm,undef_comm,undef_comm,undef_comm,.false.)
 if (diag_pes == 0) then
     call diag_error("diag_comm_init","There are no diag_pes, so communicator is not initialized", warning)
     fms_diag_concur_init = .true.
     diag_comm_exist = .false. 
     return
 endif
!> Set up the diagnostic communicator
!write (6,*) " call diag_comm_split()"
 call diag_comm_split(diag_comm,diag_pes,parent_comm)
!> Set the module to initialized
!write (6,*) "  fms_diag_concur_init = .true."
 fms_diag_concur_init = .true.
!write (6,*) " Module is intialized "
end subroutine diag_comm_init

!> \brief Sets up a communicator for the diag manager
subroutine diag_comm_split (diag_comm, diag_pes, parent_comm)
 type(fms_diag_comm_type),intent(out)   :: diag_comm !< The diag communicator
 integer, intent(in)                    :: diag_pes !< The number of cores to be used by diag_manager
 integer, intent(in), optional :: parent_comm !< The parent communicator
 integer :: comm !< Parent Communicator
 integer :: mycomm !< New split communicator
 integer :: grk, gsz, lrk, lsz !< global rank, global size, local rank, local size 
 integer :: local_group, local_flag !< The local group of cores, and a local flag
 logical :: mpi_init_flag !< True if MPI has been initialized
 integer :: merr !< error return code
 logical, allocatable, dimension(:) :: isThereAComm !< an array of .true.s for ranks on the diag_comm
 logical :: thereIsAComm = .false. !< true if the rank is on the diag communicator
 integer :: i,j, ic, nc 
 integer, allocatable, dimension(:) :: groups, gather_groups
 integer, allocatable, dimension(:) :: lsz_array !< The list of local ranks
!> Check if mpi is initialized
 call MPI_INITIALIZED (mpi_init_flag, merr)
 if (.not.mpi_init_flag) then 
     call diag_error("diag_comm_split","MPI is not initialized",WARNING) 
     diag_comm_exist = .false.
     return
 endif
!> Set up parent communicator
 if (present(parent_comm)) then
     comm = parent_comm
 else
     comm = MPI_COMM_WORLD
 endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Get informaiton 
 call MPI_COMM_SIZE(COMM, gsz, merr) 
 call MPI_COMM_RANK(COMM, grk, merr)
 if (gsz == diag_pes) then !> If the global size of the PEs is the same as the number of diag_PEs
                           !! then there should be no diag communicator
     diag_comm_exist = .false.
     return
 endif
!> Find the group
 call MPI_COMM_GET_ATTR(COMM,MPI_APPNUM,local_group,local_flag,merr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Split a new communicator as mycom
 call MPI_COMM_SPLIT(COMM, local_group, 0, mycomm, merr)
!> Get information about the split communicators
 call mpi_comm_size(mycomm,lsz,merr)
 call mpi_comm_rank(mycomm,lrk,merr)
!> See if the split created a communicator of the correct size
 allocate(logical :: isThereAComm(gsz)) 
 if (lsz==diag_pes) then !> If the local size is equal to the number of diag_pes requested, then
                         !! set up the communicator
     ThereisAComm = .true.
     call diag_set_comm (diag_comm,mycomm,lrk,lsz,grk,local_group,comm,ThereisAComm)
 else
     ThereisaComm = .false.
     call diag_set_comm (diag_comm, undef_comm, undef_comm, undef_comm, undef_comm, undef_comm, &
                         undef_comm,ThereisaComm)
 endif 
 !> Gather to set the diag_comm_exist on all ranks
 call MPI_ALLGATHER(ThereIsAComm,1,MPI_LOGICAL, &
               IsThereAComm,gsz,MPI_LOGICAL, COMM, merr)
ic = 0
!> Set diag_comm_exist to true for all ranks on the diag_comm and false for all ranks not
 do concurrent (i=1:gsz)
     if (IsThereAComm(i)) then
          diag_comm_exist = .true.
          ic = ic +1
     endif
 enddo
!> If there are multiple concurrent programs running with diag_pes number of ranks, use the group of ranks
!! with the highest value of their group
if (ic > diag_pes) then
     if (mod(ic,diag_pes) .ne. 0) then !> Make sure the number of total ranks that meet the ic 
                                       !! is divisible by diag_pes
        call diag_error("diag_comm_split","The number of cores does not match up with the number of potential communicators", FATAL)
     else
          nc = ic/diag_pes !> Figure out how many groups have the name number of ranks as diag_pes
          allocate(integer :: groups(nc))
          allocate(integer :: gather_groups(gsz))
          allocate(integer :: lsz_array(gsz))
          call MPI_ALLGATHER(local_group,1,MPI_INTEGER, &
               gather_groups,gsz,MPI_INTEGER, COMM, merr)
          call MPI_ALLGATHER(lsz,1,MPI_INTEGER, &
               lsz_array,gsz,MPI_INTEGER, COMM, merr)
!write (6,*) local_group,size(gather_groups), gsz, gather_groups
          groups = -999
          j=1
          do i = 1,gsz
               if (j > nc) exit !> exit when the ubound of groups is reached
!> Check to make sure the current group is unique, and add it to groups
                    if (gather_groups(i) .ne. groups(j) .and. j==1 .and. &
                            lsz_array(i) .eq. diag_pes) then 
                        groups(j) = gather_groups(i) 
                        j = j + 1
                    elseif (gather_groups(i) .ne. groups(j) .and.   &
                            gather_groups(i) .ne. groups(j-1) .and. &
                            lsz_array(i) .eq. diag_pes) then
                        groups(j) = gather_groups(i) 
                        j = j + 1
                    endif
          enddo
!> Set all of the groups to the undefined communicator if they are not the highest group number
          if (local_group .ne. maxval(groups)) then
                    call diag_set_comm (diag_comm, undef_comm, undef_comm, undef_comm, undef_comm, &
                         undef_comm,undef_comm,.false.)

          endif
     endif
endif
!> Clean up

if (allocated(isThereAComm)) deallocate(isThereAComm)
if (allocated(groups)) deallocate(groups)
if (allocated(gather_groups)) deallocate(gather_groups)
if (allocated(lsz_array)) deallocate(lsz_array)
end subroutine diag_comm_split

!> \brief Sets the values contained in the fms_diag_comm_type with the input arguments
subroutine diag_set_comm (diag_comm_local,comm,lpe,lsz,gpe,gp,pc,lst)
type(fms_diag_comm_type),intent(out) :: diag_comm_local
integer, intent(in) :: comm
integer, intent(in) :: lpe
integer, intent(in) :: lsz
integer, intent(in) :: gpe
integer, intent(in) :: gp
integer, intent(in) :: pc
logical, intent(in) :: lst

 diag_comm_local%comm        = comm
 diag_comm_local%local_pe    = lpe
 diag_comm_local%local_sz    = lsz
 diag_comm_local%global_pe   = gpe
 diag_comm_local%group       = gp
 diag_comm_local%parent_comm = pc
 diag_comm_local%on_diag_list= lst

end subroutine diag_set_comm

!> \brief Writes ascii information about the diag communicator 
subroutine fms_write_diag_comm (diag_comm,unit_for_writing)
 type(fms_diag_comm_type),intent(in) :: diag_comm
 integer, intent(in), optional :: unit_for_writing
 integer :: ierr
 integer :: wunit
!write (6,*) "In fms_write_diag_comm"
 if (present(unit_for_writing)) then
     wunit = unit_for_writing
 else
     wunit = 6
 endif
 if (diag_comm_exist .and. diag_comm%on_diag_list) then
     if (diag_comm%local_pe == 0) &
          write(wunit,*) "You have requested the diagnostic communicator information."
     call mpi_barrier(diag_comm%comm,ierr)
     write(wunit,*)"My global rank:",diag_comm%global_pe,"; my local rank:", &
          diag_comm%local_pe," out of ",diag_comm%local_sz
     call mpi_barrier(diag_comm%comm,ierr)
     if (diag_comm%local_pe == 0 .and. diag_comm%comm == MPI_COMM_WORLD) &
          write(wunit,*)"We are a part of MPI_COMM_WORLD" 
     call mpi_barrier(diag_comm%comm, ierr)
 elseif (.not. diag_comm_exist) then
     call diag_error("fms_write_diag_comm","There is no diagnostic communicator",NOTE)
 endif
end subroutine fms_write_diag_comm

!> \brief Checks if two fms_diag_comm_type are equal
pure function eq_undef_diag_comm (d1,d2)
type(fms_diag_comm_type),intent(in) :: d1
type(fms_diag_comm_type),intent(in) :: d2
logical :: eq_undef_diag_comm
 if ( &
 d1%comm        == d2%comm        .and.&
 d1%local_pe    == d2%local_pe    .and.&
 d1%local_sz    == d2%local_sz    .and.&
 d1%global_pe   == d2%global_pe   .and.&
 d1%group       == d2%group       .and.&
 d1%parent_comm == d2%parent_comm .and.&
 d1%on_diag_list.eqv. d2%on_diag_list  &
 )then
     eq_undef_diag_comm = .true.
 else
     eq_undef_diag_comm = .false.
 endif

end function eq_undef_diag_comm


end module fms_diag_concur_mod


