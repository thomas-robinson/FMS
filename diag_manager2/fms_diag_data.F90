module fms_diag_data_mod

use iso_c_binding
use mpi !> Fortran 2018 style for mpi
implicit none

!include 'mpif.h'

integer, parameter :: diag_null = -999
integer, parameter :: diag_not_found = -1
integer, parameter :: diag_not_registered = 0
integer, parameter :: diag_registered_id = 10
!> Supported averaging intervals
integer, parameter :: monthly = 30
integer, parameter :: daily = 24 
integer, parameter :: diurnal = 2
integer, parameter :: yearly = 12
integer, parameter :: no_diag_averaging = 0
integer, parameter :: instantaneous = 0
integer, parameter :: three_hourly = 3
integer, parameter :: six_hourly = 6
!integer, parameter :: seasonally = 180
!> Supported type/kind of the variable
!integer, parameter :: r16=16
integer, parameter :: r8 = 8
integer, parameter :: r4 = 4
integer, parameter :: i8 = -8
integer, parameter :: i4 = -4
integer, parameter :: string = 19 !< s is the 19th letter of the alphabet
integer, parameter :: null_type_int = -999
!> Matches C struct 
type, bind(c) :: diag_files_type
     character (c_char) :: fname (20) !< file name
     character (c_char) :: frequnit (7) !< the frequency unit
     integer (c_int)    :: freq !< the frequency of data
     character (c_char) :: timeunit(7) !< The unit of time
     character (c_char) :: unlimdim(8) !< The name of the unlimited dimension
     character (c_char) :: key(8) !< Storage for the key in the yaml file
end type diag_files_type
!> Matches C struct 
type, bind(c) :: diag_fields_type
     character (c_char) :: fname (20) !< The field/diagnostic name
     character (c_char) :: var(20) !< The name of the variable
     character (c_char) :: files(20) !< The files that the diagnostic will be written to
     integer (c_int)    :: ikind !< The type/kind of the variable
     character (c_char) :: skind(20) !< The type/kind of the variable
     character (c_char) :: reduction(20) !< IDK
     character (c_char) :: all_all(4) !< This has to be "all"
     character (c_char) :: region(50) !< The region
     character (c_char) :: regcoord(50) !< Coodinates of the region
     character (c_char) :: module_location(20) !< The module
     character (c_char) :: key(8) !< Storage for the key in the yaml file
end type diag_fields_type


!> Extension via containment of diag_fileds
!!  for providing write decisions functionality
type diag_file_plus_type
    type(diag_files_type) :: diag_file
    character(len=:), allocatable :: name
    integer   :: period
    integer   :: start_time
    integer   :: end_time
!!TODO: constructor which takes ancestor!
contains
    procedure :: write_now => write_now_inq
end type diag_file_plus_type


!> Placeholder for fms2_io file object type.
!! TODO: Rename to fms_fname_type ?
type fms_io_obj
    character(len=100) :: fname
end type fms_io_obj


integer :: fatal=-143, note=143, warning=143*2
public :: diag_files_type, diag_fields_type
public :: monthly, daily, diurnal, yearly, no_diag_averaging, instantaneous
public :: three_hourly, six_hourly, r8, r4, i8, i4, string
public :: diag_error, fms_io_obj
public :: fatal, note, warning

CONTAINS

!> \brief Wrapper for error routine to be used.
subroutine diag_error(sub,mess,lev,mycomm)
character(len=*), intent(in)  :: sub
character(len=*), intent(in)  :: mess
integer, intent(in)           :: lev
integer, intent(in), optional :: mycomm
character(len=15)             :: slev
logical                       :: mpi_on
integer                       :: ierr
integer                       :: comm
integer                       :: rk
mpi_on = .false.
if (lev == note) then
     slev = "NOTE: "
     call mpi_initialized(mpi_on,ierr)
elseif (lev == WARNING) then
     slev = "WARNING: "
else
     slev = "FATAL: "
endif

if (lev == note .and. mpi_on) then
     if (present(mycomm)) then
          comm = mycomm
     else
          comm = MPI_COMM_WORLD
     endif
     call mpi_comm_rank(comm, rk, ierr)
     if (rk == 0) then
          write (6,*)trim(slev)//sub,":: ",mess
     endif
else
     write (6,*)trim(slev)//sub,":: ",mess
endif

if (lev==fatal) then
  if (mpi_on) then
     call MPI_ABORT(MPI_COMM_WORLD,-1,ierr)
  else
     stop
  endif
endif

end subroutine diag_error

!> Return true if this file expect data to be writen at
!>  this time based on ...
function write_now_inq(obj, ctime) result (rslt)
    class(diag_file_plus_type) :: obj
    integer, intent(in) :: ctime
    logical :: rslt
    rslt = .true.
end function write_now_inq


end module fms_diag_data_mod
