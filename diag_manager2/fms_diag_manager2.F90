module fms_diag_manager2_mod
!> \author Tom Robinson
!> \email thomas.robinson@noaa.gov
!! \description Public module used for diag_manager.  This is the file that should be used by anyone who 
!! is using diag_manager.  No other modules should be used.  If you need to use a routine that is not
!! listed here, please contact the developer in charge of diag_manager.
!!
!!
use fms_diag_data_mod, only: diag_files_type, diag_fields_type
use fms_diag_data_mod, only: monthly, daily, diurnal, yearly, no_diag_averaging, instantaneous, &
     three_hourly, six_hourly, r8, r4, i8, i4, string
use fms_diag_data_mod, only: diag_null, diag_not_found, diag_not_registered, diag_registered_id
use fms_diag_data_mod, only: diag_error,fatal,note,warning

use fms_diag_concur_mod, only: diag_comm_init, fms_write_diag_comm, fms_diag_comm_type
use fms_diag_table_mod !get_diag_table_field
use fms_diag_register_mod
use fms_diag_object_mod, only: operator (>),operator (<),operator (>=),&
                               operator (<=),operator (==),operator (.ne.)
use fms_diag_object_mod, only: fms_diag_object_scalar, fms_diag_object_1d, &
                               fms_diag_object_2d, fms_diag_object_4d, &
                               fms_diag_object_5d
use fms_diag_object_3d_mod, only: fms_diag_object_3d  !!TODO: MZ

use fms_diag_object_mod, only: fms_diag_object
use fms_diag_send_data_mod, only: send_data

implicit none

logical :: fms_diag_manager_initialized = .false.

logical :: verbose = .false. !< When true, more information is printed out
character(len=20) :: diag_table_name = "diag_yaml           "!< The name of the diagnostic list to be parsed
integer :: diag_pes = 0 !< The number of ranks set aside for running diagnostics
logical :: diag_concurrent = .false. !< When true, run the diagnostics concurrently with diag_pes
integer :: MAX_LEN_VARNAME = 20 !< The size of the string of the varname
integer :: MAX_LEN_META = 30 !< The size of the string of each metadata
integer :: MAX_DIAG_VARS = 1024 !< The maximum number of diag variables
logical :: unique_ids = .false. !< Flag if set to true will give each diagnostic a unique ID number. These 
                                !! numbers are limited to MAX_DIAG_VARS
namelist/diag_manager_nml/verbose,diag_table_name,diag_pes,diag_concurrent,MAX_LEN_VARNAME, &
& MAX_LEN_META, MAX_DIAG_VARS, unique_ids

character(len=9) :: read_nml_file="input.nml"
type(fms_diag_comm_type) :: diag_comm !< The diag communicator

!interface fms_send_data
!     module procedure send_data_3d
!end interface fms_send_data


public :: fms_diag_manager_init
public :: fms_diag_object_scalar, fms_diag_object_1d
public :: fms_diag_object_2d, fms_diag_object_3d, fms_diag_object_4d, fms_diag_object_5d
public :: send_data
public :: operator (>), operator (<), operator (>=), operator (<=), operator (==), operator (.ne.)
public :: diag_null, diag_not_found, diag_not_registered, diag_registered_id
private :: diag_comm, diag_error
private :: diag_files_type, diag_fields_type
private :: monthly, daily, diurnal, yearly, no_diag_averaging, instantaneous
private :: three_hourly, six_hourly, r8, r4, i8, i4, string
private :: diag_comm_init, fms_write_diag_comm, fms_diag_comm_type
private :: unique_reg_ids
contains
!> \brief Reads the diag namelist and runs all diag_manager initialization routines
subroutine fms_diag_manager_init

integer :: io_error
integer :: i !< For looping
character (len=10000) :: fms_namelist_string
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! DEAL WITH NAMELIST
open(unit=29,file=read_nml_file,status="old")
read (29, nml=diag_manager_nml, iostat=io_error)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Set up the diag_id lists
 call fms_register_diag_init(MAX_DIAG_VARS, unique_ids)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Initialize and check if a diag_comm will be used
if (diag_concurrent) then
   if (diag_pes <= 0) call diag_error ("diag_manager_nml",&
     "If diag_concurrent=.true., then diag_pes must be greater than 0",fatal)
   call diag_comm_init(diag_comm, diag_pes)
   call fms_write_diag_comm(diag_comm)
endif
!> Parse the diag table and store the diag table information
call fms_diag_table_init(diag_table_name, verbose)

call fms_diag_object_init(MAX_LEN_VARNAME, MAX_LEN_META)

fms_diag_manager_initialized = .true.
end subroutine fms_diag_manager_init
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end module fms_diag_manager2_mod
