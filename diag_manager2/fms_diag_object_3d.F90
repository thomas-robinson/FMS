module fms_diag_object_3d_mod

use fms_diag_object_mod,    only: fms_diag_object
use fms_diag_data_mod,      only: diag_null, diag_error, fatal, note, warning
use fms_diag_data_mod,      only: fms_io_obj !!TODO: Temporary
use fms_diag_axis_mod,      only: diag_axis_type
use fms_diag_averaging_mod, only: get_average, alloc_subarray
use fms2_io_mod,            only: open_file, close_file, write_data, FmsNetcdfFile_t

implicit none

type, extends(fms_diag_object) :: fms_diag_object_3d
    class(*), allocatable, dimension(:,:,:) :: vardata
contains
    procedure :: send_data => send_data_3d
    procedure :: print_testc => diag_obj_3d_print_testc
end type fms_diag_object_3d

contains

subroutine diag_obj_3d_print_testc(this, iv)
    class (fms_diag_object_3d), intent(in)  :: this
    class(*), intent(in)      :: iv
end subroutine

!! This is just a static version
subroutine send_data_3d(diag_obj, time, is_in, js_in, ks_in, mask, &
                         rmask, ie_in, je_in, ke_in, weight, err_msg)
    class (fms_diag_object_3d),    intent(in)                  :: diag_obj
    integer, optional            , intent(in)                  :: time !< A time place holder
    integer, optional            , intent(in)                  :: is_in !< Start of the first dimension
    integer, optional            , intent(in)                  :: js_in !< Start of the second dimension
    integer, optional            , intent(in)                  :: ks_in !< Start of the third dimension
    integer, optional            , intent(in)                  :: ie_in !< End of the first dimension
    integer, optional            , intent(in)                  :: je_in !< End of the second dimension
    integer, optional            , intent(in)                  :: ke_in !< End of the third dimension
    logical, optional            , intent(in)                  :: mask !< A lask for point to ignore
    real   , optional            , intent(in)                  :: rmask !< I DONT KNOW
    real   , optional            , intent(in)                  :: weight !< Something for averaging?
    CHARACTER(len=*)             , INTENT(out), OPTIONAL       :: err_msg
    !! local vars
    class(*), dimension(:,:), allocatable ::  avg
    logical              :: opened
    integer              :: fn, an, an_axis_id
    type(diag_axis_type) :: an_axis !One axis
    type(fms_io_obj)     :: a_file
    type (FmsNetcdfFile_t) :: the_file  !! TODO:: How about FmsNetcdfDomainFile_t ?
    character(len=*), parameter :: metadata_name = "metadata" !! TODO: Settle on an enum file - no magic nums.
    !!class(*), dimension(:,:,:), pointer  :: pvdata

    !!pvdata => diag_obj%vardata
    do fn = 1, size( diag_obj%fms_fileobj )
        a_file =  diag_obj%fms_fileobj ( fn )
        opened = open_file(the_file, a_file%fname, "append")
        if (.not. opened) then
            call diag_error("diag_mgr_wite_static", "The file " //the_file%path &
                // " was not opened", FATAL)
            EXIT !! Break out of loop. TODO: Discuss return policy, error codes, etc.
        end if

        !! Write Metadata.
        call write_data(the_file, metadata_name, diag_obj%metadata)

        !! Loop over axis and register and write
        do an  = 1,size(diag_obj%axis)
            an_axis = diag_obj%axis(an)
            an_axis_id = diag_obj%get_axis_rid( an, fn )
            call an_axis%send_data(the_file)
        end do

        !!TODO Select over type shoul dnot be necessary as using polymorphism?
        select type (pd => diag_obj%vardata)
            type is (real)   !! class is vs. type is
                call write_data(the_file,  diag_obj%get_varname(), pd)
                !! Calculate and write the average
                !! call alloc_subarray(var, avg);
                !! TODO: include weight
                ! call get_average(var, avg)
                ! call get_average(var, avg, is_in, ie_in, js_in, je_in, ks_in, ke_in)
                ! call write_data(the_file, diag_obj%get_varname ,avg  )
            class default
                call diag_error("write_data", "type not supported yet", FATAL)
        end select

  end do !!Over file objects

end subroutine

end module fms_diag_object_3d_mod
