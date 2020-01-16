module fms_diag_write_data_mod

use fms_diag_data_mod, only: fms_io_obj !!TODO: Temporary
use fms_diag_data_mod,  only: diag_null, diag_error, fatal, note, warning
use fms_diag_object_mod, only: fms_diag_object, fms_diag_object_3d

use fms_diag_axis_mod, only: diag_axis_type
use fms_diag_averaging_mod, only: get_average, alloc_subarray
!!use fms2_io_mod, only: open_file, close_file, write_data, FmsNetcdfFile_t
use fms2_io_mod
contains

subroutine write_static (diag_obj, var, varname, time, is_in, js_in, ks_in, mask, &
                                   rmask, ie_in, je_in, ke_in, weight, err_msg)
    class (fms_diag_object),target, intent(inout), allocatable :: diag_obj !< The diag variable object
    real(kind=8), dimension(:,:,:)   , intent(in) , target     :: var !< The variable !!TODO char(*) vs real
    character(len=*), intent(in)                               :: varname  !!TODO: Is this neccesary

    integer, optional            , intent(in)                  :: time !< A time place holder
    integer, optional            , intent(in)                  :: is_in !< Start of the first dimension
    integer, optional            , intent(in)                  :: js_in !< Start of the second dimension
    integer, optional            , intent(in)                  :: ks_in !< Start of the third dimension
    integer, optional            , intent(in)                  :: ie_in !< End of the first dimension
    integer, optional            , intent(in)                  :: je_in !< End of the second dimension
    integer, optional            , intent(in)                  :: ke_in !< End of the third dimension
    logical, optional           , intent(in)                   :: mask !< A lask for point to ignore
    real   , optional            , intent(in)                  :: rmask !< I DONT KNOW
    real   , optional            , intent(in)                  :: weight !< Something for averaging?
    CHARACTER(len=*)             , INTENT(out), OPTIONAL       :: err_msg
    !! local vars
    !!class(*), dimension(:,:), allocatable ::  avg   !!TODO: why class(*)
    real(kind=8), dimension(:,:), allocatable ::  avg
    logical              :: opened
    integer              :: fn, an, i_v
    type(diag_axis_type) :: an_axis !One axis
    type (fms_io_obj)    :: a_file
    type (FmsNetcdfFile_t) :: the_file
    character(len=*), parameter :: metadata_name = "metadata" !! TODO: How do you get it

    !! TODO: *** Why is diagobj%fms_fileobj an array ?
    do fn = 1, size( diag_obj%fms_fileobj )
        a_file =  diag_obj%fms_fileobj ( fn )
        !! See netcdf_file_open_wrap(fileobj, path, mode,...from fms2_io
        opened = open_file(the_file, a_file%fname, "append")
        if (.not. opened) then
            call diag_error("diag_mgr_wite_static", "The file " //the_file%path &
            // " was not opened", FATAL)
        end if
    end do

    !! Register the axss
    do an  = 1,size(diag_obj%axis)
        an_axis = diag_obj%axis( an )
        !!...(use new dm2routine, see diyor)
    end do

    !! Write the metadata
    call write_data(the_file, metadata_name, diag_obj%metadata)

    !! Write the axes info
    do an  = 1,size(diag_obj%axis)
        an_axis = diag_obj%axis( an )
        call write_data(the_file, an_axis%aname, an_axis%adata)
    end do


    !!TODO should not have to select over type use polymorphism?
    select type (diag_obj)
        type is (fms_diag_object_3d)   !! class is vs. type is
            call alloc_subarray(var, avg);
            !! TODO: include weight
            call get_average(var, avg)
            !call get_average(var, avg, is_in, ie_in, js_in, je_in, ks_in, ke_in)
            !! TODO: Just wite the average or the data?
            !! call write_data(the_file, diag_obj%vardata) !!TODO: Need variable name (at least)
            call write_data(the_file, varname ,avg  ) !!TODO: Whats the variable name
        class default
            call diag_error("write_data", "type not supported yet", FATAL)
    end select

end subroutine  write_static

end module fms_diag_write_data_mod

!see diag_util.F90 subroutine write_static(file) L2689
!see  SUBROUTINE diag_data_out(file, field, dat, time, final_call_in, static_write_in)
! L2510 2565 2581
!see fms2_io ...
