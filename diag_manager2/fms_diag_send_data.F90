module fms_diag_send_data_mod

use fms_diag_averaging_mod, only: get_average, alloc_subarray
use fms_diag_data_mod,  only: diag_null, diag_error, fatal, note, warning
use fms_diag_object_mod
use fms2_io_mod,            only: open_file, close_file, write_data, FmsNetcdfFile_t


!> \descrption The user API for diag_manager is send_data.  Users pass their variable object to
!! this routine, and magic happens.
!! - Check if the diagnostic object is allocated/registered.  If it isn't return.
!! - On the first call, the diag object is converted to the correct type to allow for bufferring.
!! - If the input data are static, they are written to the file(s) they are supposed to go to.  Subsequent
!! send data calls will be ignored (a quick return).
!! - The optional arguments are all handled and checked.
!! - Data is buffered in the diag object.  This object is carried by the model component sending the data.
!! - Averaging routines are called to preform the correct calculations if there is averaging.  
!! - Data is written to the appropriate file at the appropriate time.  The averaged data is not buffered.
!! - If no more averaging needs to be done on a given set of data, then the data buffer is deallocated. 
!! (example: a veriable is written out on a monthly basis, and the model has reached the end of the month)
interface send_data
     module procedure fms_send_datascalar
     module procedure fms_send_data1d
     module procedure fms_send_data2d
     module procedure fms_send_data3d
     module procedure fms_send_data4d
     module procedure fms_send_data5d
end interface send_data

type send_data_opts
     integer, allocatable :: is 
     integer, allocatable :: js
     integer, allocatable :: ks
     integer, allocatable :: ie
     integer, allocatable :: je
     integer, allocatable :: ke
     logical, allocatable :: mask
     real, allocatable ::    rmask
     real, allocatable ::    weight
end type send_data_opts


!> \brief Extends the variable object to work with multiple types of data
type, extends(fms_diag_object) :: fms_diag_object_scalar
     class(*), allocatable :: vardata
!contains
    !procedure :: send_data => send_data_scalar
end type fms_diag_object_scalar

type, extends(fms_diag_object) :: fms_diag_object_1d
     class(*), allocatable, dimension(:) :: vardata
!contains
     !procedure :: send_data => send_data_1d
end type fms_diag_object_1d

type, extends(fms_diag_object) :: fms_diag_object_2d
     class(*), allocatable, dimension(:,:) :: vardata
!contains
     !procedure :: send_data => fms_send_data_2d
end type fms_diag_object_2d

type, extends(fms_diag_object) :: fms_diag_object_3d
    class(*), allocatable, dimension(:,:,:) :: vardata
contains
    procedure :: send_data => send_data_3d
end type fms_diag_object_3d

type, extends(fms_diag_object) :: fms_diag_object_4d
    !contains
    !procedure :: send_data => send_data_4d
end type fms_diag_object_4d

type, extends(fms_diag_object) :: fms_diag_object_5d
     class(*), allocatable, dimension(:,:,:,:,:) :: vardata
!contains
     !procedure :: send_data => send_data_5d
end type fms_diag_object_5d

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! variables !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
type(fms_diag_object) :: null_ob
type(fms_diag_object_scalar) :: null_sc
type(fms_diag_object_1d) :: null_1d
type(fms_diag_object_2d) :: null_2d
type(fms_diag_object_3d) :: null_3d
type(fms_diag_object_4d) :: null_4d
type(fms_diag_object_5d) :: null_5d

public :: null_sc, null_1d, null_2d, null_4d, null_5d


contains 
!> \descrption scalar wrapper for fms_send_data
subroutine fms_send_datascalar(diagobj, var)
 class (fms_diag_object),target, intent(inout), allocatable :: diagobj !< The diag variable object
 class(*)                      , intent(in) ,   target      :: var !< The variable
 class(*)                      ,                pointer     :: vptr => NULL() !< A pointer to the data

!> If the diagnostic object is not allocated, then return withut doing anything
 if (.not.allocated( diagobj )) return
end subroutine fms_send_datascalar

subroutine fms_send_data1d(diagobj, var)
 class (fms_diag_object),target, intent(inout), allocatable :: diagobj !< The diag variable object
 class(*), dimension(:)       , intent(in) , target         :: var !< The variable
 class(*), dimension(:)       ,              pointer        :: vptr => NULL() !< A pointer to the data

!> If the diagnostic object is not allocated, then return
 if (.not.allocated( diagobj )) return
!> If this is the first call in, set the type to be fms_diag_object_1d
 call switch_to_right_type(diagobj, null_1d, var(lbound(var,1)))

end subroutine fms_send_data1d

!> \descrption 4D wrapper for fms_send_data
subroutine fms_send_data4d(diagobj, var, time, is_in, js_in, ks_in, mask, &
                                   rmask, ie_in, je_in, ke_in, weight, err_msg)
 class (fms_diag_object),target, intent(inout), allocatable :: diagobj !< The diag variable object
 class(*), dimension(:,:,:,:) , intent(in) , target         :: var !< The variable
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
! local vars
 class(*), dimension(:,:,:,:) ,pointer        :: vptr => NULL() !< A pointer to the data
 class (fms_diag_object)      ,pointer       :: dptr => NULL()

!> If the diagnostic object is not allocated, then return
 if (.not.allocated( diagobj )) return
!> If this is the first call in, set the type to be fms_diag_object_4d
 call switch_to_right_type(diagobj, null_4d, &
     var(lbound(var,1),lbound(var,2),lbound(var,3),lbound(var,4)) )

end subroutine fms_send_data4d

!> \descrption 5D wrapper for fms_send_data
subroutine fms_send_data5d(diagobj, var, time, is_in, js_in, ks_in, mask, &
                                   rmask, ie_in, je_in, ke_in, weight, err_msg)
 class (fms_diag_object),target, intent(inout), allocatable :: diagobj !< The diag variable object
 class(*), dimension(:,:,:,:,:), intent(in) , target       :: var !< The variable
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
! local vars
 class(*), dimension(:,:,:,:,:) ,pointer               :: vptr => NULL() !< A pointer to the data
 class (fms_diag_object)      , pointer                :: dptr => NULL()

!> If the diagnostic object is not allocated, then return
 if (.not.allocated( diagobj )) return
!> If this is the first call in, set the type to be fms_diag_object_5d
 call switch_to_right_type(diagobj, null_5d, &
     var(lbound(var,1),lbound(var,2),lbound(var,3),lbound(var,4),lbound(var,5)) )

end subroutine fms_send_data5d

!> \descrption 2D wrapper for fms_send_data
subroutine fms_send_data2d (diagobj, var, time, is_in, js_in, ks_in, mask, &
                                   rmask, ie_in, je_in, ke_in, weight, err_msg)
 class (fms_diag_object),target, intent(inout), allocatable :: diagobj !< The diag variable object
 class(*), dimension(:,:)   , intent(in) , target         :: var !< The variable
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
! local vars
 class(*), dimension(:,:)       ,pointer        :: vptr => NULL() !< A pointer to the data
 class (fms_diag_object)        ,pointer        :: dptr => NULL()
!> If this is the first call in, set the type to be fms_diag_object_2d
 call switch_to_right_type(diagobj, null_2d, var(lbound(var,1),lbound(var,2)) )

end subroutine fms_send_data2d

!> \descrption 3D wrapper for fms_send_data
subroutine fms_send_data3d (diagobj, var, varname, time, is_in, js_in, ks_in, mask, &
                                   rmask, ie_in, je_in, ke_in, weight, err_msg)
 class (fms_diag_object),target, intent(inout), allocatable :: diagobj !< The diag variable object
 class(*), dimension(:,:,:)   , intent(in) , target         :: var !< The variable
 character(len=*),              intent(in)                  :: varname  !!TODO: Is this neccesary
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
! local vars
 class(*), dimension(:,:,:)   ,pointer                      :: vptr => NULL() !< A pointer to the data
 class (fms_diag_object_3d)   ,pointer                      :: dptr => NULL()

 !
 !! If the diagnostic object is not allocated, then return
 if (.not. allocated( diagobj )) then
     call diag_error("fms_send_data3d", "The diag object " // diagobj%get_varname() // &
         " is not allocated", WARNING)
 endif
 return

 !! If this is the first call in, set the type to be fms_diag_object_3d
 !! first check if already of type 3d
 call switch_to_right_type(diagobj, null_3d, &
    var(lbound(var,1),lbound(var,2),lbound(var,3)))

     !! If the diagnostic object is not registered, then return.
 if(.not. diagobj%is_registeredB())then
     call diag_error("fms_send_data3d", "The diag object " // diagobj%get_varname() // &
         " is not registered", WARNING)
     return
 endif

!> Write the object if its static
!> TODO: Only if its not yet writtennetcd
if ( .not. diagobj%is_static() ) then
    call diagobj%send_data(time, is_in, js_in, ks_in, mask, &
                            rmask, ie_in, je_in, ke_in, weight, err_msg)
else
    call diag_error("fms_send_data3d", "dynamic snd_data" // diagobj%get_varname() // &
          " is not yet implemented", WARNING)
endif
end subroutine fms_send_data3d



!> \Description This routine is used to switch the diag object to the correct type based on the dimensions
!! of the input data.  It then determines the type of the data, and stores that within the object.  Only
!! one datum needs to be send in to determine the data type, so this routine is shared by all of the
!! send_data calls.
!! This only needs to be done one time. 
subroutine switch_to_right_type(diagobj, null_obj,var)
 class (fms_diag_object),target, intent(inout), allocatable :: diagobj !< The diag variable object
 class (fms_diag_object),intent(in)                         :: null_obj !< The null object that matches the diagnostic
 class (*)              ,intent (in)                        :: var
 class (fms_diag_object), allocatable                       :: dcopy
 class (fms_diag_object), pointer                           :: dptr => NULL()
 class (fms_diag_object), allocatable                      :: dcopy_3d
 
 dptr => diagobj
 select type (dptr)
     type is (fms_diag_object)
!> Switch the diagobj to the correct type
          dcopy = null_obj !< Copy null into the temp object to set the type
          call dptr%copy(dcopy) !> Copy the values of the object into the tFmsNetcdfFile_temp dcopy
          deallocate(diagobj) !> Deallocate the object
          diagobj = null_obj !> Copy the null into the variable object to set the type
          call dcopy%copy(diagobj) !> Copy the original values in
!> Determne the type of var
          call diagobj%set_type(var)
 end select
 if (allocated(dcopy)) deallocate(dcopy)
 if (associated(dptr)) nullify(dptr) !> Dont leak memory

end subroutine switch_to_right_type



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!
!!! Dimentional extension of fms_diag_obj:

!> Procedure which contains code that is common to the send_data_nx routines.
subroutine send_data_generic(diag_obj, time, is_in, js_in, ks_in, mask, &
                         rmask, ie_in, je_in, ke_in, weight, err_msg)
    class (fms_diag_object),    intent(in)                  :: diag_obj
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

end subroutine send_data_generic

!> Register (when necessary) and write all the data (including axis infortmation) to
!!  all the files specified in the object.
!! TODO: This is just a static version
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
    type(diag_axis_type) :: an_axis
    type(fms_io_obj)     :: a_file
    type (FmsNetcdfFile_t) :: the_file  !! TODO:: There are additional types (e.g. FmsNetcdfDomainFile_t)
    character(len=*), parameter :: metadata_name = "metadata" !! TODO: Settle on an enum file - no magic nums.

    do fn = 1, size( diag_obj%fms_fileobj )
        a_file =  diag_obj%fms_fileobj ( fn )

        opened = open_file(the_file, a_file%fname, "append")
        if (.not. opened) then
            call diag_error("diag_mgr_wite_static", "The file " //the_file%path &
                // " was not opened", FATAL)
            EXIT !! Break out of loop. TODO: Discuss return policy, error codes, etc.
        end if

        !!TODO: Cnsider moving the next few lines to diag_object
        !!TODO: Needs test to see if already written
        call send_attribute_meta(diag_obj, the_file)

        !!Loop over axis and register and write data and metadata
        !!Each axis first determins if its already written, etc.
        do  an = 1,size(diag_obj%axis)
            an_axis = diag_obj%axis(an)
            an_axis_id = diag_obj%get_axis_rid( an, fn )
            call an_axis%send_data(the_file, diag_obj%get_varname())
        end do

        !!And write the data
        !!select type (pd => diag_obj%vardata)
        call write_data(the_file,  diag_obj%get_varname(), diag_obj%vardata)

        !!And calculate and write the average
        !! call alloc_subarray(var, avg);
        !! TODO: include weight, min, max, etc
        ! call get_average(var, avg)
        ! call get_average(var, avg, is_in, ie_in, js_in, je_in, ks_in, ke_in)
        ! call write_data(the_file, diag_obj%get_varname ,avg  )


  end do !!Over file objects

end subroutine

!> \brief Write the field attributes (file metadata) to file.
!! Note: there is a similar function for axis
subroutine send_attribute_meta(diag_obj, fileob)
    class (fms_diag_object),    intent(in) :: diag_obj
    class(FmsNetcdfFile_t), intent(inout)  :: fileob
    !! TODO: Fill in function body
    ! local vars
    character(len=:), allocatable         ::vname
    vname = diag_obj%get_varname()
    DO i = 1, size(diag_obj%metadata)
        !! Note: Original function in diag_output.FO:write_axis_meta had select over attribute%type
        !!call register_variable_attribute(fileob, varname,TRIM(attributes(i))  , att_str(1:att_len))
    end do
end subroutine send_attribute_meta



end module fms_diag_send_data_mod
