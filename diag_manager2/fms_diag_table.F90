module fms_diag_table_mod

use fms_diag_data_mod, only: diag_files_type, diag_fields_type, diag_error, diag_null
use fms_diag_data_mod, only: fatal,note,warning
use c_to_fortran_mod, only: fms_c2f_string
use iso_c_binding
implicit none

public :: diag_table, fms_diag_table_init, fms_write_diag_table, is_field_type_null

 character(len=:),allocatable :: diag_table



integer :: num_diag_files, num_diag_fields
type(diag_files_type),  allocatable, dimension(:) :: diag_files 
type(diag_fields_type), allocatable, dimension(:) :: diag_fields 
type(diag_fields_type)                            :: null_field_type

logical :: diag_table_init = .false.



!> Interface with C routines wrapping libyaml
interface 
!> Checks the diag_yaml file
 integer function diag_parse_check (fname) bind(C, name="diag_parse_check")
        use iso_c_binding
        character(kind=c_char)  :: fname !< The name of the diag_yaml file
 end function diag_parse_check
!> Outputs the number of files and fields
 subroutine diag_num_files (fname, ifiles_p, ifields_p) bind(C, name="diag_num_files")
        use iso_c_binding
        character(kind=c_char)  :: fname !< The name of the diag_yaml file
        integer(c_int)          :: ifiles_p !< The number of files listed in the diag_yaml
        integer(c_int)          :: ifields_p !< The number of fields listed in the diag_yaml
 end subroutine diag_num_files
!> Fills in the information from the diag_yaml related to the diag_file_type  
 subroutine diag_get_file_info (fname, diag_files_fortran,diag_fields_fortran,i,j) bind(C, name="diag_get_file_info")
        use iso_c_binding
        import
        character(kind=c_char) :: fname !< The name of the diag_yaml file
        type(diag_files_type) :: diag_files_fortran !< Matches the struct filled in with the diag_file_info
        type(diag_fields_type) :: diag_fields_fortran !< Matches the struct filled in with the diag_file_info
!        type(c_ptr) :: diag_files_fortran
        integer(c_int),value :: i !< The index of the diag_file array
        integer(c_int),value :: j !< The index of the diag_field array

 end subroutine diag_get_file_info
end interface


contains
!> Initializaes the diag_table by reading the diag_table_nml, reading in the diag_table, parsing 
!! the diag_table and getting the relevant information by filling in the diag_file and diag_field
!! derived type arrays 
subroutine fms_diag_table_init (diag_yaml_name,verb)

character(len=*), intent(in)            :: diag_yaml_name        !< The name of the namelist file
logical         , intent(in), optional  :: verb                  !< If true, more output is printed
logical                                 :: verbose               !< If true, more output is printed
integer                                 :: c_err                 !< Error code from C function
integer                                 :: i,j                   !< Indexing integer
character(len=:),allocatable            :: char1, char2          !< Utility strings
if (present(verb)) then
     verbose = verb
else
     verbose = .false.
endif
!> If already initalized, return
if (diag_table_init) then
     if(verbose)call diag_error("fms_diag_table_mod","diag_table already initialized",NOTE)
     return
endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Check the diag_table file
 c_err = diag_parse_check(trim(diag_yaml_name)//c_null_char)
 if (c_err .ne. 0) then
        if (c_err == 1) call diag_error("fms_diag_table_mod","parser error",FATAL)
        if (c_err == 2) call diag_error("fms_diag_table_mod",diag_yaml_name//" error opening",FATAL)        
 endif
!> get the number of files and fields in the diag_yaml 
 call diag_num_files(trim(diag_yaml_name)//c_null_char, num_diag_files, num_diag_fields)

 if (verbose) then
     allocate(character(len=8) :: char1)
     allocate(character(len=8) :: char2)
     write(char1,'(I8)') num_diag_files
     write(char2,'(I8)') num_diag_fields
     call diag_error("diag_table_init","There are "//trim(char2)//" fields in "//trim(char1)//&
                     " diag files.",NOTE)
     deallocate(char1) ; deallocate(char2)
 endif
 allocate(diag_files(num_diag_files) )
 allocate(diag_fields(num_diag_fields) )
!> get the diag_yaml file info
 if (num_diag_files >= num_diag_fields) then
     j = 0
     do i = 1,num_diag_files
          j = j + 1
          if (j > num_diag_fields) j = num_diag_fields
          call diag_get_file_info(trim(diag_yaml_name)//c_null_char, diag_files(i),diag_fields(j), i-1, j-1)
     enddo
 else
     i = 0
     do j = 1,num_diag_fields
          i = i+1
          if (i > num_diag_files) i = num_diag_files
          call diag_get_file_info(trim(diag_yaml_name)//c_null_char, diag_files(i),diag_fields(j), i-1, j-1)
     enddo     
 endif
!> Initialize the NULL types
null_field_type%ikind = DIAG_NULL
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> Set the init value to .true.
diag_table_init = .true.
if (verbose) call diag_error("fms_diag_table_mod","Initialized!",NOTE)
end subroutine fms_diag_table_init
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
type(diag_fields_type)function get_diag_table_field (field_name) result (field)
 character(len=*), intent(IN) :: field_name
 integer :: i
 do i = 1,size(diag_fields)
!  write(6,*) trim(field_name),trim(fms_c2f_string(diag_fields(i)%fname)),"-"
!  write (6,*) len(trim(field_name)),len(fms_c2f_string(diag_fields(i)%fname))
!  write (6,*) diag_fields(i)%fname, trim(field_name) == fms_c2f_string(diag_fields(i)%fname)
     if (trim(field_name) == trim(fms_c2f_string(diag_fields(i)%fname))) then
          field = diag_fields(i)
write (6,*) field_name//" Found"

          return
     endif
 enddo
 field = null_field_type

end function get_diag_table_field
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> \brief Compares two field type variables
pure logical function is_field_type_null (in1)
type(diag_fields_type), intent(in) :: in1
is_field_type_null = (in1%ikind == DIAG_NULL) 
end function is_field_type_null
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine fms_write_diag_table 
!character(len=*), intent(in)                 :: diag_table_name 
!character(len=:), intent(out), allocatable   :: diag_table_string

end subroutine fms_write_diag_table

end module fms_diag_table_mod
