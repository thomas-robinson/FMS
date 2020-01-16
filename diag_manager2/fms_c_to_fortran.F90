module c_to_fortran_mod 
use iso_c_binding

contains
!> \brief Converts a c_char string to a fortran string with type character.
pure function fms_c2f_string (cstring) result(fstring)
 character(c_char), intent(in) :: cstring(*) !< The C string to convert to fortran
 character(len=:), allocatable :: fstring    !< The fortran string returned
 integer :: length !< The string length
 integer :: i
 length = 1
! call c_f_pointer (cstring,
 c_loop: do !> Loop through the C string until you find the C_NULL_CHAR
     if (cstring(length) == c_null_char) then
          length = length - 1 !> When C_NULL_CHAR is found set the correct length
          exit c_loop
     endif
     length = length + 1
 enddo c_loop
 allocate(character(len=length) :: fstring) !> Set the length of fstring
 do i = 1,length !> Copy the strings in the C string array into the fortran string fstring
     fstring(i:i) = cstring(i)
 enddo
 
end function fms_c2f_string


end module c_to_fortran_mod


