module fms_diag_util_mod
implicit none
!> Miscellanoeous utility functions.
!!TODO: Whats the best place to put these?

contains

!> Make and return a string (character array) representation of an integer
function int_to_cs(i) result(res)
    character(:),allocatable :: res
    integer,intent(in) :: i
    character(range(i)+2) :: tmp
    write(tmp,'(i0)') i
    res = trim(tmp)
end function int_to_cs

  !> Make and return a string (character array) representation of a logcal
function logical_to_cs(ii) result(res)
    character(:),allocatable :: res
    logical ,intent(in) :: ii
    character(1) :: tmp
    if (ii) then
        res = "T"
    else
        res = "F"
    end if
 end function logical_to_cs

end module fms_diag_util_mod



