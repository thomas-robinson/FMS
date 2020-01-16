program diag_table_test

use fms_diag_table_mod, only: fms_diag_table_init

character(len=100) :: input_nml

!call fms_diag_table_init()
write (6,*) "Running diag_yaml test"
open (unit=29, file="test/input.nml", &
          status="old", access="stream")
read (29,iostat=i) input_nml
write (6,*) "Call fms_diag_table_init"
call fms_diag_table_init(input_nml)
write (6,*) " Finished fms_diag_table_init"


end program diag_table_test
