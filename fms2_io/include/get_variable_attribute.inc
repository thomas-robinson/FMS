!> @brief Get the value of a variable's attribute.
subroutine get_variable_attribute_0d(fileobj, variable_name, attribute_name, &
                                     attribute_value, broadcast)

  class(FmsNetcdfFile_t), intent(in) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  character(len=*), intent(in) :: attribute_name !< Attribute name.
  class(*), intent(inout) :: attribute_value !< Attribute value
  logical, intent(in), optional :: broadcast !< Flag controlling whether or
                                             !! not the data will be
                                             !! broadcasted to non
                                             !! "I/O root" ranks.
                                             !! The broadcast will be done
                                             !! by default.

  integer :: varid
  integer :: err
  character(len=1024), dimension(1) :: buffer

  if (fileobj%is_root) then
    varid = get_variable_id(fileobj%ncid, trim(variable_name))
    select type(attribute_value)
      type is (character(len=*))
        err = nf90_get_att(fileobj%ncid, varid, trim(attribute_name), buffer(1))
      type is (integer(kind=int32))
        err = nf90_get_att(fileobj%ncid, varid, trim(attribute_name), attribute_value)
      type is (integer(kind=int64))
        err = nf90_get_att(fileobj%ncid, varid, trim(attribute_name), attribute_value)
      type is (real(kind=real32))
        err = nf90_get_att(fileobj%ncid, varid, trim(attribute_name), attribute_value)
      type is (real(kind=real64))
        err = nf90_get_att(fileobj%ncid, varid, trim(attribute_name), attribute_value)
      class default
        call error("unsupported type.")
    end select
    call check_netcdf_code(err)
  endif
  if (present(broadcast)) then
    if (.not. broadcast) then
      return
    endif
  endif
  select type(attribute_value)
    type is (character(len=*))
      call mpp_broadcast(buffer, len(buffer(1)), fileobj%io_root, pelist=fileobj%pelist)
      call string_copy(attribute_value, buffer(1))
    type is (integer(kind=int32))
      call mpp_broadcast(attribute_value, fileobj%io_root, pelist=fileobj%pelist)
    type is (integer(kind=int64))
      call mpp_broadcast(attribute_value, fileobj%io_root, pelist=fileobj%pelist)
    type is (real(kind=real32))
      call mpp_broadcast(attribute_value, fileobj%io_root, pelist=fileobj%pelist)
    type is (real(kind=real64))
      call mpp_broadcast(attribute_value, fileobj%io_root, pelist=fileobj%pelist)
    class default
      call error("unsupported type.")
  end select
end subroutine get_variable_attribute_0d


!> @brief Get the value of a variable's attribute.
subroutine get_variable_attribute_1d(fileobj, variable_name, attribute_name, &
                                     attribute_value, broadcast)

  class(FmsNetcdfFile_t), intent(in) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  character(len=*), intent(in) :: attribute_name !< Attribute name.
  class(*), dimension(:), intent(inout) :: attribute_value !< Attribute value
  logical, intent(in), optional :: broadcast !< Flag controlling whether or
                                             !! not the data will be
                                             !! broadcasted to non
                                             !! "I/O root" ranks.
                                             !! The broadcast will be done
                                             !! by default.

  integer :: varid
  integer :: err

  if (fileobj%is_root) then
    varid = get_variable_id(fileobj%ncid, trim(variable_name))
    select type(attribute_value)
      type is (integer(kind=int32))
        err = nf90_get_att(fileobj%ncid, varid, trim(attribute_name), attribute_value)
      type is (integer(kind=int64))
        err = nf90_get_att(fileobj%ncid, varid, trim(attribute_name), attribute_value)
      type is (real(kind=real32))
        err = nf90_get_att(fileobj%ncid, varid, trim(attribute_name), attribute_value)
      type is (real(kind=real64))
        err = nf90_get_att(fileobj%ncid, varid, trim(attribute_name), attribute_value)
      class default
        call error("unsupported type.")
    end select
    call check_netcdf_code(err)
  endif
  if (present(broadcast)) then
    if (.not. broadcast) then
      return
    endif
  endif
  select type(attribute_value)
    type is (integer(kind=int32))
      call mpp_broadcast(attribute_value, size(attribute_value), fileobj%io_root, &
                         pelist=fileobj%pelist)
    type is (integer(kind=int64))
      call mpp_broadcast(attribute_value, size(attribute_value), fileobj%io_root, &
                         pelist=fileobj%pelist)
    type is (real(kind=real32))
      call mpp_broadcast(attribute_value, size(attribute_value), fileobj%io_root, &
                         pelist=fileobj%pelist)
    type is (real(kind=real64))
      call mpp_broadcast(attribute_value, size(attribute_value), fileobj%io_root, &
                         pelist=fileobj%pelist)
    class default
      call error("unsupported type.")
  end select
end subroutine get_variable_attribute_1d