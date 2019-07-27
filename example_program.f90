program example_program
  use iso_fortran_env, only : int64, stdout => output_unit
  use tomlc99
  use iso_c_binding, only : c_ptr
  implicit none 

  type(c_ptr)                   :: filePtr, tblPtr
  integer                       :: ierr, strLen
  integer(int64)                :: intVal
  double precision              :: dblVal
  character(len=:), allocatable :: strVal

  filePtr = open_file("example_data.toml")
  tblPtr  = table_in(filePtr, "server", ierr)

  strLen = get_key_strlen(tblPtr, "host")
  write(stdout,'(i0)') strLen
  allocate(character(strLen) :: strVal)
  call get_key_str(tblPtr, "host", strVal, ierr)
  write(stdout,'(a)') strVal

  call get_key_int(tblPtr, "port", intVal, ierr)
  write(stdout,'(i0)') intVal

  call get_key_dbl(tblPtr, "time", dblVal, ierr)
  write(stdout,'(f0.1)') dblVal

  strLen = get_keyLen_at_index(filePtr, 0)
  write(stdout,'(i0)') strLen
  deallocate(strVal)
  allocate(character(strLen) :: strVal)
  call get_keyName_at_index(filePtr,0,strVal,ierr)
  write(stdout,'(a)')  strVal

end program
