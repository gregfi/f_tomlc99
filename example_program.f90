program example_program
  use iso_fortran_env, only : int64, real64, stdout => output_unit
  use tomlc99
  use iso_c_binding, only : c_ptr
  implicit none 

  type(c_ptr)                   :: filePtr, tblPtr, arrPtr
  integer                       :: strLen, arrNelem
  integer(int64)                :: intVal
  double precision              :: dblVal
  character(len=:), allocatable :: strVal
  logical                       :: boolVal
  character                     :: arrType, arrKind
  
  integer(int64), dimension(:), allocatable :: intArr
  real(real64),   dimension(:), allocatable :: dblArr

  filePtr = open_file("example_data.toml")
  tblPtr  = table_in(filePtr, "server")

  strLen = get_key_strlen(tblPtr, "host")
  write(stdout,'(i0)') strLen
  allocate(character(strLen) :: strVal)
  call get_key_str(tblPtr, "host", strVal)
  write(stdout,'(a)') strVal

  call get_key_int(tblPtr, "port", intVal)
  write(stdout,'(i0)') intVal

  call get_key_dbl(tblPtr, "time", dblVal)
  write(stdout,'(f0.1)') dblVal

  call get_key_bool(tblPtr, "enabled", boolVal)
  write(stdout,*) boolVal

  strLen = get_keyLen_at_index(filePtr, 0)
  write(stdout,'(i0)') strLen
  deallocate(strVal)
  allocate(character(strLen) :: strVal)
  call get_keyName_at_index(filePtr,0,strVal)
  write(stdout,'(a)')  strVal

  arrPtr   = array_in(tblPtr, "intA")
  arrKind  = array_kind(arrPtr) 
  arrType  = array_type(arrPtr) 
  arrNelem = array_nelem(arrPtr) 
  write(stdout, '(a1,1x,a1,1x,i0)') arrKind, arrType, arrNelem
  allocate(intArr(arrNelem)); intArr = 0
  call get_array_int(arrPtr, intArr)
  write(stdout, '(5(i4))') intArr

  arrPtr   = array_in(tblPtr, "dblA")
  arrKind  = array_kind(arrPtr) 
  arrType  = array_type(arrPtr) 
  arrNelem = array_nelem(arrPtr) 
  write(stdout, '(a1,1x,a1,1x,i0)') arrKind, arrType, arrNelem
  allocate(dblArr(arrNelem)); dblArr = 0
  call get_array_dbl(arrPtr, dblArr)
  write(stdout, '(5(f6.1))') dblArr

end program
