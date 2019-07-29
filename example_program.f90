program example_program
  use iso_fortran_env, only : int64, real64, stdout => output_unit
  use tomlc99
  use iso_c_binding, only : c_ptr, c_null_char
  implicit none 

  type(c_ptr)                   :: filePtr, tblPtr, arrPtr
  integer                       :: strLen, arrNelem, idx
  integer(int64)                :: intVal
  double precision              :: dblVal, xVal, yVal, zVal
  character(len=:), allocatable :: strVal
  logical                       :: boolVal
  character                     :: arrType, arrKind, keyType
  
  integer(int64), dimension(:), allocatable :: intArr
  real(real64),   dimension(:), allocatable :: dblArr, xArr, yArr, zArr
  logical,        dimension(:), allocatable :: boolAr
  character(len=:), &
                  dimension(:), allocatable :: strArr

  type(c_ptr),    dimension(:), allocatable :: tArr, aArr

  filePtr = open_file("example_data.toml")

  strLen = get_keyLen_at_index(filePtr, 0)
  write(stdout,'(a,i0)') "first key string length: ", strLen
  allocate(character(strLen) :: strVal)
  call get_keyName_at_index(filePtr,0,strVal)
  write(stdout,'(a,a)') "first key string value: ", strVal

  tblPtr  = table_in(filePtr, "server")

  strLen = get_key_strlen(tblPtr, "host")
  write(stdout,'(a,i0)') "'host' string length: ", strLen
  deallocate(strVal); allocate(character(strLen) :: strVal)
  call get_key_str(tblPtr, "host", strVal)
  write(stdout,'(a,a)') "'host' string value: ", strVal

  call get_key_int(tblPtr, "port", intVal)
  write(stdout,'(a,i0)') "'port' value: ", intVal

  call get_key_dbl(tblPtr, "uptime", dblVal)
  write(stdout,'(a,f0.1)') "'uptime' value: ", dblVal

  call get_key_bool(tblPtr, "enabled", boolVal)
  write(stdout,'(a,l)') "'enabled' value: ", boolVal

  write(stdout,'(/a)') "name    kind  type #elem"
  arrPtr   = array_in(tblPtr, "intArray")
  arrKind  = array_kind(arrPtr) 
  arrType  = array_type(arrPtr) 
  arrNelem = array_nelem(arrPtr) 
  write(stdout, '(a8,a4,a6,i6)') "intArray", arrKind, arrType, arrNelem
  allocate(intArr(arrNelem)); intArr = 0
  call get_array_int(arrPtr, intArr)

  arrPtr   = array_in(tblPtr, "dblArray")
  arrKind  = array_kind(arrPtr) 
  arrType  = array_type(arrPtr) 
  arrNelem = array_nelem(arrPtr) 
  write(stdout, '(a8,a4,a6,i6)') "dblArray", arrKind, arrType, arrNelem
  allocate(dblArr(arrNelem)); dblArr = 0
  call get_array_dbl(arrPtr, dblArr)
  
  arrPtr   = array_in(tblPtr, "boolArray")
  arrKind  = array_kind(arrPtr) 
  arrType  = array_type(arrPtr) 
  arrNelem = array_nelem(arrPtr) 
  write(stdout, '(a9,a3,a6,i6)') "boolArray", arrKind, arrType, arrNelem
  allocate(boolAr(arrNelem)); boolAr = .false.
  call get_array_bool(arrPtr, boolAr)

  arrPtr   = array_in(tblPtr, "strArray")
  arrKind  = array_kind(arrPtr) 
  arrType  = array_type(arrPtr) 
  arrNelem = array_nelem(arrPtr) 
  strLen   = array_strlen(arrPtr)
  write(stdout, '(a8,a4,a6,i6)') "strArray", arrKind, arrType, arrNelem
  allocate(character(strLen) :: strArr(arrNelem))
  call get_array_str(arrPtr, strArr)

  write(stdout, '()')
  write(stdout, '(a,5(i4))') "inArray: ", intArr
  write(stdout, '(a,5(f6.1))') "dblArray: ", dblArr
  write(stdout, '(a,5(l2))') "boolArray: ", boolAr
  write(stdout, '(a,5a8)') "strArray: ", strArr

  write(stdout, '()')
  write(stdout, '(2a)') '"port" type: ', inquire_key_type(tblPtr, "port")
  write(stdout, '(2a)') '"boolArray" type: ', inquire_key_type(tblPtr, "boolArray")
  write(stdout, '(2a)') '"server" type: ', inquire_key_type(filePtr, "server")
  keyType = inquire_key_type(filePtr, "notpresent")
  if (keyType == c_null_char) then
    write(stdout, '(2a)') '"notpresent" type: ', '(c_null_char)'
  else
    write(stdout, '(2a)') '"notpresent" type: ', keyType
  endif

  arrPtr   = array_in(tblPtr, "points")
  arrKind  = array_kind(arrPtr) 
  arrNelem = array_nelem(arrPtr) 
  write(stdout, '(/a,a1,a,i0)') "points: array kind=",arrKind, &
                                    "; #elem=",arrNelem
  allocate(tArr(arrNelem));
  call get_array_tbl(arrPtr, tArr)
  do idx=1,arrNelem
    call get_key_dbl(tArr(idx), "x", xVal)
    call get_key_dbl(tArr(idx), "y", yVal)
    call get_key_dbl(tArr(idx), "z", zVal)
    write(stdout, '(4x,3(f6.1))') xVal, yVal, zVal
  enddo

  arrPtr   = array_in(tblPtr, "alt_points")
  arrKind  = array_kind(arrPtr) 
  arrNelem = array_nelem(arrPtr) 
  write(stdout, '(/a,a1,a,i0)') "alt_points: array kind=",arrKind, &
                                        "; #elem=",arrNelem
  allocate(aArr(arrNelem));
  call get_array_arr(arrPtr, aArr)
  allocate(xArr(array_nelem(aArr(1)))) 
  allocate(yArr(array_nelem(aArr(2)))) 
  allocate(zArr(array_nelem(aArr(3)))) 
  call get_array_dbl(aArr(1), xArr)
  call get_array_dbl(aArr(2), yArr)
  call get_array_dbl(aArr(3), zArr)
  write(stdout, '((4x,3(f6.1)))') (xArr(idx), yArr(idx), zArr(idx), &
                                  idx=1,array_nelem(arrPtr))

end program
