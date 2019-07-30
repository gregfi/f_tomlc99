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
  character                     :: arrType, arrKind, keyType, valType
  
  integer(int64), dimension(:), allocatable :: intArr
  real(real64),   dimension(:), allocatable :: dblArr, xArr, yArr, zArr
  logical,        dimension(:), allocatable :: boolAr
  character(len=:), &
                  dimension(:), allocatable :: strArr

  type(c_ptr),    dimension(:), allocatable :: tArr, aArr

  filePtr = toml_parse_file("example_data.toml")

  strLen = toml_get_keyLen_at_index(filePtr, 0)
  write(stdout,'(a,i0)') "first key string length: ", strLen
  allocate(character(strLen) :: strVal)
  call toml_get_keyName_at_index(filePtr,0,strVal)
  write(stdout,'(a,a)') "first key string value: ", strVal

  tblPtr = toml_table_in(filePtr, "server")

  strLen = toml_get_val_strlen(tblPtr, "host")
  valType= toml_inquire_val_type(tblPtr, "host")
  deallocate(strVal); allocate(character(strLen) :: strVal)
  call toml_get_val_str(tblPtr, "host", strVal)
  write(stdout,'(/a,a,a,i0,a,a)') "'host' type=",valType, &
          "; length=", strLen, "; value=", strVal

  valType= toml_inquire_val_type(tblPtr, "port")
  call toml_get_val_int(tblPtr, "port", intVal)
  write(stdout,'(3a,i0)') "'port' type=", valType, "; value=", intVal

  valType= toml_inquire_val_type(tblPtr, "uptime")
  call toml_get_val_dbl(tblPtr, "uptime", dblVal)
  write(stdout,'(3a,f0.1)') "'uptime' type=", valType, "; value=", dblVal

  valType= toml_inquire_val_type(tblPtr, "enabled")
  call toml_get_val_bool(tblPtr, "enabled", boolVal)
  write(stdout,'(3a,l)') "'enabled' type=", valType, "; value=", boolVal

  write(stdout,'(/a)') "name    kind  type #elem"
  arrPtr   = toml_array_in(tblPtr, "intArray")
  arrKind  = toml_array_kind(arrPtr) 
  arrType  = toml_array_type(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(a8,a4,a6,i6)') "intArray", arrKind, arrType, arrNelem
  allocate(intArr(arrNelem)); intArr = 0
  call toml_get_array_int(arrPtr, intArr)

  arrPtr   = toml_array_in(tblPtr, "dblArray")
  arrKind  = toml_array_kind(arrPtr) 
  arrType  = toml_array_type(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(a8,a4,a6,i6)') "dblArray", arrKind, arrType, arrNelem
  allocate(dblArr(arrNelem)); dblArr = 0
  call toml_get_array_dbl(arrPtr, dblArr)
  
  arrPtr   = toml_array_in(tblPtr, "boolArray")
  arrKind  = toml_array_kind(arrPtr) 
  arrType  = toml_array_type(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(a9,a3,a6,i6)') "boolArray", arrKind, arrType, arrNelem
  allocate(boolAr(arrNelem)); boolAr = .false.
  call toml_get_array_bool(arrPtr, boolAr)

  arrPtr   = toml_array_in(tblPtr, "strArray")
  arrKind  = toml_array_kind(arrPtr) 
  arrType  = toml_array_type(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  strLen   = toml_array_strlen(arrPtr)
  write(stdout, '(a8,a4,a6,i6)') "strArray", arrKind, arrType, arrNelem
  allocate(character(strLen) :: strArr(arrNelem))
  call toml_get_array_str(arrPtr, strArr)

  write(stdout, '()')
  write(stdout, '(a,5(i4))') "inArray: ", intArr
  write(stdout, '(a,5(f6.1))') "dblArray: ", dblArr
  write(stdout, '(a,5(l2))') "boolArray: ", boolAr
  write(stdout, '(a,5a8)') "strArray: ", strArr

  write(stdout, '()')
  write(stdout, '(2a)') "'port' kind: ", toml_inquire_key_kind(tblPtr, "port")
  write(stdout, '(2a)') "'boolArray' kind: ", toml_inquire_key_kind(tblPtr, "boolArray")
  write(stdout, '(2a)') "'server' kind: ", toml_inquire_key_kind(filePtr, "server")
  keyType = toml_inquire_key_kind(filePtr, "notpresent")
  if (keyType == c_null_char) then
    write(stdout, '(2a)') "'notpresent' kind: ", '(c_null_char)'
  else
    write(stdout, '(2a)') "'notpresent' kind: ", keyType
  endif

  arrPtr   = toml_array_in(tblPtr, "points")
  arrKind  = toml_array_kind(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(/a,a1,a,i0)') 'points: array kind=',arrKind, &
                                    '; #elem=',arrNelem
  allocate(tArr(arrNelem));
  call toml_get_array_tbl(arrPtr, tArr)
  do idx=1,arrNelem
    call toml_get_val_dbl(tArr(idx), "x", xVal)
    call toml_get_val_dbl(tArr(idx), "y", yVal)
    call toml_get_val_dbl(tArr(idx), "z", zVal)
    write(stdout, '(4x,3(f6.1))') xVal, yVal, zVal
  enddo

  arrPtr   = toml_array_in(tblPtr, "alt_points")
  arrKind  = toml_array_kind(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(/a,a1,a,i0)') 'alt_points: array kind=',arrKind, &
                                        '; #elem=',arrNelem
  allocate(aArr(arrNelem));
  call toml_get_array_arr(arrPtr, aArr)
  allocate(xArr(toml_array_nelem(aArr(1)))) 
  allocate(yArr(toml_array_nelem(aArr(2)))) 
  allocate(zArr(toml_array_nelem(aArr(3)))) 
  call toml_get_array_dbl(aArr(1), xArr)
  call toml_get_array_dbl(aArr(2), yArr)
  call toml_get_array_dbl(aArr(3), zArr)
  write(stdout, '((4x,3(f6.1)))') (xArr(idx), yArr(idx), zArr(idx), &
                                  idx=1,toml_array_nelem(arrPtr))

end program
