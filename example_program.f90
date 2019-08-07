program example_program
  use iso_fortran_env, only : int64, real64, stdout => output_unit
  use tomlc99
  use iso_c_binding, only : c_ptr, c_null_char
  implicit none 

  type(c_ptr)                   :: filePtr, tblPtr, arrPtr
  integer(int32)                :: nkval, narr, ntab
  integer(int32)                :: strLen, arrNelem, idx
  integer(int64)                :: intVal
  double precision              :: dblVal, xVal, yVal, zVal
  character(len=:), allocatable :: strVal, keyStr
  character(len=:, kind=ucs4), &
                    allocatable :: ucsStr
  logical                       :: boolVal
  character                     :: arrType, arrKind, keyType, valType
  type(toml_time)               :: timeVal
  character(len=:), allocatable :: timeStr
  
  integer(int64), dimension(:), allocatable :: intArr
  real(real64),   dimension(:), allocatable :: dblArr, xArr, yArr, zArr
  logical,        dimension(:), allocatable :: boolAr
  character(len=:), &
                  dimension(:), allocatable :: strArr

  type(c_ptr),    dimension(:), allocatable :: tArr, aArr
  type(toml_time),dimension(:), allocatable :: dateAr  

  ! set encoding; parse the file
  open(stdout,encoding='UTF-8')
  filePtr = toml_parse_file("example_data.toml")

  ! retrieve the first key name and print it 
  call toml_get_key_at_index(filePtr,0,keyStr)
  write(stdout,'(a,i0)') "first key string length: ", len(keyStr)
  write(stdout,'(a,a)') "first key string value: ", keyStr

  ! parse the "server" table; retrieve the first key name and print it 
  tblPtr = toml_table_in(filePtr, "server")
  call toml_array_key(tblPtr,keyStr)
  write(stdout,'(/a,i0)') "table key string length: ", len(keyStr)
  write(stdout,'(a,a)') "table key string value: ", keyStr

  ! print number of constituent vals, arrays, tables
  narr = toml_table_narr(tblPtr)
  ntab = toml_table_ntab(tblPtr)
  nkval= toml_table_nkval(tblPtr)
  write(stdout,'(/"nkval=",i0,"; narr=",i0,"; ntab=",i0)') nkval, narr, ntab

  ! retrieve and print the "host" key (string); note that the "special"
  ! characters at the end print correctly, but would need to be decoded from
  ! UTF-8 to UCS to be operated upon
  valType=toml_inquire_val_type(tblPtr, "host")
  call toml_get_val_str(tblPtr, "host", strVal)
  write(stdout,'(/a,a,a,i0,a,a)') "'host' type=",valType, &
          "; length=", len(strVal), "; value=", strVal

  ! retrieve and print the "port" key (int64)
  valType= toml_inquire_val_type(tblPtr, "port")
  call toml_get_val_int(tblPtr, "port", intVal)
  write(stdout,'(3a,i0)') "'port' type=", valType, "; value=", intVal

  ! retrieve and print the "uptime" key (real64)
  valType= toml_inquire_val_type(tblPtr, "uptime")
  call toml_get_val_dbl(tblPtr, "uptime", dblVal)
  write(stdout,'(3a,f0.1)') "'uptime' type=", valType, "; value=", dblVal

  ! retrieve and print the "enabled" key (logical)
  valType= toml_inquire_val_type(tblPtr, "enabled")
  call toml_get_val_bool(tblPtr, "enabled", boolVal)
  write(stdout,'(3a,l)') "'enabled' type=", valType, "; value=", boolVal

  ! retrieve and print the "ldt1" key (toml_time)
  valType= toml_inquire_val_type(tblPtr, "ldt1")
  call toml_get_val_ts(tblPtr, "ldt1", timeVal)
  call toml_timestamp_to_string(timeVal, timeStr)
  write(stdout,'(4a)') "'ldt1' type=", valType, "; value=",timeStr

  ! convert the "key" string (google translated from Korean) into UTF-8
  call toml_utf8_encode_str(char(int(z'D0A4'), ucs4), keyStr)

  ! retrieve the value using the UTF-8-encoded keyStr
  valType=toml_inquire_val_type(tblPtr, keyStr)
  call toml_get_val_str(tblPtr, keyStr, strVal)

  ! decode the UTF-8 into UCS
  call toml_utf8_decode_str(strVal, ucsStr)

  ! write the summary of Korean-language key and value extraction
  write(stdout,'(5a,i0,3a,i0)') "'",char(int(z'D0A4'), ucs4),&
                              "' type=",valType, &
                              "; length=", len(ucsStr), &
                              "; raw_value=", strVal, &
                              "; unicode_int=", ichar(ucsStr(1:1),int64)

  ! test whether strings are ascii or not
  write(stdout,'(/a,l)') "'host' is ascii: ", &
          toml_inquire_string_is_ascii("host")
  write(stdout,'(a,l)') "'" // keyStr // "' is ascii: ", &
          toml_inquire_string_is_ascii(keyStr)

  ! write column headers for array data
  write(stdout,'(/a)') "name    kind  type #elem"

  ! retrieve integer array parameters and values
  arrPtr   = toml_array_in(tblPtr, "intArray")
  call toml_array_key(arrPtr, keyStr) 
  arrKind  = toml_array_kind(arrPtr) 
  arrType  = toml_array_type(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  call toml_get_array_int(arrPtr, intArr)
  write(stdout, '(a8,a4,a6,i6)') keyStr, arrKind, arrType, arrNelem

  ! retrieve double array parameters and values
  arrPtr   = toml_array_in(tblPtr, "dblArray")
  call toml_array_key(arrPtr, keyStr) 
  arrKind  = toml_array_kind(arrPtr) 
  arrType  = toml_array_type(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(a8,a4,a6,i6)') keyStr, arrKind, arrType, arrNelem
  call toml_get_array_dbl(arrPtr, dblArr)
  
  ! retrieve logical array parameters and values
  arrPtr   = toml_array_in(tblPtr, "boolArray")
  call toml_array_key(arrPtr, keyStr) 
  arrKind  = toml_array_kind(arrPtr) 
  arrType  = toml_array_type(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(a9,a3,a6,i6)') keyStr, arrKind, arrType, arrNelem
  call toml_get_array_bool(arrPtr, boolAr)

  ! retrieve toml_time array parameters and values
  arrPtr   = toml_array_in(tblPtr, "dateArray")
  call toml_array_key(arrPtr, keyStr) 
  arrKind  = toml_array_kind(arrPtr) 
  arrType  = toml_array_type(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(a9,a3,a6,i6)') keyStr, arrKind, arrType, arrNelem
  call toml_get_array_time(arrPtr, dateAr)

  ! retrieve string array parameters and values
  arrPtr   = toml_array_in(tblPtr, "strArray")
  call toml_array_key(arrPtr, keyStr) 
  arrKind  = toml_array_kind(arrPtr) 
  arrType  = toml_array_type(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  strLen   = toml_array_strlen(arrPtr)
  write(stdout, '(a8,a4,a6,i6)') keyStr, arrKind, arrType, arrNelem
  call toml_get_array_str(arrPtr, strArr)

  ! print array values
  write(stdout, '()')
  write(stdout, '(a,5(i4))') "intArray: ", intArr
  write(stdout, '(a,5(f6.1))') "dblArray: ", dblArr
  write(stdout, '(a,5(l2))') "boolArray: ", boolAr

  write(stdout, '(a)', advance="no") "dateArray: "
  do idx=1,size(dateAr)
    call toml_timestamp_to_string(dateAr(idx), timeStr)
    write(stdout, '(a,1x)', advance="no") timeStr
  enddo 
  write(stdout, '()')

  write(stdout, '(a,5a8)') "strArray: ", strArr

  ! print key kinds
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

  ! retrieve and print array-of-tables
  arrPtr   = toml_array_in(tblPtr, "points")
  arrKind  = toml_array_kind(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(/a,a1,a,i0)') 'points: array kind=',arrKind, &
                                    '; #elem=',arrNelem
  call toml_get_array_tbl(arrPtr, tArr)
  do idx=1,arrNelem
    call toml_get_val_dbl(tArr(idx), "x", xVal)
    call toml_get_val_dbl(tArr(idx), "y", yVal)
    call toml_get_val_dbl(tArr(idx), "z", zVal)
    write(stdout, '(4x,3(f6.1))') xVal, yVal, zVal
  enddo

  ! retrieve and print array-of-arrays
  arrPtr   = toml_array_in(tblPtr, "alt_points")
  arrKind  = toml_array_kind(arrPtr) 
  arrNelem = toml_array_nelem(arrPtr) 
  write(stdout, '(/a,a1,a,i0)') 'alt_points: array kind=',arrKind, &
                                        '; #elem=',arrNelem
  call toml_get_array_arr(arrPtr, aArr)
  call toml_get_array_dbl(aArr(1), xArr)
  call toml_get_array_dbl(aArr(2), yArr)
  call toml_get_array_dbl(aArr(3), zArr)
  write(stdout, '((4x,3(f6.1)))') (xArr(idx), yArr(idx), zArr(idx), &
                                  idx=1,toml_array_nelem(arrPtr))

  call toml_free(filePtr)

end program
