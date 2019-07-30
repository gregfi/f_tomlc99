module tomlc99
  use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, &
                                            stderr=>error_unit, &
                                            int32, int64, real64
  use iso_c_binding

  implicit none 

  integer(int32), parameter :: maxStrLen = 262144

  interface

    ! interfaces to standard C library functions

    function c_fopen(fileName, mode) bind(C,name="fopen")
      import                 :: c_ptr, c_char
      type(c_ptr)            :: c_fopen
      character(kind=c_char) :: fileName, mode
    end function

    function c_fclose(filePtr) bind(C,name="fclose")
      import                 :: c_ptr, c_int
      type(c_ptr), value     :: filePtr
      integer(c_int)         :: c_fclose
    end function

    subroutine c_free(ptr) bind(C,name="free")
      import                 :: c_ptr
      type(c_ptr), value     :: ptr
    end subroutine

    ! interfaces to standard tomlc99 functions

    function tomlc99_toml_parse_file(filePtr, errBuf, errBufSz) &
             bind(C,name="toml_parse_file")
      import                 :: c_ptr, c_char, c_int
      type(c_ptr)            :: tomlc99_toml_parse_file
      type(c_ptr), value     :: filePtr
      character(kind=c_char) :: errBuf(*)
      integer(c_int)         :: errBufSz 
    end function 

    function tomlc99_toml_key_in(tblPtr, keyIdx) bind(C,name="toml_key_in")
      import                 :: c_ptr, c_int
      type(c_ptr)            :: tomlc99_toml_key_in
      type(c_ptr), value     :: tblPtr
      integer(c_int), value  :: keyIdx
    end function 

    function tomlc99_toml_raw_in(dataPtr, keyName) bind(C,name="toml_raw_in")
      import                 :: c_ptr, c_char
      type(c_ptr)            :: tomlc99_toml_raw_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: keyName(*)
    end function 

    function tomlc99_toml_table_in(dataPtr, tableName) &
             bind(C,name="toml_table_in")
      import                 :: c_ptr, c_char
      type(c_ptr)            :: tomlc99_toml_table_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: tableName(*)
    end function 

    function tomlc99_toml_array_in(dataPtr, arrayName) &
             bind(C,name="toml_array_in")
      import                 :: c_ptr, c_char
      type(c_ptr)            :: tomlc99_toml_array_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: arrayName(*)
    end function 

    function tomlc99_toml_array_kind(arrPtr) &
             bind(C,name="toml_array_kind")
      import                 :: c_ptr, c_char
      type(c_ptr), value     :: arrPtr
      character(kind=c_char) :: tomlc99_toml_array_kind
    end function 

    function tomlc99_toml_array_type(arrPtr) &
             bind(C,name="toml_array_type")
      import                 :: c_ptr, c_char
      type(c_ptr), value     :: arrPtr
      character(kind=c_char) :: tomlc99_toml_array_type
    end function 

    function tomlc99_toml_array_nelem(arrPtr) &
             bind(C,name="toml_array_nelem")
      import                 :: c_ptr, c_int 
      integer(c_int)         :: tomlc99_toml_array_nelem
      type(c_ptr), value     :: arrPtr
    end function 

    function tomlc99_toml_raw_at(arrPtr, idx) &
             bind(C,name="toml_raw_at")
      import                 :: c_ptr, c_int 
      type(c_ptr)            :: tomlc99_toml_raw_at
      type(c_ptr), value     :: arrPtr
      integer(c_int), value  :: idx
    end function 

    function tomlc99_toml_array_at(arrPtr, idx) &
             bind(C,name="toml_array_at")
      import                 :: c_ptr, c_int 
      type(c_ptr)            :: tomlc99_toml_array_at
      type(c_ptr), value     :: arrPtr
      integer(c_int), value  :: idx
    end function 

    function tomlc99_toml_table_at(arrPtr, idx) &
             bind(C,name="toml_table_at")
      import                 :: c_ptr, c_int 
      type(c_ptr)            :: tomlc99_toml_table_at
      type(c_ptr), value     :: arrPtr
      integer(c_int), value  :: idx
    end function 

    function tomlc99_toml_rtos(raw, outStr) bind(C,name="toml_rtos")
      import                 :: c_int, c_ptr, c_char
      integer(c_int)         :: tomlc99_toml_rtos
      type(c_ptr), value     :: raw
      type(c_ptr)            :: outStr
    end function 

    function tomlc99_toml_rtoi(raw, outInt) bind(C,name="toml_rtoi")
      import                 :: c_ptr, c_int, c_int64_t
      integer(c_int)         :: tomlc99_toml_rtoi
      type(c_ptr), value     :: raw
      integer(c_int64_t)     :: outInt
    end function 

    function tomlc99_toml_rtod(raw, outDbl) bind(C,name="toml_rtod")
      import                 :: c_ptr, c_int, c_double
      integer(c_int)         :: tomlc99_toml_rtoi
      type(c_ptr), value     :: raw
      real(c_double)         :: outDbl
    end function 

    function tomlc99_toml_rtob(raw, outBool) bind(C,name="toml_rtob")
      import                 :: c_ptr, c_int, c_bool
      integer(c_int)         :: tomlc99_toml_rtob
      type(c_ptr), value     :: raw
      logical(c_bool)        :: outBool
    end function 

  end interface

  contains

  function toml_parse_file(fileName)

    ! description: opens a TOML file named "fileName" and parses the data;
    !              returns a c pointer to the root-level "toml_table_t" data 
    !              structure. Errors are fatal.

    type(c_ptr)                     :: toml_parse_file
    character(len=*), intent(in)    :: fileName
    type(c_ptr)                     :: fh
    character(len=512, kind=c_char) :: errBuf

    fh = c_fopen(trim(fileName) // c_null_char, &
                 c_char_"r" // c_null_char)

    if (c_associated(fh) .eqv. .false.) then
      write(stderr,101) trim(fileName)
      error stop
    endif

    toml_parse_file = tomlc99_toml_parse_file(fh, errBuf, &
                                              len(errBuf, kind=c_int))

    if (c_associated(toml_parse_file) .eqv. .false.) then
      write(stderr,102) trim(fileName)
      call write_error_buffer(errBuf)
      error stop
    endif
   
    if (c_fclose(fh) /= 0) then
      write(stderr,103) trim(fileName)
      error stop
    endif

    101 format ('ERROR: Failed to open ',a)
    102 format ('ERROR: Failed to parse ',a)
    103 format ('ERROR: Failed to close ',a)

  end function

  subroutine write_error_buffer(errBuf)

    ! description: writes error messages associated with the "parse" operation

    character(len=*, kind=c_char), intent(in) :: errBuf
    integer :: idx

    do idx=1,len(errBuf)
      if (errBuf(idx:idx) == c_null_char) exit
    enddo
    write(stderr, '(a)') errBuf(1:idx)

  end subroutine

  function toml_table_in(inTblPtr, tblName)

    ! description: returns a c pointer to the table with name "tblName" 
    !              contained in the "toml_table_t" structure referenced
    !              by "inTblPtr". Returns c_null_ptr if "tblName" is not found.

    type(c_ptr)                   :: toml_table_in
    type(c_ptr), intent(in)       :: inTblPtr
    character(len=*), intent(in)  :: tblName

    toml_table_in = tomlc99_toml_table_in(inTblPtr, &
                                          tblName // c_null_char)

  end function

  function toml_array_in(inTblPtr, arrayName)

    ! description: returns a c pointer to the array with name "arrayName" 
    !              contained in the "toml_table_t" structure referenced
    !              by "inTblPtr". Returns c_null_ptr if "arrayName" is not found.

    type(c_ptr)                   :: toml_array_in
    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: arrayName

    toml_array_in = tomlc99_toml_array_in(inTblPtr, &
                                          arrayName // c_null_char)

  end function

  function toml_array_kind(inArrPtr)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              and returns the kind; 'v' for value, 'a' for array, and
    !              't' for table. 

    character                     :: toml_array_kind
    type(c_ptr), intent(in)       :: inArrPtr

    character(kind=c_char)        :: c_kind

    c_kind          = tomlc99_toml_array_kind(inArrPtr)
    toml_array_kind = c_kind

  end function

  function toml_array_type(inArrPtr)

    ! description: for array type 'v', accepts a pointer to a "toml_array_t" 
    !              data structure and returns the value type; 'i' for int, 'd'
    !              for double, 'b' for bool, 's' for string

    character                     :: toml_array_type
    type(c_ptr), intent(in)       :: inArrPtr
    character(kind=c_char)        :: c_kind

    c_kind          = tomlc99_toml_array_type(inArrPtr)
    toml_array_type = c_kind
 
  end function

  function toml_array_nelem(inArrPtr)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              structure and returns the value type; 'i' for int, 'd'
    !              for double, 'b' for bool, 's' for string

    integer                       :: toml_array_nelem
    type(c_ptr), intent(in)       :: inArrPtr

    integer(c_int)                :: c_nelem

    c_nelem          = tomlc99_toml_array_nelem(inArrPtr)
    toml_array_nelem = c_nelem

  end function

  subroutine toml_get_array_int(inArrPtr, outArray)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              structure and returns an integer array. A fatal error
    !              is issued if the parameters do not match

    type(c_ptr),                  intent(in)  :: inArrPtr
    integer(int64), dimension(:), intent(out) :: outArray
    
    integer(c_int64_t), dimension(:), allocatable :: c_outArray
    integer(c_int)                                :: c_nelem, c_idx, c_ierr
    character(kind=c_char)                        :: c_kind, c_type
    type(c_ptr)                                   :: tmpRaw
    integer                                       :: idx
    
    c_nelem     = tomlc99_toml_array_nelem(inArrPtr)
    c_kind      = tomlc99_toml_array_kind(inArrPtr)
    c_type      = tomlc99_toml_array_type(inArrPtr)

    if (c_nelem /= size(outArray)) then
      write(stderr,101) c_nelem, size(outArray)
      error stop
    endif

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      error stop
    endif

    if (c_type /= 'i') then
      write(stderr,103) c_type, 'i'
      error stop
    endif

    allocate(c_outArray(0:c_nelem-1)); c_outArray = 0

    do idx=1,c_nelem
      c_idx  = idx - 1
      tmpRaw = tomlc99_toml_raw_at(inArrPtr, c_idx)
      c_ierr = tomlc99_toml_rtoi(tmpRaw, c_outArray(c_idx))
    enddo

    outArray = c_outArray

    101 format ('ERROR: the size of the toml array data (',i0,') does not ',&
                'match the size of output array (',i0,').')
    102 format ('ERROR: array has kind "',a,'" but "',a,'" is required.')
    103 format ('ERROR: array has type "',a,'" but "',a,'" is required.')

  end subroutine

  subroutine toml_get_array_dbl(inArrPtr, outArray)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              structure and returns an double array. A fatal error
    !              is issued if the parameters do not match

    type(c_ptr),                 intent(in) :: inArrPtr
    real(real64), dimension(:), intent(out) :: outArray
    
    real(c_double), dimension(:), allocatable :: c_outArray
    integer(c_int)                            :: c_nelem, c_idx, c_ierr
    character(kind=c_char)                    :: c_kind, c_type
    type(c_ptr)                               :: tmpRaw
    integer                                   :: idx
    
    c_nelem     = tomlc99_toml_array_nelem(inArrPtr)
    c_kind      = tomlc99_toml_array_kind(inArrPtr)
    c_type      = tomlc99_toml_array_type(inArrPtr)

    if (c_nelem /= size(outArray)) then
      write(stderr,101) c_nelem, size(outArray)
      error stop
    endif

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      error stop
    endif

    if (c_type /= 'd') then
      write(stderr,103) c_type, 'd'
      error stop
    endif

    allocate(c_outArray(0:c_nelem-1)); c_outArray = 0

    do idx=1,c_nelem
      c_idx  = idx - 1
      tmpRaw = tomlc99_toml_raw_at(inArrPtr, c_idx)
      c_ierr = tomlc99_toml_rtod(tmpRaw, c_outArray(c_idx))
    enddo

    outArray = c_outArray

    101 format ('ERROR: the size of the toml array data (',i0,') does not ',&
                'match the size of output array (',i0,').')
    102 format ('ERROR: array has kind "',a,'" but "',a,'" is required.')
    103 format ('ERROR: array has type "',a,'" but "',a,'" is required.')

  end subroutine

  subroutine toml_get_array_bool(inArrPtr, outArray)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              structure and returns an logical array. A fatal error
    !              is issued if the parameters do not match

    type(c_ptr),                intent(in)  :: inArrPtr
    logical,      dimension(:), intent(out) :: outArray
    
    logical(kind=c_bool),&
                    dimension(:), allocatable :: c_outArray
    integer(c_int)                            :: c_nelem, c_idx, c_ierr
    character(kind=c_char)                    :: c_kind, c_type
    type(c_ptr)                               :: tmpRaw
    integer                                   :: idx
    
    c_nelem     = tomlc99_toml_array_nelem(inArrPtr)
    c_kind      = tomlc99_toml_array_kind(inArrPtr)
    c_type      = tomlc99_toml_array_type(inArrPtr)

    if (c_nelem /= size(outArray)) then
      write(stderr,101) c_nelem, size(outArray)
      error stop
    endif

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      error stop
    endif

    if (c_type /= 'b') then
      write(stderr,103) c_type, 'b'
      error stop
    endif

    allocate(c_outArray(0:c_nelem-1)); c_outArray = .false.

    do idx=1,c_nelem
      c_idx  = idx - 1
      tmpRaw = tomlc99_toml_raw_at(inArrPtr, c_idx)
      c_ierr = tomlc99_toml_rtob(tmpRaw, c_outArray(c_idx))
    enddo

    outArray = c_outArray

    101 format ('ERROR: the size of the toml array data (',i0,') does not ',&
                'match the size of output array (',i0,').')
    102 format ('ERROR: array has kind "',a,'" but "',a,'" is required.')
    103 format ('ERROR: array has type "',a,'" but "',a,'" is required.')

  end subroutine

  function toml_array_strlen(inArrPtr)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              structure and returns the maximum string length in the
    !              array. A fatal error is issued if the parameters do not match

    integer                     :: toml_array_strlen
    type(c_ptr), intent(in)     :: inArrPtr
    
    integer(c_int)              :: c_nelem, c_idx, c_ierr
    character(kind=c_char)      :: c_kind, c_type
    type(c_ptr)                 :: tmpRaw, c_outStr
    integer                     :: idx, tmpStrLen
    character(len=maxStrLen), &
                        pointer :: fstring
    
    toml_array_strlen = 0

    c_nelem           = tomlc99_toml_array_nelem(inArrPtr)
    c_kind            = tomlc99_toml_array_kind(inArrPtr)
    c_type            = tomlc99_toml_array_type(inArrPtr)

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      error stop
    endif

    if (c_type /= 's') then
      write(stderr,103) c_type, 's'
      error stop
    endif

    do idx=1,c_nelem

      c_idx  = idx - 1
      tmpRaw = tomlc99_toml_raw_at(inArrPtr, c_idx)
      c_ierr = tomlc99_toml_rtos(tmpRaw, c_outStr)

      call c_f_pointer(c_outStr, fstring)
      tmpStrLen = index(fstring, c_null_char)-1
      call c_free(c_outStr)

      toml_array_strlen = max(toml_array_strlen, tmpStrLen)

    enddo

    102 format ('ERROR: array has kind "',a,'" but "',a,'" is required.')
    103 format ('ERROR: array has type "',a,'" but "',a,'" is required.')

  end function

  subroutine toml_get_array_str(inArrPtr, outArray)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              structure and returns the a string array. A fatal error is 
    !              issued if the parameters do not match

    type(c_ptr),                intent(in)  :: inArrPtr
    character(len=*), dimension(:), &
                                intent(out) :: outArray
    
    integer(c_int)                          :: c_nelem, c_idx, c_ierr
    character(kind=c_char)                  :: c_kind, c_type
    type(c_ptr)                             :: tmpRaw, c_outStr
    integer                                 :: idx, tmpStrLen, maxLen
    character(len=maxStrLen),   pointer     :: fstring
    
    c_nelem     = tomlc99_toml_array_nelem(inArrPtr)
    c_kind      = tomlc99_toml_array_kind(inArrPtr)
    c_type      = tomlc99_toml_array_type(inArrPtr)

    if (c_nelem /= size(outArray)) then
      write(stderr,101) c_nelem, size(outArray)
      error stop
    endif

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      error stop
    endif

    if (c_type /= 's') then
      write(stderr,103) c_type, 's'
      error stop
    endif

    maxLen = toml_array_strlen(inArrPtr)
    if (len(outArray) /= maxLen) then
      write(stderr,104) maxLen, len(outArray)
      error stop
    endif

    do idx=1,c_nelem

      c_idx  = idx - 1
      tmpRaw = tomlc99_toml_raw_at(inArrPtr, c_idx)
      c_ierr = tomlc99_toml_rtos(tmpRaw, c_outStr)

      call c_f_pointer(c_outStr, fstring)
      tmpStrLen = index(fstring, c_null_char)-1
      outArray(idx) = fstring(1:tmpStrLen)

      call c_free(c_outStr)

    enddo

    101 format ('ERROR: the size of the toml array data (',i0,') does not ',&
                'match the size of output array (',i0,').')
    102 format ('ERROR: array has kind "',a,'" but "',a,'" is required.')
    103 format ('ERROR: array has type "',a,'" but "',a,'" is required.')
    104 format ('ERROR: the maximum string length of the toml array data (',i0, &
                ') does not match the size of output array (',i0,').')

  end subroutine

  subroutine toml_get_array_tbl(inArrPtr, outArray)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              structure and returns an array of pointers to subordinate
    !              tables. A fatal error is issued if the parameters do not 
    !              match.

    type(c_ptr),                intent(in)  :: inArrPtr
    type(c_ptr), dimension(:),  intent(out) :: outArray
    
    integer(c_int)                          :: c_nelem, c_idx
    character(kind=c_char)                  :: c_kind
    integer                                 :: idx
    
    c_nelem     = tomlc99_toml_array_nelem(inArrPtr)
    c_kind      = tomlc99_toml_array_kind(inArrPtr)

    if (c_nelem /= size(outArray)) then
      write(stderr,101) c_nelem, size(outArray)
      error stop
    endif

    if (c_kind /= 't') then
      write(stderr,102) c_kind, 't'
      error stop
    endif

    do idx=1,c_nelem

      c_idx  = idx - 1
      outArray(idx) = tomlc99_toml_table_at(inArrPtr, c_idx)

    enddo

    101 format ('ERROR: the size of the toml array data (',i0,') does not ',&
                'match the size of output array (',i0,').')
    102 format ('ERROR: array has kind "',a,'" but "',a,'" is required.')

  end subroutine

  subroutine toml_get_array_arr(inArrPtr, outArray)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              structure and returns an array of pointers to subordinate
    !              arrays. A fatal error is issued if the parameters do not 
    !              match.

    type(c_ptr),                intent(in)  :: inArrPtr
    type(c_ptr), dimension(:),  intent(out) :: outArray
    
    integer(c_int)                          :: c_nelem, c_idx
    character(kind=c_char)                  :: c_kind
    integer                                 :: idx
    
    c_nelem     = tomlc99_toml_array_nelem(inArrPtr)
    c_kind      = tomlc99_toml_array_kind(inArrPtr)

    if (c_nelem /= size(outArray)) then
      write(stderr,101) c_nelem, size(outArray)
      error stop
    endif

    if (c_kind /= 'a') then
      write(stderr,102) c_kind, 'a'
      error stop
    endif

    do idx=1,c_nelem

      c_idx  = idx - 1
      outArray(idx) = tomlc99_toml_array_at(inArrPtr, c_idx)

    enddo

    101 format ('ERROR: the size of the toml array data (',i0,') does not ',&
                'match the size of output array (',i0,').')
    102 format ('ERROR: array has kind "',a,'" but "',a,'" is required.')

  end subroutine

  function toml_get_val_strlen(inTblPtr, keyName)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and a key name for a string; returns the string length.
    !              If the parameters do not match, a fatal error is issued.

    integer(int32)                :: toml_get_val_strlen
    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 

    type(c_ptr)                   :: tmpRaw
    character                     :: valType
    integer(c_int)                :: c_ierr = 0
    type(c_ptr)                   :: c_outStr

    character(len=maxStrLen), pointer :: fstring

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      error stop
    endif

    valType = toml_inquire_val_type(inTblPtr, trim(keyName) // c_null_char)
    if (valType /= "s") then
      write(stderr,102) trim(keyName), valType, "s"
      error stop
    endif

    c_ierr = tomlc99_toml_rtos(tmpRaw, c_outStr)

    call c_f_pointer(c_outStr, fstring)
    toml_get_val_strlen =  index(fstring, c_null_char)-1
    call c_free(c_outStr)

    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Key "',a,'" has type "',a,&
                        '", but the output array is type "',a,'"')

  end function

  subroutine toml_get_val_str(inTblPtr, keyName, outVal)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and a key name for a string; returns the string value.
    !              If the parameters do not match, a fatal error is issued.

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    character(len=*), intent(out) :: outVal

    type(c_ptr)                    :: tmpRaw
    character                      :: valType
    integer(c_int)                 :: c_ierr = 0
    type(c_ptr)                    :: c_outStr

    character(len=maxStrLen), pointer  :: fstring

    integer :: strLen = 0

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      error stop
    endif

    valType = toml_inquire_val_type(inTblPtr, trim(keyName) // c_null_char)
    if (valType /= "s") then
      write(stderr,102) trim(keyName), valType, "s"
      error stop
    endif

    c_ierr = tomlc99_toml_rtos(tmpRaw, c_outStr)

    call c_f_pointer(c_outStr, fstring)
    strLen =  index(fstring, c_null_char)-1

    if (strLen /= len(outVal)) then
      write(stderr,103) trim(keyName)
      error stop
    endif

    outVal = fstring(1:strLen)
    call c_free(c_outStr)

    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Key "',a,'" has type "',a,&
                        '", but the output array is type "',a,'"')
    103 format ('ERROR: Output string length does not match TOML data for key: ',a)

  end subroutine

  subroutine toml_get_val_int(inTblPtr, keyName, outVal)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and a key name for an int; returns the int value.
    !              If the parameters do not match, a fatal error is issued.

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    integer(int64),   intent(out) :: outVal

    type(c_ptr)                   :: tmpRaw
    character                     :: valType
    integer(c_int)                :: c_ierr = 0
    integer(c_int64_t)            :: c_outVal = 0

    outVal = 0
    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      error stop
    endif

    valType = toml_inquire_val_type(inTblPtr, trim(keyName) // c_null_char)
    if (valType /= "i") then
      write(stderr,102) trim(keyName), valType, "i"
      error stop
    endif

    c_ierr = tomlc99_toml_rtoi(tmpRaw, c_outVal)

    if (c_ierr == -1) then
      write(stderr,103) trim(keyName)
      error stop
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Key "',a,'" has type "',a,&
                        '", but the output array is type "',a,'"')
    103 format ('ERROR: Failed integer conversion for key: ',a)

  end subroutine

  subroutine toml_get_val_dbl(inTblPtr, keyName, outVal)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and a key name for a double; returns the double value.
    !              If the parameters do not match, a fatal error is issued.

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    real(real64),     intent(out) :: outVal

    type(c_ptr)                   :: tmpRaw
    character                     :: valType
    integer(c_int)                :: c_ierr = 0
    real(c_double)                :: c_outVal = 0

    outVal = 0

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      error stop
    endif

    valType = toml_inquire_val_type(inTblPtr, trim(keyName) // c_null_char)
    if (valType /= "d") then
      write(stderr,102) trim(keyName), valType, "d"
      error stop
    endif

    c_ierr = tomlc99_toml_rtod(tmpRaw, c_outVal)

    if (c_ierr == -1) then
      write(stderr,103) trim(keyName)
      error stop
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Key "',a,'" has type "',a,&
                        '", but the output array is type "',a,'"')
    103 format ('ERROR: Failed double conversion for key: ',a)

  end subroutine

  subroutine toml_get_val_bool(inTblPtr, keyName, outVal)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and a key name for a bool; returns the logical value.
    !              If the parameters do not match, a fatal error is issued.

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    logical,          intent(out) :: outVal

    type(c_ptr)                   :: tmpRaw
    character                     :: valType
    integer(c_int)                :: c_ierr = 0
    logical(kind=c_bool)          :: c_outVal

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      error stop
    endif

    valType = toml_inquire_val_type(inTblPtr, trim(keyName) // c_null_char)
    if (valType /= "b") then
      write(stderr,102) trim(keyName), valType, "b"
      error stop
    endif

    c_ierr = tomlc99_toml_rtob(tmpRaw, c_outVal)

    if (c_ierr == -1) then
      write(stderr,103) trim(keyName)
      error stop
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Key "',a,'" has type "',a,&
                        '", but the output array is type "',a,'"')
    103 format ('ERROR: Failed bool conversion for key: ',a)

  end subroutine

  function toml_get_keyLen_at_index(inTblPtr, keyIndex)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and an index number (starting at 1; Fortran convention);
    !              returns the string length of the key name.

    integer(int32)              :: toml_get_keyLen_at_index
    type(c_ptr),    intent(in)  :: inTblPtr
    integer(int32), intent(in)  :: keyIndex

    integer(c_int)              :: c_idx
    type(c_ptr)                 :: tmpKey
    character(len=maxStrLen), &
                        pointer :: fstring

    c_idx  = keyIndex
    tmpKey = tomlc99_toml_key_in(inTblPtr, c_idx)
    call c_f_pointer(tmpKey, fstring)
    toml_get_keyLen_at_index = index(fstring, c_null_char)-1

  end function

  subroutine toml_get_keyName_at_index(inTblPtr, keyIndex, keyName)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and an index number (starting at 1; Fortran convention);
    !              returns the string value of the key name. If a length
    !              mismatch is detected, a fatal error is issued.

    type(c_ptr),      intent(in)  :: inTblPtr
    integer(int32),   intent(in)  :: keyIndex
    character(len=*), intent(out) :: keyName

    integer(c_int)                :: c_idx
    type(c_ptr)                   :: tmpKey
    character(len=maxStrLen), &
                        pointer   :: fstring

    integer(int32)                :: keyLen

    c_idx  = keyIndex
    tmpKey = tomlc99_toml_key_in(inTblPtr, c_idx)
    call c_f_pointer(tmpKey, fstring)
    keyLen = index(fstring, c_null_char)-1

    if (keyLen /= len(keyName)) then
      write(stderr,101) trim(keyName)
      error stop
    endif

    keyName = fstring(1:keyLen)

    101 format ('ERROR: Output string length does not match TOML data for key: ',a)

  end subroutine

  function toml_inquire_key_kind(inTblPtr, keyName)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and an key name; returns 'v' for value, 'a' for array, 
    !              't' for table, or c_null_char for key-not-found

    character                    :: toml_inquire_key_kind
    type(c_ptr),      intent(in) :: inTblPtr
    character(len=*), intent(in) :: keyName

    type(c_ptr)                  :: tmpRaw

    toml_inquire_key_kind = c_null_char

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)
    if (c_associated(tmpRaw) .eqv. .true.) then
      toml_inquire_key_kind = "v"
      return
    endif

    tmpRaw = tomlc99_toml_array_in(inTblPtr, trim(keyName) // c_null_char)
    if (c_associated(tmpRaw) .eqv. .true.) then
      toml_inquire_key_kind = "a"
      return
    endif

    tmpRaw = tomlc99_toml_table_in(inTblPtr, trim(keyName) // c_null_char)
    if (c_associated(tmpRaw) .eqv. .true.) then
      toml_inquire_key_kind = "t"
      return
    endif

  end function

  function toml_inquire_val_type(inTblPtr, keyName)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and an key name for a value kind; returns 's' for string, 
    !              'i' for int, 'b' for bool, 'd' for double, or c_null_char 
    !              for key-not-found. Timestamp data yields a fatal error 
    !              (for now), as it has not been implemented

    character                    :: toml_inquire_val_type
    type(c_ptr),      intent(in) :: inTblPtr
    character(len=*), intent(in) :: keyName

    type(c_ptr)                  :: tmpRaw, c_outChar
    integer(c_int)               :: c_ierr
    logical(kind=c_bool)         :: c_outBool
    real(c_double)               :: c_outDbl
    integer(c_int64_t)           :: c_outInt

    toml_inquire_val_type = c_null_char
    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      toml_inquire_val_type = c_null_char
      return
    endif

    c_ierr = tomlc99_toml_rtos(tmpRaw, c_outChar)
    if (c_ierr == 0) then
      toml_inquire_val_type = "s"
      return
    endif

    c_ierr = tomlc99_toml_rtoi(tmpRaw, c_outInt)
    if (c_ierr == 0) then
      toml_inquire_val_type = "i"
      return
    endif

    c_ierr = tomlc99_toml_rtob(tmpRaw, c_outBool)
    if (c_ierr == 0) then
      toml_inquire_val_type = "b"
      return
    endif

    c_ierr = tomlc99_toml_rtod(tmpRaw, c_outDbl)
    if (c_ierr == 0) then
      toml_inquire_val_type = "d"
      return
    endif

    write(stderr, '(3a)') 'ERROR: unknown type for key "', &
                          keyName, '"'
    error stop

  end function

end module
