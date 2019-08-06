module tomlc99
  use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, &
                                            stderr=>error_unit, &
                                            int32, int64, real64
  use iso_c_binding

  implicit none 

  integer, parameter :: ucs4 = selected_char_kind('ISO_10646')

  integer(int32), parameter :: maxStrLen = 262144

  type, bind(c) :: bufferType
    integer(c_int)         :: year, month, day
    integer(c_int)         :: hour, minute, second
    character(kind=c_char) :: z(10)
  end type

  type, bind(c) :: timestampType
    type(bufferType) :: buffer 
    type(c_ptr)      :: year, month, day
    type(c_ptr)      :: hour, minute, second
    type(c_ptr)      :: z
  end type

  type :: toml_time
    integer           :: year, month, day
    integer           :: hour, minute, second
    character(len=10) :: offset
    character(len=1)  :: timeType
  end type

  interface

    ! interfaces to standard C library functions

    function c_fopen(fileName, mode) bind(C,name="fopen")
      import                 :: c_ptr, c_char
      implicit none
      type(c_ptr)            :: c_fopen
      character(kind=c_char) :: fileName, mode
    end function

    function c_fclose(filePtr) bind(C,name="fclose")
      import                 :: c_ptr, c_int
      implicit none
      type(c_ptr), value     :: filePtr
      integer(c_int)         :: c_fclose
    end function

    subroutine c_free(ptr) bind(C,name="free")
      import                 :: c_ptr
      implicit none
      type(c_ptr), value     :: ptr
    end subroutine

    ! interfaces to tomlc99 functions

    function tomlc99_toml_parse_file(filePtr, errBuf, errBufSz) &
             bind(C,name="toml_parse_file")
      import                 :: c_ptr, c_char, c_int
      implicit none
      type(c_ptr)            :: tomlc99_toml_parse_file
      type(c_ptr), value     :: filePtr
      character(kind=c_char) :: errBuf(*)
      integer(c_int)         :: errBufSz 
    end function 

    subroutine tomlc99_toml_free(tblPtr) bind(C,name="toml_free")
      import                 :: c_ptr
      implicit none
      type(c_ptr), value     :: tblPtr
    end subroutine

    function tomlc99_toml_key_in(tblPtr, keyIdx) bind(C,name="toml_key_in")
      import                 :: c_ptr, c_int
      implicit none
      type(c_ptr)            :: tomlc99_toml_key_in
      type(c_ptr), value     :: tblPtr
      integer(c_int), value  :: keyIdx
    end function 

    function tomlc99_toml_raw_in(dataPtr, keyName) bind(C,name="toml_raw_in")
      import                 :: c_ptr, c_char
      implicit none
      type(c_ptr)            :: tomlc99_toml_raw_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: keyName(*)
    end function 

    function tomlc99_toml_table_in(dataPtr, tableName) &
             bind(C,name="toml_table_in")
      import                 :: c_ptr, c_char
      implicit none
      type(c_ptr)            :: tomlc99_toml_table_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: tableName(*)
    end function 

    function tomlc99_toml_table_nkval(tblPtr) &
             bind(C,name="toml_table_nkval")
      import                 :: c_ptr, c_int
      implicit none
      type(c_ptr), value     :: tblPtr
      integer(c_int)         :: tomlc99_toml_table_nkval
    end function 

    function tomlc99_toml_table_narr(tblPtr) &
             bind(C,name="toml_table_narr")
      import                 :: c_ptr, c_int
      implicit none
      type(c_ptr), value     :: tblPtr
      integer(c_int)         :: tomlc99_toml_table_narr
    end function 

    function tomlc99_toml_table_ntab(tblPtr) &
             bind(C,name="toml_table_ntab")
      import                 :: c_ptr, c_int
      implicit none
      type(c_ptr), value     :: tblPtr
      integer(c_int)         :: tomlc99_toml_table_ntab
    end function 

    function tomlc99_toml_array_in(dataPtr, arrayName) &
             bind(C,name="toml_array_in")
      import                 :: c_ptr, c_char
      implicit none
      type(c_ptr)            :: tomlc99_toml_array_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: arrayName(*)
    end function 

    function tomlc99_toml_array_kind(arrPtr) &
             bind(C,name="toml_array_kind")
      import                 :: c_ptr, c_char
      implicit none
      type(c_ptr), value     :: arrPtr
      character(kind=c_char) :: tomlc99_toml_array_kind
    end function 

    function tomlc99_toml_array_type(arrPtr) &
             bind(C,name="toml_array_type")
      import                 :: c_ptr, c_char
      implicit none
      type(c_ptr), value     :: arrPtr
      character(kind=c_char) :: tomlc99_toml_array_type
    end function 

    function tomlc99_toml_array_nelem(arrPtr) &
             bind(C,name="toml_array_nelem")
      import                 :: c_ptr, c_int 
      implicit none
      integer(c_int)         :: tomlc99_toml_array_nelem
      type(c_ptr), value     :: arrPtr
    end function 

    function tomlc99_toml_raw_at(arrPtr, idx) &
             bind(C,name="toml_raw_at")
      import                 :: c_ptr, c_int 
      implicit none
      type(c_ptr)            :: tomlc99_toml_raw_at
      type(c_ptr), value     :: arrPtr
      integer(c_int), value  :: idx
    end function 

    function tomlc99_toml_array_at(arrPtr, idx) &
             bind(C,name="toml_array_at")
      import                 :: c_ptr, c_int 
      implicit none
      type(c_ptr)            :: tomlc99_toml_array_at
      type(c_ptr), value     :: arrPtr
      integer(c_int), value  :: idx
    end function 

    function tomlc99_toml_table_at(arrPtr, idx) &
             bind(C,name="toml_table_at")
      import                 :: c_ptr, c_int 
      implicit none
      type(c_ptr)            :: tomlc99_toml_table_at
      type(c_ptr), value     :: arrPtr
      integer(c_int), value  :: idx
    end function 

    function tomlc99_toml_rtos(raw, outStr) bind(C,name="toml_rtos")
      import                 :: c_int, c_ptr, c_char
      implicit none
      integer(c_int)         :: tomlc99_toml_rtos
      type(c_ptr), value     :: raw
      type(c_ptr)            :: outStr
    end function 

    function tomlc99_toml_rtoi(raw, outInt) bind(C,name="toml_rtoi")
      import                 :: c_ptr, c_int, c_int64_t
      implicit none
      integer(c_int)         :: tomlc99_toml_rtoi
      type(c_ptr), value     :: raw
      integer(c_int64_t)     :: outInt
    end function 

    function tomlc99_toml_rtod(raw, outDbl) bind(C,name="toml_rtod")
      import                 :: c_ptr, c_int, c_double
      implicit none
      integer(c_int)         :: tomlc99_toml_rtod
      type(c_ptr), value     :: raw
      real(c_double)         :: outDbl
    end function 

    function tomlc99_toml_rtob(raw, outBool) bind(C,name="toml_rtob")
      import                 :: c_ptr, c_int, c_bool
      implicit none
      integer(c_int)         :: tomlc99_toml_rtob
      type(c_ptr), value     :: raw
      logical(c_bool)        :: outBool
    end function 

    function tomlc99_toml_rtots(raw, outTime) bind(C,name="toml_rtots")
      import                 :: c_ptr, c_int, timestampType
      implicit none
      integer(c_int)         :: tomlc99_toml_rtots
      type(c_ptr), value     :: raw
      type(timestampType)    :: outTime
    end function 

    function tomlc99_toml_utf8_to_ucs(orig, length, outVal) &
             bind(C,name="toml_utf8_to_ucs")
      import                 :: c_char, c_int, c_int64_t
      implicit none
      integer(c_int)         :: tomlc99_toml_utf8_to_ucs
      character(kind=c_char) :: orig(*)
      integer(c_int), value  :: length
      integer(c_int64_t)     :: outVal
    end function 

    function tomlc99_toml_ucs_to_utf8(codeVal, buffer) &
             bind(C,name="toml_ucs_to_utf8")
      import                    :: c_char, c_int, c_int64_t
      implicit none
      integer(c_int)            :: tomlc99_toml_ucs_to_utf8
      integer(c_int64_t), value :: codeVal
      character(kind=c_char)    :: buffer(6)
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

  subroutine toml_free(inTblPtr)

    ! description: free the memory associated with the root-level "toml_table_t"
    !              data structure 

    type(c_ptr), intent(in) :: inTblPtr
    call tomlc99_toml_free(inTblPtr)

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

  function toml_table_nkval(inTblPtr)

    ! description: returns the integer number of key-value pairs in the 
    !              "toml_table_t" structure referenced by "inTblPtr". 

    integer(int32)                :: toml_table_nkval
    type(c_ptr), intent(in)       :: inTblPtr

    integer(c_int)                :: c_outVal         

    c_outVal = tomlc99_toml_table_nkval(inTblPtr)
    toml_table_nkval = c_outVal

  end function

  function toml_table_narr(inTblPtr)

    ! description: returns the integer number of arrays in the 
    !              "toml_table_t" structure referenced by "inTblPtr". 

    integer(int32)                :: toml_table_narr 
    type(c_ptr), intent(in)       :: inTblPtr

    integer(c_int)                :: c_outVal         

    c_outVal = tomlc99_toml_table_narr(inTblPtr)
    toml_table_narr = c_outVal

  end function

  function toml_table_ntab(inTblPtr)

    ! description: returns the integer number of sub-tables in the
    !              "toml_table_t" structure referenced by "inTblPtr". 

    integer(int32)                :: toml_table_ntab 
    type(c_ptr), intent(in)       :: inTblPtr

    integer(c_int)                :: c_outVal         

    c_outVal = tomlc99_toml_table_ntab(inTblPtr)
    toml_table_ntab = c_outVal

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
    !              structure and returns the number of elements in the array

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

  subroutine toml_get_array_time(inArrPtr, outArray)

    ! description: accepts a pointer to a "toml_array_t" data structure
    !              structure and returns an type(toml_time) array. A fatal error
    !              is issued if the parameters do not match

    type(c_ptr),                   intent(in)  :: inArrPtr
    type(toml_time), dimension(:), intent(out) :: outArray
    
    integer(c_int)                            :: c_nelem, c_idx, c_ierr
    character(kind=c_char)                    :: c_kind, c_type
    type(timestampType)                       :: c_outTime
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

    if (c_type /= 't' .and. c_type /= "D" .and. c_type /= "T") then
      write(stderr,103) c_type, "t || T || D"
      error stop
    endif

    do idx=1,c_nelem
      c_idx  = idx - 1
      tmpRaw = tomlc99_toml_raw_at(inArrPtr, c_idx)
      c_ierr = tomlc99_toml_rtots(tmpRaw, c_outTime)
      call toml_c_f_timestamp(c_outTime, outArray(idx))
    enddo

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
    !              structure and returns a string array. A fatal error is 
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
    !              structure and returns an array of pointers to the enclosed
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
    !              structure and returns an array of pointers to the enclosed
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
    integer :: ucsLen

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
    toml_get_val_strlen = index(fstring, c_null_char)-1

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

  subroutine toml_get_val_ts(inTblPtr, keyName, outTime)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and a key name for a double; returns the time stamp.
    !              If the parameters do not match, a fatal error is issued.

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    type(toml_time),  intent(out) :: outTime

    type(c_ptr)                   :: tmpRaw
    character                     :: valType
    integer(c_int)                :: c_ierr = 0
    type(timestampType)           :: c_outTime

    outTime%year   = 0
    outTime%month  = 0
    outTime%day    = 0
    outTime%hour   = 0
    outTime%minute = 0
    outTime%second = 0

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      error stop
    endif

    valType = toml_inquire_val_type(inTblPtr, trim(keyName) // c_null_char)
    if (valType /= "t" .and. valType /= "T" .and. valType /= "D") then
      write(stderr,102) trim(keyName), valType, "t || T || D"
      error stop
    endif

    c_ierr = tomlc99_toml_rtots(tmpRaw, c_outTime)

    if (c_ierr == -1) then
      write(stderr,103) trim(keyName)
      error stop
    endif

    call toml_c_f_timestamp(c_outTime, outTime)

    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Key "',a,'" has type "',a,&
                        '", but the output array is type "',a,'"')
    103 format ('ERROR: Failed double conversion for key: ',a)

  end subroutine

  subroutine toml_c_f_timestamp(c_time, f_time)

    ! description: accepts a "toml_timestamp_t" data structure from the 
    !              output of an "rtots" call and converts it to a Fortran
    !              structure.

    type(timestampType), intent(in)  :: c_time
    type(toml_time),     intent(out) :: f_time

    integer                          :: idx
    integer, pointer                 :: tmpInt
    character(len=10), pointer       :: tmpChar

    if (c_associated(c_time%year)) then
      call c_f_pointer(c_time%year,  tmpInt)
      f_time % year  = tmpInt
      call c_f_pointer(c_time%month, tmpInt)
      f_time % month = tmpInt
      call c_f_pointer(c_time%day,   tmpInt)
      f_time % day   = tmpInt
    endif

    if (c_associated(c_time%hour)) then
      call c_f_pointer(c_time%hour,  tmpInt)
      f_time % hour  = tmpInt
      call c_f_pointer(c_time%minute, tmpInt)
      f_time % minute= tmpInt
      call c_f_pointer(c_time%second, tmpInt)
      f_time % second= tmpInt
    endif

    if (c_associated(c_time%z)) then
      call c_f_pointer(c_time%z,  tmpchar)
      idx = index(tmpChar,c_null_char) - 1
      f_time % offset = tmpChar(1:idx)
    endif

    ! set val type flag
    if (c_associated(c_time%year) .and. &
        c_associated(c_time%hour)) then
      f_time % timeType = "T"
    elseif (c_associated(c_time%year)) then
      f_time % timeType = "D"
    elseif (c_associated(c_time%hour)) then
      f_time % timeType = "t" 
    endif

  end subroutine

  subroutine toml_timestamp_to_string(tsVal, outString)

    ! description: writes a toml_time structure to a string

    type(toml_time),   intent(in)  :: tsVal
    character(len=29), intent(out) :: outString
    
    if (tsVal % timeType == "T") then
      write(outString,101) tsVal%year, tsVal%month,  tsVal%day, &
                           tsVal%hour, tsVal%minute, tsVal%second, &
                           adjustl(tsVal%offset)
    else if (tsVal % timeType == "D") then
      write(outString,102) tsVal%year, tsVal%month,  tsVal%day
    else if (tsVal % timeType == "t") then
      write(outString,103) tsVal%hour, tsVal%minute, tsVal%second
    else
      write(stderr,104) tsVal % timeType
      error stop
    endif

    101 format (i4.4,'-',i2.2,'-',i2.2,'T',i2.2,':',i2.2,':',i2.2,a10)
    102 format (i4.4,'-',i2.2,'-',i2.2)
    103 format (i2.2,':',i2.2,':',i2.2)
    104 format ('ERROR: Timestamp type "',a1,'" is unrecognized.')
    
  end subroutine

  function toml_get_keyLen_at_index(inTblPtr, keyIndex)

    ! description: accepts a pointer to a "toml_table_t" data structure
    !              and an index number (starting with 0, C convention);
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
    !              and an index number (starting wth 0, C convention);
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
    !              'i' for int, 'b' for bool, 'd' for double, 'T' for date-time,
    !              'D' for date, 't' for time, or c_null_char for key-not-found. 

    character                    :: toml_inquire_val_type
    type(c_ptr),      intent(in) :: inTblPtr
    character(len=*), intent(in) :: keyName

    type(c_ptr)                  :: tmpRaw, c_outChar
    integer(c_int)               :: c_ierr
    logical(kind=c_bool)         :: c_outBool
    real(c_double)               :: c_outDbl
    integer(c_int64_t)           :: c_outInt
    type(timestampType)          :: c_outTime
    integer, pointer             :: tsYearVal, tsHourVal

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

    c_ierr = tomlc99_toml_rtots(tmpRaw, c_outTime)
    if (c_ierr == 0) then
      if (c_associated(c_outTime%year) .and. &
          c_associated(c_outTime%hour)) then
        toml_inquire_val_type = "T"
      elseif (c_associated(c_outTime%year)) then
        toml_inquire_val_type = "D"
      elseif (c_associated(c_outTime%hour)) then
        toml_inquire_val_type = "t"
      endif
      return
    endif

    write(stderr, '(3a)') 'ERROR: unknown type for key "', &
                          keyName, '"'
    error stop

  end function

  function toml_utf8_decode_strlen(inStr)

    ! description: accepts a string that is encoded with UTF-8 and returns the 
    !              number of unicode (UCS-4) values contained in the string. 

    integer                      :: toml_utf8_decode_strlen
    character(len=*), intent(in) :: inStr
    
    integer                           :: strEnd, fIdx
    integer(c_int)                    :: bytes
    integer(c_int64_t)                :: ucsInt

    toml_utf8_decode_strlen = 0

    fIdx   = 1
    do while (fIdx <= len(inStr))

      strEnd = min(fIdx+5,len(inStr))
      bytes  = tomlc99_toml_utf8_to_ucs(inStr(fIdx:strEnd) // c_null_char,&
                                        strEnd-fIdx+1,ucsInt)

      if (bytes /= -1) then
        fIdx = fIdx + bytes
        toml_utf8_decode_strlen = toml_utf8_decode_strlen + 1
      else 
        write(stderr,'(a)') &
           "ERROR: UTF-8 decoding failed (bytes == -1; toml_utf8_decode_strlen)"
        error stop
      endif

    enddo

    if (fIdx /= len(inStr)+1) then
      write(stderr,'(a)') &
        "ERROR: UTF-8 decoding failed (fIdx /= len(inStr)+1); " & 
        // "(toml_utf8_decode_strlen)"
      error stop
    endif

  end function

  subroutine toml_utf8_decode_str(inStr, outStr)

    ! description: accepts a string that is encoded with UTF-8 and returns a
    !              decoded character string of kind ISO_10646 (UCS-4).

    character(len=*), intent(in) :: inStr
    character(len=*, kind=ucs4), &
                    intent(out)  :: outStr

    integer                           :: expLen, fIdx, uIdx, strEnd
    integer(c_int)                    :: bytes
    integer(c_int64_t)                :: ucsInt

    expLen = toml_utf8_decode_strlen(inStr)

    if (expLen /= len(outStr)) then
      write(stderr, '(a)') &
        "ERROR: UCS-4 output string length does not match expectations " &
        // "(toml_utf8_decode_str)"
      error stop
    endif

    fIdx   = 1
    uIdx   = 1
    do while (fIdx <= len(inStr))

      strEnd = min(fIdx+5,len(inStr))
      bytes = tomlc99_toml_utf8_to_ucs(inStr(fIdx:strEnd), &
                                       strEnd-fIdx+1,ucsInt)

      if (bytes /= -1) then
        outStr(uIdx:uIdx) = char(ucsInt, kind=ucs4)
        fIdx = fIdx + bytes
        uIdx = uIdx + 1
      else 
        write(stderr,'(a)') &
           "ERROR: UTF-8 decoding failed (bytes == -1; toml_utf8_decode_str)"
        error stop
      endif
      
    enddo

    if (fIdx /= len(inStr)+1) then
      write(stderr,'(a)') &
        "ERROR: UTF-8 decoding failed (fIdx /= len(inStr)+1); " & 
        // "toml_utf8_decode_str)"
      error stop
    endif

  end subroutine

  function toml_utf8_encode_strlen(inStr)

    ! description: accepts a character string of kind ISO_10646 (UCS-4) and
    !              returns the string length of default kind for the UTF-8-
    !              encoded rendition of the input string

    integer                      :: toml_utf8_encode_strlen
    character(len=*,kind=ucs4), &
                      intent(in) :: inStr
    
    integer                           :: strEnd, uIdx
    integer(int64)                    :: tmpInt
    integer(c_int)                    :: bytes
    integer(c_int64_t)                :: ucsInt
    character(c_char)                 :: buffer(6)

    toml_utf8_encode_strlen = 0

    do uIdx=1,len(inStr)

      ucsInt = ichar(inStr(uIdx:uIdx), int64)
      bytes  = tomlc99_toml_ucs_to_utf8(ucsInt, buffer)

      if (bytes /= -1) then
        toml_utf8_encode_strlen = toml_utf8_encode_strlen + bytes
      else 
        write(stderr,'(a)') &
           "ERROR: UTF-8 encoding failed (bytes == -1; toml_utf8_encode_strlen)"
        error stop
      endif

    enddo

  end function

  subroutine toml_utf8_encode_str(inStr, outStr)

    ! description: accepts a character string of kind ISO_10646 (UCS-4) and
    !              returns a string that is encoded with UTF-8.

    character(len=*, kind=ucs4), &
                    intent(in)    :: inStr
    character(len=*), intent(out) :: outStr

    integer                           :: expLen, fIdx, uIdx, bIdx 
    integer(c_int)                    :: bytes
    integer(c_int64_t)                :: ucsInt
    character(c_char)                 :: buffer(6)

    expLen = toml_utf8_encode_strlen(inStr)

    if (expLen /= len(outStr)) then
      write(stderr, '(a)') &
        "ERROR: UTF-8 output string length does not match expectations " &
        // "(toml_utf8_encode_str)"
      error stop
    endif

    fIdx = 1
    do uIdx=1,len(inStr)

      ucsInt = ichar(inStr(uIdx:uIdx), int64)
      bytes  = tomlc99_toml_ucs_to_utf8(ucsInt, buffer)

      if (bytes /= -1) then
        do bIdx=1,bytes
          outStr((fidx+bIdx-1):(fidx+bIdx-1)) = buffer(bIdx)
        enddo
        fidx = fidx + bytes
      else 
        write(stderr,'(a)') &
           "ERROR: UTF-8 encoding failed (bytes == -1; toml_utf8_encode_str)"
        error stop
      endif

    enddo

    if (fIdx /= len(outStr)+1) then
      write(stderr,'(a)') &
        "ERROR: UTF-8 encoding failed (fIdx /= len(outStr)+1); " & 
        // "toml_utf8_encode_str)"
      write(stderr,*) "FIDX",fIdx,len(inStr)+1
      error stop
    endif

  end subroutine

  function toml_inquire_string_is_ascii(inStr)

    ! description: accepts a string that is encoded with UTF-8 and returns a
    !              logical indicating whether the string is plain ascii

    logical                      :: toml_inquire_string_is_ascii
    character(len=*), intent(in) :: inStr

    toml_inquire_string_is_ascii = .true.
    if (len(inStr) /= toml_utf8_decode_strlen(inStr)) &
      toml_inquire_string_is_ascii = .false.

  end function

end module
