module tomlc99
  use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, &
                                            stderr=>error_unit, &
                                            int32, int64, real64
  use iso_c_binding

  implicit none 

  integer(int32), parameter :: maxStrLen = 262144
  logical,        parameter :: errorsFatal = .true.

  interface

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

  function open_file(fileName)
    type(c_ptr)                     :: open_file
    character(len=*), intent(in)    :: fileName
    type(c_ptr)                     :: fh
    character(len=512, kind=c_char) :: errBuf

    fh = c_fopen(trim(fileName) // c_null_char, &
                 c_char_"r" // c_null_char)

    if (c_associated(fh) .eqv. .false.) then
      write(stderr,101) trim(fileName)
      error stop
    endif

    open_file = tomlc99_toml_parse_file(fh, errBuf, len(errBuf, kind=c_int))

    if (c_associated(open_file) .eqv. .false.) then
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
    character(len=*, kind=c_char), intent(in) :: errBuf
    integer :: idx
    do idx=1,len(errBuf)
      if (errBuf(idx:idx) == c_null_char) exit
    enddo
    write(stderr, '(a)') errBuf(1:idx)
  end subroutine

  function table_in(inTblPtr, tblName)
    type(c_ptr)                   :: table_in
    type(c_ptr), intent(in)       :: inTblPtr
    character(len=*), intent(in)  :: tblName

    table_in = tomlc99_toml_table_in(inTblPtr, &
                                     tblName // c_null_char)

    if (c_associated(table_in) .eqv. .false.) then
      write(stderr,101) trim(tblName)
      if (errorsFatal .eqv. .true.) error stop
    endif
 
    101 format ('ERROR: Failed to find table: ',a)

  end function

  function array_in(inTblPtr, arrayName)

    type(c_ptr)                   :: array_in
    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: arrayName

    array_in = tomlc99_toml_array_in(inTblPtr, &
                                     arrayName // c_null_char)

    if (c_associated(array_in) .eqv. .false.) then
      write(stderr,101) trim(arrayName)
      if (errorsFatal .eqv. .true.) error stop
    endif
 
    101 format ('ERROR: Failed to find array: ',a)

  end function

  function array_kind(inArrPtr)

    character                     :: array_kind
    type(c_ptr), intent(in)       :: inArrPtr

    character(kind=c_char)        :: c_kind

    c_kind = tomlc99_toml_array_kind(inArrPtr)

    if (c_kind == c_null_char) then
      write(stderr,101) 
      if (errorsFatal .eqv. .true.) error stop
    endif

    array_kind = c_kind
 
    101 format ('ERROR: Call to array_kind failed.')

  end function

  function array_type(inArrPtr)

    character                     :: array_type
    type(c_ptr), intent(in)       :: inArrPtr
    character(kind=c_char)        :: c_kind

    c_kind = tomlc99_toml_array_type(inArrPtr)

    if (c_kind == c_null_char) then
      write(stderr,101) 
      if (errorsFatal .eqv. .true.) error stop
    endif

    array_type = c_kind
 
    101 format ('ERROR: array type is unknown.')

  end function

  function array_nelem(inArrPtr)

    integer                       :: array_nelem
    type(c_ptr), intent(in)       :: inArrPtr

    integer(c_int)                :: c_nelem

    c_nelem     = tomlc99_toml_array_nelem(inArrPtr)
    array_nelem = c_nelem

  end function

  subroutine get_array_int(inArrPtr, outArray)

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
      if (errorsFatal .eqv. .true.) error stop
    endif

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      if (errorsFatal .eqv. .true.) error stop
    endif

    if (c_type /= 'i') then
      write(stderr,103) c_type, 'i'
      if (errorsFatal .eqv. .true.) error stop
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

  subroutine get_array_dbl(inArrPtr, outArray)

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
      if (errorsFatal .eqv. .true.) error stop
    endif

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      if (errorsFatal .eqv. .true.) error stop
    endif

    if (c_type /= 'd') then
      write(stderr,103) c_type, 'd'
      if (errorsFatal .eqv. .true.) error stop
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

  subroutine get_array_bool(inArrPtr, outArray)

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
      if (errorsFatal .eqv. .true.) error stop
    endif

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      if (errorsFatal .eqv. .true.) error stop
    endif

    if (c_type /= 'b') then
      write(stderr,103) c_type, 'b'
      if (errorsFatal .eqv. .true.) error stop
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

  function array_strlen(inArrPtr)

    integer                     :: array_strlen
    type(c_ptr), intent(in)     :: inArrPtr
    
    integer(c_int)              :: c_nelem, c_idx, c_ierr
    character(kind=c_char)      :: c_kind, c_type
    type(c_ptr)                 :: tmpRaw, c_outStr
    integer                     :: idx, tmpStrLen
    character(len=maxStrLen), &
                        pointer :: fstring
    
    array_strlen = 0

    c_nelem     = tomlc99_toml_array_nelem(inArrPtr)
    c_kind      = tomlc99_toml_array_kind(inArrPtr)
    c_type      = tomlc99_toml_array_type(inArrPtr)

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      if (errorsFatal .eqv. .true.) error stop
    endif

    if (c_type /= 's') then
      write(stderr,103) c_type, 's'
      if (errorsFatal .eqv. .true.) error stop
    endif

    do idx=1,c_nelem

      c_idx  = idx - 1
      tmpRaw = tomlc99_toml_raw_at(inArrPtr, c_idx)
      c_ierr = tomlc99_toml_rtos(tmpRaw, c_outStr)

      call c_f_pointer(c_outStr, fstring)
      tmpStrLen = index(fstring, c_null_char)-1
      call c_free(c_outStr)

      array_strlen = max(array_strlen, tmpStrLen)

    enddo

    102 format ('ERROR: array has kind "',a,'" but "',a,'" is required.')
    103 format ('ERROR: array has type "',a,'" but "',a,'" is required.')

  end function

  subroutine get_array_str(inArrPtr, outArray)

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
      if (errorsFatal .eqv. .true.) error stop
    endif

    if (c_kind /= 'v') then
      write(stderr,102) c_kind, 'v'
      if (errorsFatal .eqv. .true.) error stop
    endif

    if (c_type /= 's') then
      write(stderr,103) c_type, 's'
      if (errorsFatal .eqv. .true.) error stop
    endif

    maxLen = array_strlen(inArrPtr)
    if (len(outArray) /= maxLen) then
      write(stderr,104) maxLen, len(outArray)
      if (errorsFatal .eqv. .true.) error stop
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

  function get_key_strlen(inTblPtr, keyName)

    integer(int32)                :: get_key_strlen
    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 

    type(c_ptr)                   :: tmpRaw
    integer(c_int)                :: c_ierr = 0
    type(c_ptr)                   :: c_outStr

    character(len=maxStrLen), pointer :: fstring

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    c_ierr = tomlc99_toml_rtos(tmpRaw, c_outStr)

    call c_f_pointer(c_outStr, fstring)
    get_key_strlen =  index(fstring, c_null_char)-1
    call c_free(c_outStr)

    101 format ('ERROR: Failed to find key: ',a)

  end function

  subroutine get_key_str(inTblPtr, keyName, outVal)

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    character(len=*), intent(out) :: outVal

    type(c_ptr)                    :: tmpRaw
    integer(c_int)                 :: c_ierr = 0
    type(c_ptr)                    :: c_outStr

    character(len=maxStrLen), pointer  :: fstring

    integer :: strLen = 0

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    c_ierr = tomlc99_toml_rtos(tmpRaw, c_outStr)

    call c_f_pointer(c_outStr, fstring)
    strLen =  index(fstring, c_null_char)-1

    if (strLen /= len(outVal)) then
      write(stderr,102) trim(keyName)
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    outVal = fstring(1:strLen)
    call c_free(c_outStr)

    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Output string length does not match TOML data for key: ',a)

  end subroutine

  subroutine get_key_int(inTblPtr, keyName, outVal)

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    integer(int64),   intent(out) :: outVal

    type(c_ptr)                   :: tmpRaw
    integer(c_int)                :: c_ierr = 0
    integer(c_int64_t)            :: c_outVal = 0

    outVal = 0

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    c_ierr = tomlc99_toml_rtoi(tmpRaw, c_outVal)

    if (c_ierr == -1) then
      write(stderr,102) trim(keyName)
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Failed integer conversion for key: ',a)

  end subroutine

  subroutine get_key_dbl(inTblPtr, keyName, outVal)

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    real(real64),     intent(out) :: outVal

    type(c_ptr)                   :: tmpRaw
    integer(c_int)                :: c_ierr = 0
    real(c_double)                :: c_outVal = 0

    outVal = 0

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    c_ierr = tomlc99_toml_rtod(tmpRaw, c_outVal)

    if (c_ierr == -1) then
      write(stderr,102) trim(keyName)
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Failed double conversion for key: ',a)

  end subroutine

  subroutine get_key_bool(inTblPtr, keyName, outVal)

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    logical,          intent(out) :: outVal

    type(c_ptr)                   :: tmpRaw
    integer(c_int)                :: c_ierr = 0
    logical(kind=c_bool)          :: c_outVal

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    c_ierr = tomlc99_toml_rtob(tmpRaw, c_outVal)

    if (c_ierr == -1) then
      write(stderr,102) trim(keyName)
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Failed bool conversion for key: ',a)

  end subroutine

  function get_keyLen_at_index(inTblPtr, keyIndex)

    integer(int32)              :: get_keyLen_at_index
    type(c_ptr),    intent(in)  :: inTblPtr
    integer(int32), intent(in)  :: keyIndex

    integer(c_int)              :: c_idx
    type(c_ptr)                 :: tmpKey
    character(len=maxStrLen), &
                        pointer :: fstring

    c_idx  = keyIndex
    tmpKey = tomlc99_toml_key_in(inTblPtr, c_idx)
    call c_f_pointer(tmpKey, fstring)
    get_keyLen_at_index = index(fstring, c_null_char)-1

  end function

  subroutine get_keyName_at_index(inTblPtr, keyIndex, keyName)

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
      if (errorsFatal .eqv. .true.) error stop
      return
    endif

    keyName = fstring(1:keyLen)

    101 format ('ERROR: Output string length does not match TOML data for key: ',a)

  end subroutine

  function inquire_key_type(inTblPtr, keyName)
    character                    :: inquire_key_type
    type(c_ptr),      intent(in) :: inTblPtr
    character(len=*), intent(in) :: keyName

    type(c_ptr)                  :: tmpRaw

    inquire_key_type = c_null_char

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)
    if (c_associated(tmpRaw) .eqv. .true.) then
      inquire_key_type = "v"
      return
    endif

    tmpRaw = tomlc99_toml_array_in(inTblPtr, trim(keyName) // c_null_char)
    if (c_associated(tmpRaw) .eqv. .true.) then
      inquire_key_type = "a"
      return
    endif

    tmpRaw = tomlc99_toml_table_in(inTblPtr, trim(keyName) // c_null_char)
    if (c_associated(tmpRaw) .eqv. .true.) then
      inquire_key_type = "t"
      return
    endif

  end function

end module
