module tomlc99
  use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, &
                                            stderr=>error_unit, &
                                            int32, int64, real64
  use iso_c_binding

  implicit none 

  integer(int32), parameter :: maxStrLen = 262144

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

  function table_in(inTblPtr, tblName, ierr)
    type(c_ptr)                   :: table_in
    type(c_ptr), intent(in)       :: inTblPtr
    character(len=*), intent(in)  :: tblName
    integer,          intent(out) :: ierr

    ierr = 0
    table_in = tomlc99_toml_table_in(inTblPtr, &
                                     tblName // c_null_char)

    if (c_associated(table_in) .eqv. .false.) then
      write(stderr,101) trim(tblName)
      ierr = 1
    endif
 
    101 format ('ERROR: Failed to find table: ',a)

  end function

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
      get_key_strlen = -1
      return
    endif

    c_ierr = tomlc99_toml_rtos(tmpRaw, c_outStr)

    call c_f_pointer(c_outStr, fstring)
    get_key_strlen =  index(fstring, c_null_char)-1

    101 format ('ERROR: Failed to find key: ',a)

  end function

  subroutine get_key_str(inTblPtr, keyName, outVal, ierr)

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    character(len=*), intent(out) :: outVal
    integer(int32),   intent(out) :: ierr

    type(c_ptr)                    :: tmpRaw
    integer(c_int)                 :: c_ierr = 0
    type(c_ptr)                    :: c_outStr

    character(len=maxStrLen), pointer  :: fstring

    integer :: strLen = 0

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      ierr = -1
      return
    endif

    c_ierr = tomlc99_toml_rtos(tmpRaw, c_outStr)

    call c_f_pointer(c_outStr, fstring)
    strLen =  index(fstring, c_null_char)-1

    if (strLen /= len(outVal)) then
      write(stderr,102) trim(keyName)
      ierr = -1
      return
    endif

    outVal = fstring(1:strLen)

    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Output string length does not match TOML data for key: ',a)

  end subroutine

  subroutine get_key_int(inTblPtr, keyName, outVal, ierr)

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    integer(int64),   intent(out) :: outVal
    integer(int32),   intent(out) :: ierr

    type(c_ptr)                   :: tmpRaw
    integer(c_int)                :: c_ierr = 0
    integer(c_int64_t)            :: c_outVal = 0

    outVal = 0
    ierr   = 0

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      ierr = -1
      return
    endif

    c_ierr = tomlc99_toml_rtoi(tmpRaw, c_outVal)
    ierr   = c_ierr

    if (c_ierr == -1) then
      write(stderr,102) trim(keyName)
      return
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Failed integer conversion for key: ',a)

  end subroutine

  subroutine get_key_dbl(inTblPtr, keyName, outVal, ierr)

    type(c_ptr),      intent(in)  :: inTblPtr
    character(len=*), intent(in)  :: keyName 
    real(real64),     intent(out) :: outVal
    integer(int32),   intent(out) :: ierr

    type(c_ptr)                   :: tmpRaw
    integer(c_int)                :: c_ierr = 0
    real(c_double)                :: c_outVal = 0

    outVal = 0
    ierr   = 0

    tmpRaw = tomlc99_toml_raw_in(inTblPtr, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      ierr = -1
      return
    endif

    c_ierr = tomlc99_toml_rtod(tmpRaw, c_outVal)
    ierr   = c_ierr

    if (c_ierr == -1) then
      write(stderr,102) trim(keyName)
      return
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Failed double conversion for key: ',a)

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

  subroutine get_keyName_at_index(inTblPtr, keyIndex, keyName, ierr)

    type(c_ptr),      intent(in)  :: inTblPtr
    integer(int32),   intent(in)  :: keyIndex
    character(len=*), intent(out) :: keyName
    integer(int32),   intent(out) :: ierr

    integer(c_int)                :: c_idx
    type(c_ptr)                   :: tmpKey
    character(len=maxStrLen), &
                        pointer   :: fstring

    integer(int32)                :: keyLen

    ierr   = 0
    c_idx  = keyIndex
    tmpKey = tomlc99_toml_key_in(inTblPtr, c_idx)
    call c_f_pointer(tmpKey, fstring)
    keyLen = index(fstring, c_null_char)-1

    if (keyLen /= len(keyName)) then
      write(stderr,101) trim(keyName)
      ierr = -1
      return
    endif

    keyName = fstring(1:keyLen)

    101 format ('ERROR: Output string length does not match TOML data for key: ',a)

  end subroutine

end module
