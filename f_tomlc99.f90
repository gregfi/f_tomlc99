module f_tomlc99
  use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, &
                                            stderr=>error_unit, &
                                            int32, int64, real64

  use iso_c_binding, only: c_ptr, c_char, c_null_char, &
                           c_int, c_int64_t, c_double, & 
                           c_associated, c_null_ptr 

  implicit none 

  type(c_ptr), protected :: toml_data = c_null_ptr, &
                            toml_table = c_null_ptr

  interface

    function fopen(fileName, mode) bind(C)
      import                 :: c_ptr, c_char
      type(c_ptr)            :: fopen
      character(kind=c_char) :: fileName, mode
    end function

    function fclose(filePtr) bind(C)
      import                 :: c_ptr, c_int
      type(c_ptr), value     :: filePtr
      integer(c_int)         :: fclose
    end function

    function toml_parse_file(filePtr, errBuf, errBufSz) bind(C)
      import                 :: c_ptr, c_char, c_int
      type(c_ptr)            :: toml_parse_file
      type(c_ptr), value     :: filePtr
      character(kind=c_char) :: errBuf(*)
      integer(c_int)         :: errBufSz 
    end function 

    function toml_table_nkval(tblPtr) bind(C)
      import                 :: c_ptr, c_int
      type(c_ptr), value     :: tblPtr
      integer(c_int)         :: toml_table_nkval
    end function 

    function toml_table_narr(tblPtr) bind(C)
      import                 :: c_ptr, c_int
      type(c_ptr), value     :: tblPtr
      integer(c_int)         :: toml_table_nkval
    end function 

    function toml_table_ntab(tblPtr) bind(C)
      import                 :: c_ptr, c_int
      type(c_ptr), value     :: tblPtr
      integer(c_int)         :: toml_table_nkval
    end function 

    function toml_key_in(dataPtr, keyIdx) bind(C)
      import                 :: c_ptr, c_int
      type(c_ptr)            :: toml_raw_in
      type(c_ptr), value     :: dataPtr
      integer(c_int)         :: keyIdx
    end function 

    function toml_raw_in(dataPtr, keyName) bind(C)
      import                 :: c_ptr, c_char
      type(c_ptr)            :: toml_raw_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: keyName(*)
    end function 

    function toml_table_in(dataPtr, tableName) bind(C)
      import                 :: c_ptr, c_char
      type(c_ptr)            :: toml_table_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: tableName(*)
    end function 

    function toml_array_in(dataPtr, arrayName) bind(C)
      import                 :: c_ptr, c_char
      type(c_ptr)            :: toml_array_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: arrayName(*)
    end function 

    function toml_rtos(raw, outStr) bind(C)
      import                 :: c_int, c_ptr, c_char
      integer(c_int)         :: toml_rtos
      type(c_ptr), value     :: raw
      character(kind=c_char) :: outStr(*)
    end function 

    function toml_rtoi(raw, outInt) bind(C)
      import                 :: c_ptr, c_int, c_int64_t
      integer(c_int)         :: toml_rtoi
      type(c_ptr), value     :: raw
      integer(c_int64_t)     :: outInt
    end function 

    function toml_rtod(raw, outDbl) bind(C)
      import                 :: c_ptr, c_int, c_double
      integer(c_int)         :: toml_rtoi
      type(c_ptr), value     :: raw
      real(c_double)         :: outDbl
    end function 

  end interface

  contains

  subroutine open_toml_file(fileName)
    character(len=*), intent(in) :: fileName
    type(c_ptr) :: fh
    character(len=512, kind=c_char):: errBuf

    fh = fopen(trim(fileName) // c_null_char, &
               c_char_"r" // c_null_char)

    if (c_associated(fh) .eqv. .false.) then
      write(stderr,101) trim(fileName)
      error stop
    endif

    toml_data = toml_parse_file(fh, errBuf, len(errBuf, kind=c_int))

    if (c_associated(toml_data) .eqv. .false.) then
      write(stderr,102) trim(fileName)
      call write_error_buffer(errBuf)
      error stop
    endif
   
    if (fclose(fh) /= 0) then
      write(stderr,103) trim(fileName)
      error stop
    endif

    101 format ('ERROR: Failed to open ',a)
    102 format ('ERROR: Failed to parse ',a)
    103 format ('ERROR: Failed to close ',a)

  end subroutine

  subroutine write_error_buffer(errBuf)
    character(len=*, kind=c_char), intent(in) :: errBuf
    integer :: idx
    do idx=1,len(errBuf)
      if (errBuf(idx:idx) == char(0)) exit
    enddo
    write(stderr, '(a)') errBuf(1:idx)
  end subroutine

  subroutine toml_load_table(tblName, ierr)
    character(len=*), intent(in)  :: tblName
    integer,          intent(out) :: ierr

    ierr = 0
    toml_table = toml_table_in(toml_data, &
                               tblName // c_null_char)

    if (c_associated(toml_data) .eqv. .false.) then
      write(stderr,101) trim(tblName)
      ierr = 1
    endif
 
    101 format ('ERROR: Failed to find table: ',a)

  end subroutine

  subroutine toml_get_key_str(keyName, outStr, ierr)
    character(len=*), intent(in)  :: keyName 
    integer(int32),   intent(out) :: ierr
    character(len=*), intent(out) :: outStr

    type(c_ptr)                    :: tmpRaw
    integer(c_int)                 :: c_ierr = 0
    character(len=128)             :: c_outStr = ""

    outStr = ""
    ierr   = 0

    tmpRaw = toml_raw_in(toml_table, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      ierr = -1
      return
    endif

    c_ierr = toml_rtos(tmpRaw, c_outStr)
    ierr   = c_ierr

    if (c_ierr == -1) then
      write(stderr,102) trim(keyName)
      return
    endif
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Failed string conversion for key: ',a)

  end subroutine

  subroutine toml_get_key_int(keyName, outVal, ierr)
    character(len=*), intent(in)  :: keyName 
    integer(int32),   intent(out) :: ierr
    integer(int64),   intent(out) :: outVal

    type(c_ptr)                   :: tmpRaw
    integer(c_int)                :: c_ierr = 0
    integer(c_int64_t)            :: c_outVal = 0

    outVal = 0
    ierr   = 0

    tmpRaw = toml_raw_in(toml_table, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      ierr = -1
      return
    endif

    c_ierr = toml_rtoi(tmpRaw, c_outVal)
    ierr   = c_ierr

    if (c_ierr == -1) then
      write(stderr,102) trim(keyName)
      return
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Failed integer conversion for key: ',a)

  end subroutine

  subroutine toml_get_key_dbl(keyName, outVal, ierr)
    character(len=*), intent(in)  :: keyName 
    integer(int32),   intent(out) :: ierr
    real(real64),     intent(out) :: outVal

    type(c_ptr)                   :: tmpRaw
    integer(c_int)                :: c_ierr = 0
    real(c_double)                :: c_outVal = 0

    outVal = 0
    ierr   = 0

    tmpRaw = toml_raw_in(toml_table, trim(keyName) // c_null_char)

    if (c_associated(tmpRaw) .eqv. .false.) then
      write(stderr,101) trim(keyName)
      ierr = -1
      return
    endif

    c_ierr = toml_rtod(tmpRaw, c_outVal)
    ierr   = c_ierr

    if (c_ierr == -1) then
      write(stderr,102) trim(keyName)
      return
    endif

    outVal = c_outVal
 
    101 format ('ERROR: Failed to find key: ',a)
    102 format ('ERROR: Failed double conversion for key: ',a)

  end subroutine



end module
