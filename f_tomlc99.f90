module f_tomlc99
  use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, &
                                            stderr=>error_unit
  use iso_c_binding, only: c_ptr, c_char, c_null_char, c_int, &
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

    function toml_raw_in(dataPtr, keyName) bind(C)
      import                 :: c_ptr, c_char, c_int
      type(c_ptr)            :: toml_raw_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: keyName(*)
    end function 

    function toml_table_in(dataPtr, tableName) bind(C)
      import                 :: c_ptr, c_char, c_int
      type(c_ptr)            :: toml_table_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: tableName(*)
    end function 

    function toml_array_in(dataPtr, arrayName) bind(C)
      import                 :: c_ptr, c_char, c_int
      type(c_ptr)            :: toml_array_in
      type(c_ptr), value     :: dataPtr
      character(kind=c_char) :: arrayName(*)
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

end module
