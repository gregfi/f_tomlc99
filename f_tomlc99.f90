module f_tomlc99
  use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, &
                                            stderr=>error_unit
  use iso_c_binding, only: c_ptr, c_char, c_null_char, c_int, &
                           c_associated

  implicit none

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

  end interface

  contains

  subroutine open_toml_file(fileName)
    character(len=*), intent(in) :: fileName
    type(c_ptr) :: fh, tomlPtr
    character(len=512, kind=c_char):: errBuf

    fh = fopen(trim(fileName) // c_null_char, &
               c_char_"r" // c_null_char)

    if (c_associated(fh) .eqv. .false.) then
      write(stderr,101) trim(fileName)
      error stop
    endif

    tomlPtr = toml_parse_file(fh, errBuf, len(errBuf, kind=c_int))

    if (c_associated(tomlPtr) .eqv. .false.) then
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

end module
