module f_tomlc99
  implicit none
  use iso_c_binding, only: c_int, c_ptr, c_char, c_null_char

  interface

    function fopen(fileName, mode) bind(C)
      import :: c_ptr, c_char
      type(c_ptr) :: fopen
      character(kind=c_char) :: fileName, mode
    end function

    function toml_parse_file(filePtr, errBuf, errBufSz) bind(C)
      import :: c_ptr, c_int
      type(c_ptr) :: toml_parse_file, filePtr, errBuf
      type(c_int) :: errBufSz 
    end function 

  end interface

  contains

  subroutine open_toml_file(fileName)
    character(len=*), intent(in) :: fileName

  end subroutine

end module
