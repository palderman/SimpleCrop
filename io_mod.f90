module io_mod

  use iso_c_binding, only: c_char,c_ptr,c_float,c_null_char,c_loc

  implicit none

  type(c_ptr) :: input_obj

  interface
     subroutine init_input(inp_ptr) bind(C,name="init_input_c")
       import c_ptr
       type(c_ptr) :: inp_ptr
     end subroutine init_input
  end interface

  interface
     subroutine get_c(inp_ptr,key,value) bind(C,name="get_c")
       import c_char,c_ptr,c_float
       type(c_ptr) :: inp_ptr
       character(len=1,kind=C_char),dimension(*) :: key
       real(c_float) :: value
     end subroutine get_c
  end interface

  interface
     subroutine read_input_c(inp_ptr,filename) bind(C,name="read_input_c")
       import c_char,c_ptr
       type(c_ptr) :: inp_ptr
       character(len=1,kind=C_char),dimension(*) :: filename
     end subroutine read_input_c
  end interface

  interface
     subroutine delete_input_obj_c(inp_ptr) bind(C,name="delete_input_obj_c")
       import c_ptr
       type(c_ptr) :: inp_ptr
     end subroutine delete_input_obj_c
  end interface

  contains

    subroutine init_input_obj()

      implicit none

      call init_input(input_obj)

    end subroutine init_input_obj

    subroutine read_input(filename)

      implicit none
      character(len=*) filename

      call read_input_c(input_obj,trim(filename)//c_null_char)

    end subroutine read_input

    subroutine delete_input_obj()

      implicit none

      call delete_input_obj_c(input_obj)

    end subroutine delete_input_obj

    subroutine get(key,value)

      implicit none

      character(len=*) :: key
      real value

      call get_c(input_obj,trim(key)//C_NULL_CHAR,value)

    end subroutine get

end module io_mod
