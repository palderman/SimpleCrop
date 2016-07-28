module io_mod

  use iso_c_binding

  implicit none

  type(c_ptr) :: plant_io

  interface
     subroutine get_c(inp_ptr,key,value) bind(C,name="get_value")
       import c_char,c_ptr,c_float
       type(c_ptr) :: inp_ptr
       character(len=1,kind=C_char),dimension(*) :: key
       real(c_float) :: value
     end subroutine
  end interface

  interface
     subroutine read_plant_input_c(inp_ptr,filename) bind(C,name="read_plant_input")
       import c_char,c_ptr
       type(c_ptr) :: inp_ptr
       character(len=1,kind=C_char),dimension(*) :: filename
     end subroutine read_plant_input_c
  end interface

  interface
     subroutine delete_plant_input_c(inp_ptr) bind(C,name="delete_plant_input")
       import c_ptr
       type(c_ptr) :: inp_ptr
     end subroutine delete_plant_input_c
  end interface

  contains

    subroutine read_plant_input(filename)

      implicit none
      character(len=*) filename
      character(len=1),parameter:: NUL=char(0)

      call read_plant_input_c(plant_io,trim(filename)//NUL)

    end subroutine read_plant_input

    subroutine delete_plant_input()

      implicit none

      call delete_plant_input_c(plant_io)

    end subroutine delete_plant_input

    subroutine get(key,value)

      implicit none

      character(len=*) key
      character(len=1),parameter:: NUL=char(0)
      real value

      call get_c(plant_io,trim(key)//NUL,value)

    end subroutine get

end module io_mod
