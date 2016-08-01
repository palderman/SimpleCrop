program test

  use io_mod

  implicit none

  real Lfmax

  call init_input_obj()

  call read_input('plant.inp')

  call get('Lfmax',Lfmax)

  write(*,*) 'Lfmax = ',Lfmax

  call delete_input_obj()

end program test
