program test

  use io_mod

  implicit none

  real Lfmax

  call read_plant_input('plant.inp')

  call get('Lfmax',Lfmax)

  write(*,*) Lfmax

  call delete_plant_input()

end program test
