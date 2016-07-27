!************************************************************************
!************************************************************************
!*     subroutine reat_wth Reads daily weather data from file
!************************************************************************
!*
!*     LIST OF VARIABLES
!*
!*     DATE = date of weather record (YYDDD)
!*     DYN  = dynamic control variable
!*     PAR  = photosynthetically active radiation (MJ/m2/d)
!*     RAIN = daily rainfall (mm)
!*     SRAD = daily solar radiation (MJ/m2/d)
!*     TMAX = daily maximum temperature (Celsius)
!*     TMIN = daily minimum temperature (Celsius)
!*
!***********************************************************************
subroutine read_wth(srad,tmax,tmin,rain,par)

  implicit none

  real SRAD,TMAX,TMIN,RAIN,PAR

  read(4,'(7X,F4.1,2X,F4.1,2X,F4.1,F6.1,14X,F4.1)') srad,tmax,tmin,rain,par
  par = 0.5 *  srad         ! Par is defined as 50% of SRAD

  return
end subroutine read_wth

subroutine  open_wth(filename)

  implicit none

  character(len=*) filename

  open(unit=4,file=filename)

end subroutine open_wth


subroutine close_wth()

  implicit none

  close(4)

end subroutine close_wth
