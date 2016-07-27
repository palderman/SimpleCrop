!**************************************************************************
!**************************************************************************
!*        DRIVER for the  MODEL TO SIMULATE CROP GROWTH SUBJECTED TO             
!*           DAILY VARIATIONS OF WEATHER AND SOIL WATER CONDITIONS      
!*   Written in Microsoft FORTRAN for PC-compatible machines              
!*   Authors: RICARDO BRAGA and JADIR ROSA                                
!*   Obs: This program is an assignment of the course AGE 5646-Agricultural
!*      and Biological Systems Simulation.
!*   Date: 03/31/1997
!*   Modified 7/99 CHP - modified modular format, revised output format, 
!*         modified soil water routines, added water balance 
!C*************************************************************************
!*
!*     LIST OF VARIABLES
!*
!*     DOY    = Julian day 
!*     DOYP   = date of planting (Julian day)
!*     endsim = code signifying physiological maturity (end of simulation)
!*     FROP   = frequency of printout (days)
!*     IPRINT = code for printout (=0 for printout)
!*     LAI    = canopy leaf area index (m2 m-2)
!*     PAR    = photosynthetically active radiation (MJ/m2/d)
!*     RAIN   = daily rainfall (mm)
!*     SRAD   = daily solar radiation (MJ/m2/d)
!*     SWFAC1 = soil water deficit stress factor 
!*     SWFAC2 = soil water excess stress factor
!*     TAIRHR = hourly average temperature (Celsius)
!*     TMAX   = daily maximum temperature (Celsius)
!*     TMIN   = daily minimum temperature (Celsius)
!*
!***********************************************************************

PROGRAM MAIN

!-----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER DOY,DOYP, endsim
  INTEGER FROP, IPRINT

  real WP, FC, ST, SWC_INIT, DRNp, S, THE,&
     SWC, DP, SWFAC1, SWFAC2,&
     TRAIN, TIRR, TESA, TEPA, TROF, TDRN, TINF, SWC_ADJ

  real Lfmax, EMP2,EMP1,PD,nb,rm,tb,intot,LAI,w,wr,wc,wf,&
     p1,sla,count

  real SRAD,TMAX,TMIN,RAIN,&
             DRN, INF, ROF, EPa, ESa, ETp

  real int,Pg,dLAI,dN,dw,dwc,dwr,dwf,di,N
  real IRR,PAR
!************************************************************************
!************************************************************************
!     INITIALIZATION AND INPUT OF DATA
!************************************************************************
  CALL OPENF(DOYP, FROP)
      
  call open_wth('weather.inp')

  call init_soil_water(WP, FC, ST, SWC_INIT, DRNp, S, THE,&
     SWC, DP, SWFAC1, SWFAC2,&
     TRAIN, TIRR, TESA, TEPA, TROF, TDRN, TINF, SWC_ADJ)

  call init_plant(Lfmax, EMP2,EMP1,PD,nb,rm,fc,tb,intot,n,lai,w,wr,wc,&
     p1,sla,endsim,count)

  
!-----------------------------------------------------------------------
!     DAILY TIME LOOP 
!-----------------------------------------------------------------------
  DO 500 DOY = 0,1000
     IF (DOY .NE. 0) THEN

        call read_wth(SRAD,TMAX,TMIN,RAIN,PAR)

!************************************************************************
!************************************************************************
!     RATE CALCULATIONS
!************************************************************************
        call calc_soil_water_rates(SRAD,TMAX,TMIN,RAIN,&
             S,FC,WP,DRNp,&
             SWC,SWFAC1,SWFAC2,LAI,&
             TIRR,TRAIN,&
             DRN, INF, ROF, EPa, ESa)

          IF (DOY .GT. DOYP) THEN
             call calc_plant_rates(PD,rm,Lfmax,EMP1,EMP2,nb,p1,sla,Fc,tb,&
                  TMAX,TMIN,PAR,SWFAC1,SWFAC2,&
                  LAI,&
                  Pg,N,dLAI,dN,dw,dwc,dwr,dwf,di)
          ENDIF

!************************************************************************
!************************************************************************
!     INTEGRATION OF STATE VARIABLES
!************************************************************************
          call integrate_soil_water(ST,THE,&
               SWC,&
               INF, ESa, EPa, DRN, ROF,&
               SWFAC1,SWFAC2,&
               TINF, TESA, TEPA, TDRN, TROF, SWC_ADJ)

          IF (DOY .GT. DOYP) THEN
             call integrate_plant(INTOT,&
                  doy,dLAI,dw,dwc,dwr,dwf,dN,di,&
                  LAI,w,wc,wr,wf,N,int,endsim)
          ENDIF

        ENDIF

!************************************************************************
!************************************************************************
!     WRITE DAILY OUTPUT
!************************************************************************

        IPRINT = MOD(DOY, FROP)
        IF ((IPRINT .EQ. 0) .OR. (endsim .EQ. 1) .OR. &
             (DOY .EQ. DOYP)) THEN

     call write_soil_water_output(&
          DOY, SRAD, TMAX, TMIN, RAIN, IRR, ROF, INF, DRN,&
          ETP, ESa, EPa, SWC, DP, SWFAC1, SWFAC2)

          IF (DOY .GE. DOYP) THEN
             call write_plant_output(DOY,n,int,w,wc,wr,wf,lai,COUNT)
          ENDIF

        ENDIF

        IF (ENDSIM .EQ. 1) EXIT

!-----------------------------------------------------------------------
!     END OF DAILY TIME LOOP 
!-----------------------------------------------------------------------
  500 ENDDO

!************************************************************************
!************************************************************************
!     CLOSE FILES AND WRITE SUMMARY REPORTS
!************************************************************************
     call close_wth()

     call write_wbal(SWC_INIT, SWC, TDRN, TEPA, &
          TESA, TIRR, TRAIN, TROF, SWC_ADJ, TINF)
     call close_sw()

     call close_plant()

!      PAUSE 'End of Program - hit enter key to end'

!-----------------------------------------------------------------------  
      STOP
      END PROGRAM MAIN
!****************************************************************************



!************************************************************************
!*     SUBROUTINE OPENF(DOYP)
!*     This subroutine opens the simulation control file, and reads date of
!*     planting (DOYP)
!*
!*     SIMCTRL.INP => date of planting, frequency of printout
!************************************************************************

      SUBROUTINE OPENF(DOYP, FROP)

      IMPLICIT NONE
      INTEGER DOYP, FROP

      OPEN (UNIT=8, FILE='SIMCTRL.INP',STATUS='UNKNOWN')
      READ(8,5) DOYP, FROP
      IF (FROP .LE. 0) FROP = 1
    5 FORMAT(2I6)
      CLOSE(8)

!-----------------------------------------------------------------------  
      RETURN
      END SUBROUTINE OPENF
!************************************************************************
!************************************************************************
