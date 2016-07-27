!************************************************************************
!************************************************************************
!*     Subroutine PLANT
!*     This subroutine simulates the growth of the plant using pre-determined
!*     conditions.Hourly values of temperature and photosyntetically active
!*     radiation come from WEATHER subroutine and daily values of availability
!*     of water in the soil come from SW subroutine. This subroutine supplies
!*     the SW subroutine with daily values of leaf area index (LAI).
!C****************************************************************************

!*                  LIST OF VARIABLES 
!*     di    = daily accumulated temperature above tb (degree days)
!*     dLAI  = daily increase in leaf area index (m2/m2/d)
!*     dN    = incremental leaf number
!*     DOY   = day of the year 
!*     DYN   = dynamic control variable
!*     dw    = incremental total plant dry matter weight (g m-2)
!*     dwc   = incremental canopy dry matter weight (g m-2)
!*     dwf   = incremental fruit dry matter weight (g m-2)
!*     dwr   = incremental root dry matter weight (g m-2)
!*     E     = conversion efficiency of CH2O to plant tissue (g g-1)
!*     EMP1  = empirical coef. for expoilinear eq.
!*     EMP2  = empirical coef. for expoilinear eq.
!*     endsim= code signifying physiological maturity (end of simulation)
!*     Fcan    = fraction of total crop growth partitioned to canopy
!*     FL    = code for development phase (1=vegetative phase, 
!*                 2=reproductive phase)
!*     int   = accumulated temperature after reproductive phase starts (c)
!*     INTOT = duration of reproductive stage (degree days)
!*     LAI   = canopy leaf area index (m2 m-2)
!*     Lfmax = maximum number of leaves
!*     N     = leaf number
!*     nb    = empirical coef. for expoilinear eq.
!*     p1    = dry matter of leaves removed per plant per unit development after
!*              maximum number of leaves is reached (g)
!*     PD    = plant density m-2
!*     Pg    = canopy gross photosynthesis rate (g plant-1 day-1)
!*     PT    = photosynthesis reduction factor for temp.
!*     rm    = maximum rate of leaf appearearance (day-1)
!*     sla   = specific leaf area (m2 g-1)
!*     SRAD  = Daily solar radiation (MJ m-2)
!*     SWFAC1= soil water deficit stress factor 
!*     SWFAC2= soil water excess stress factor
!*     tb    = base temperature above which reproductive growth occurs (c)
!*     TMAX  = Daily maximum temperature (c)
!*     TMIN  = Daily manimum temperature (c)
!*     TMN   = Daily mean temperature (c)
!*     W     = total plant dry matter weight (g m-2)
!*     Wc    = canopy dry matter weight (g m-2)
!*     Wf    = fruit dry matter weight (g m-2)
!*     Wr    = root dry matter weight (g m-2)

subroutine init_plant(&
     Lfmax, EMP2,EMP1,PD,nb,rm,Fcan,tb,intot,n,lai,w,wr,wc,&
     p1,sla,endsim,count,int)

  implicit none

  character(len=400) linefmt
  real Lfmax, EMP2,EMP1,PD,nb,rm,Fcan,tb,intot,n,lai,w,wr,wc,&
       p1,sla,endsim,count,int

  endsim = 0

  OPEN (2,FILE='plant.inp',STATUS='UNKNOWN')
  READ(2,'(17(1X,F7.4))') Lfmax, EMP2,EMP1,PD,nb,rm,Fcan,tb,intot,n,lai,w,wr,wc,&
       p1,sla
  CLOSE(2)

  OPEN (1,FILE='plant.out',STATUS='REPLACE')

  linefmt =  "('Results of plant growth simulation: ')"
  WRITE(1,linefmt)
  WRITE(*,linefmt)

  linefmt = "(//,'                Accum',"//&
       "/,'       Number    Temp                                    Leaf',"//&
       "/,'  Day      of  during   Plant  Canopy    Root   Fruit    Area',"//&
       "/,'   of    Leaf  Reprod  Weight  Weight  Weight  weight   Index',"//&
       "/,' Year   Nodes    (oC)  (g/m2)  (g/m2)  (g/m2)  (g/m2) (m2/m2)',"//&
       "/,' ----  ------  ------  ------  ------  ------  ------  ------')"
  WRITE(1,linefmt)
  WRITE(*,linefmt)

  int = 0
  COUNT = 0

end subroutine init_plant

subroutine calc_plant_rates(PD,rm,Lfmax,EMP1,EMP2,nb,p1,sla,Fcan,tb,&
     TMAX,TMIN,PAR,SWFAC1,SWFAC2,&
     LAI,&
     Pg,N,dLAI,dN,dw,dwc,dwr,dwf,di)

  implicit none

  real PD,rm,Lfmax,EMP1,EMP2,nb,p1,sla,Fcan,tb,&
     TMAX,TMIN,PAR,SWFAC1,SWFAC2,&
     LAI,&
     Pg,N,dLAI,dN,dw,dwc,dwr,dwf,di
  real E,FL,PT,TMN

  TMN = 0.5 * (TMAX + TMIN)
  CALL PTS(TMAX,TMIN,PT)
  CALL PGS(SWFAC1, SWFAC2,PAR, PD, PT, LAI, Pg)

  IF (N .LT. Lfmax) THEN
!         Vegetative phase
     FL = 1.0
     E  = 1.0
     dN = rm * PT

     CALL LAIS(FL,di,PD,EMP1,EMP2,N,nb,SWFAC1,SWFAC2,PT,&
          dN,p1, sla, dLAI)
     dw = E * (Pg) * PD
     dwc = Fcan * dw
     dwr = (1-Fcan) * dw
     dwf = 0.0
     di = 0.0

  ELSE
!         Reproductive phase
     FL = 2.0
          
     IF (TMN .GE. tb .and. TMN .LE. 25) THEN
        di = (TMN-tb)
     ELSE 
        di = 0.0
     ENDIF

     E = 1.0
     CALL LAIS(FL,di,PD,EMP1,EMP2,N,nb,SWFAC1,SWFAC2,PT,&
          dN,p1, sla, dLAI)
     dw = E * (Pg) * PD
     dwf = dw
     dwc = 0.0
     dwr = 0.0
     dn = 0.0
  ENDIF

end subroutine calc_plant_rates

subroutine integrate_plant(INTOT,&
     doy,dLAI,dw,dwc,dwr,dwf,dN,di,&
     LAI,w,wc,wr,wf,N,int,endsim)

  implicit none

  integer doy,endsim
  real INTOT,dLAI,dw,dwc,dwr,dwf,dN,di,&
     LAI,w,wc,wr,wf,N,int

  LAI = LAI + dLAI
  w  = w  + dw
  wc = wc + dwc
  wr = wr + dwr
  wf = wf + dwf

  LAI = MAX(LAI,0.0)
  w   = MAX(w, 0.0)
  wc  = MAX(wc,0.0)
  wr  = MAX(wr,0.0)
  wf  = MAX(wf,0.0)

  N   = N   + dN

  int = int + di

  IF (int .GT. INTOT) THEN
     endsim = 1
     WRITE(1,"(/,'  The crop matured on day ',I3,'.')") doy
     RETURN
  ENDIF

end subroutine integrate_plant

subroutine write_plant_output(DOY,n,int,w,wc,wr,wf,lai,COUNT)

  implicit none

  integer DOY
  real n,int,w,wc,wr,wf,lai,COUNT
  character(len=400) linefmt

  WRITE(1,'(I5,7F8.2)') DOY,n,int,w,wc,wr,wf,lai
  WRITE(*,'(I5,7F8.2)') DOY,n,int,w,wc,wr,wf,lai

  IF (COUNT .EQ. 23) THEN
     COUNT = 0
     WRITE(*,'(//)')
     linefmt = "(//,'                Accum',"//&
       "/,'       Number    Temp                                    Leaf',"//&
       "/,'  Day      of  during   Plant  Canopy    Root   Fruit    Area',"//&
       "/,'   of    Leaf  Reprod  Weight  Weight  Weight  weight   Index',"//&
       "/,' Year   Nodes    (oC)  (g/m2)  (g/m2)  (g/m2)  (g/m2) (m2/m2)',"//&
       "/,' ----  ------  ------  ------  ------  ------  ------  ------')"
     write(*,linefmt)

  ENDIF

  COUNT = COUNT + 1

end subroutine write_plant_output

subroutine close_plant()

  implicit none

  close(1)

end subroutine close_plant

!************************************************************************
!*     Subroutine LAIS
!*     Calculates the canopy leaf area index (LAI)
!-----------------------------------------------------------------------  
!*     Input:  FL, di, PD, EMP1, EMP2, N, nb, SWFAC1, SWFAC2, PT, dN
!*     Output: dLAI
!*************************************************************************
SUBROUTINE LAIS(FL,di,PD,EMP1,EMP2,N,nb,SWFAC1,SWFAC2,PT,&
     dN,p1, sla, dLAI)

!-----------------------------------------------------------------------  
  IMPLICIT NONE

  REAL PD,EMP1,EMP2,N,nb,dLAI, SWFAC,a, dN, p1,sla
  REAL SWFAC1, SWFAC2, PT, di, FL
!-----------------------------------------------------------------------  

  SWFAC = MIN(SWFAC1, SWFAC2)
  IF (FL .EQ. 1.0) THEN 
     a = exp(EMP2 * (N-nb))  
     dLAI = SWFAC * PD * EMP1 * PT * (a/(1+a)) * dN 
  ELSEIF (FL .EQ. 2.0) THEN

     dLAI = - PD * di * p1 * sla 

  ENDIF
!-----------------------------------------------------------------------  
  RETURN
END SUBROUTINE LAIS
!************************************************************************



!*****************************************************************************
!*     Subroutine PGS
!*     Calculates the canopy gross photosysntesis rate (PG)
!******************************************************************************
SUBROUTINE PGS(SWFAC1, SWFAC2,PAR, PD, PT, Lai, Pg)

!-----------------------------------------------------------------------  
  IMPLICIT NONE 

  REAL PAR, Lai, Pg, PT, Y1
  REAL SWFAC1, SWFAC2, SWFAC,ROWSPC,PD

!-----------------------------------------------------------------------  
!     ROWSP = row spacing
!     Y1 = canopy light extinction coefficient

  SWFAC = MIN(SWFAC1, SWFAC2)
  ROWSPC = 60.0  
  Y1    = 1.5 - 0.768 * ((ROWSPC * 0.01)**2 * PD)**0.1 
  Pg = PT * SWFAC * 2.1 * PAR/PD * (1.0 - EXP(-Y1 * LAI))

!-----------------------------------------------------------------------  
  RETURN
END SUBROUTINE PGS
!************************************************************************



!************************************************************************
!*     Subroutine PTS
!*     Calculates the factor that incorporates the effect of temperature 
!*     on photosynthesis
!*************************************************************************
SUBROUTINE PTS(TMAX,TMIN,PT)
!-----------------------------------------------------------------------
  IMPLICIT NONE 

  REAL PT,TMAX,TMIN

!-----------------------------------------------------------------------  
  PT = 1.0 - 0.0025*((0.25*TMIN+0.75*TMAX)-26.0)**2

!-----------------------------------------------------------------------  
  RETURN
END SUBROUTINE PTS
!************************************************************************
!************************************************************************
