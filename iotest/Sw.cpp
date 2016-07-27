#include <ifstream>
#include <ofstream>

int sw(int &DOY,       // day of year
       float &LAI,     // leaf area index (m2/m2)
       float &RAIN,    // daily rainfall (mm)
       float &SRAD,    // solar radiation (mj/m2/day)
       float &TMAX,    // daily maximum temperature (c)
       float &TMIN,    // daily minimum temperature (c)
       float &SWFAC1,  // soil water deficit stress factor 
       float &SWFAC2  // soil water excess stress factor
       )
{

  float CN;      // runoff curve number
  int DATE;    // date of irrigation applications (YYDDD)
  float DP;      // depth of the profile considered in the simulation (cm)
  float DRN;     // daily subsurface drainage (mm)
  float DRNp;    // daily drainage percentage (fraction of void space)
  float DYN;     // dynamic control variable
  float EPa;     // actual daily plant transpiration (mm)
  float EPp;     // potential plant transpiration (mm)
  float ESa;     // daily soil evaporation (mm)
  float ESp;     // potential soil evaporation (mm)
  float ETp;     // daily potential evapotranspiration (mm)
  float FC;      // soil water storage at field capacity (mm)
  float FCp;     // water content at field capacity (fraction of void space)
  float INF;     // daily infiltration (mm)
  float IRR;     // daily irrigation (mm)
  float POTINF;  // potential infiltration (mm)
  float ROF;     // daily runoff (mm)
  float ST;      // soil water storage at saturation (mm)
  float STp;     // water content saturation (fraction of void space)
  float SWC;     // actual soil water storage in the profile (mm)
  float SWC_ADJ; // cumulative adjustment factor for soil water content (mm)
  float SWC_INIT;// initial soil water content (mm)
  float TDRN;    // cumulative vertical drainage (mm)
  float TEPA;    // cumulative plant transpiration (mm)
  float TESA;    // cumulative soil evaporation (mm)
  float TINF;    // cumulative infiltration (mm)
  float TIRR;    // cumulative irrigation applied (mm)
  float TRAIN;   // cumulative precipitation (mm)
  float TROF;    // cumulative runoff (mm)
  float WP;      // soil water storage at wilting point (mm)
  float WPp;     // water content at wilting point (fraction of void space)

}
