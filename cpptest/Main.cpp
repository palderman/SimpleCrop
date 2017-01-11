#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int get_weathr(ifstream &wthfile,float &srad, float &tmax, float &tmin,
	       float &rain, float &par);

int openf(int &doyp,int &frop)
{
  
  std::ifstream simctrl;

  simctrl.open("Simctrl.inp");
  simctrl >> doyp;
  simctrl >> frop;
  simctrl.close();

}

int main(){
  float lai;
  float swfac1;
  float swfac2;
  float srad;
  float tmax;
  float tmin;
  float par;
  float rain;

  int doy;
  int doyp;
  int endsim;
  int frop;
  int iprint;

  ifstream weather;
  string line;

  openf(doyp,frop);

  cout << "doyp is: " << doyp << endl;
  cout << "frop is: " << frop << endl;

  weather.open("weather.inp");

  get_weathr(weather,srad,tmax,tmin,rain,par);

  weather.close();

}
