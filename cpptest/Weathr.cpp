#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int get_weathr(ifstream &wthfile,float &srad, float &tmax, float &tmin,
	       float &rain, float &par)
{
  std::string line;

  std::getline(wthfile,line);
  sscanf(line.c_str(),"%*5d%6f%6f%6f%6f%18f",&srad,&tmax,&tmin,&rain,&par);
  par = 0.5*srad;

}
