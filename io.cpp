#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <map>

//using namespace std

extern "C" {

void read_plant_input(std::map<std::string,float> *inpmap, 
		      std::string *filename)
{
  std::ifstream inpfile;
  std::string line;
  std::stringstream names;
  std::stringstream values;
  std::string cur_name;
  float cur_val;

  inpfile.open((*filename).c_str());

  getline(inpfile,line);
  values.str(line);

  getline(inpfile,line);
  names.str(line);

  inpfile.close();

  while(names >> cur_name && values >> cur_val)
    {
      (*inpmap)[cur_name]=cur_val;
    }

}

void get_value(std::map<std::string,float> *inpmap,std::string *key,float *value)
{
  std::map<std::string,float>::iterator tmp;

  tmp = (*inpmap).find(*key);

  if(tmp != (*inpmap).end())
    {
      *value = (*inpmap)[*key];
    }
  else
    {
      std::cout << *key << " not found in input data structure." << std::endl;
    }
}


void delete_plant_input(std::map<std::string,float> *inpmap)
{
  (*inpmap).~map();
}
}
