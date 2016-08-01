#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <map>

//using namespace std

extern "C" {

  void init_input_c(std::map<std::string,float> *inpmap)
  {
    std::map<std::string,float> newmap;
    *inpmap = newmap;
  }

  void read_input_c(std::map<std::string,float> *inpmap, 
		      char *filename)
{
  std::ifstream inpfile;
  std::string line;
  std::stringstream names;
  std::stringstream values;
  std::string cur_name;
  float cur_val;

  inpfile.open(filename);

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

  void get_c(std::map<std::string,float> *inpmap,char *key,float *value)
{
  std::map<std::string,float>::iterator tmp;

  tmp = (*inpmap).find(key);

  if(tmp != (*inpmap).end())
    {
      *value = (*tmp).second;
    }
  else
    {
      std::cout << key << " not found in input data structure." << std::endl;
    }
}


  void delete_input_obj_c(std::map<std::string,float> *inpmap)
{
  (*inpmap).~map();
}
}
