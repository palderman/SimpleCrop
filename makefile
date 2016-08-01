Data/simplecrop.so:Main.f90 Weathr.f90 Sw.f90 Plant.f90
	gfortran -g *.f90 -o Data/SimpleCrop.so

orig:
	gfortran -g *.for -o Data/orig.so

testio:
	g++ -g -c io.cpp
	gfortran -g -c io_mod.f90
	gfortran -g -c testmain.f90
	gfortran *.o -o Data/testio.so -lstdc++
