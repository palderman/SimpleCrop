odir = obj
objf = $(odir)/Main.o $(odir)/Weathr.o $(odir)/Sw.o $(odir)/Plant.o $(odir)/io_mod.o $(odir)/io.o
fc = -I$(odir) -J$(odir)
fl = -lstdc++

Data/simplecrop.so:$(objf)
	@ mkdir -p $(odir)
	gfortran -o Data/SimpleCrop.so $(objf) $(fl)

$(odir)/Main.o:Main.for $(odir)/io_mod.mod
	@ mkdir -p $(odir)
	gfortran $(fc) -c Main.for -o $@

$(odir)/Weathr.o:Weathr.for $(odir)/io_mod.mod
	@ mkdir -p $(odir)
	gfortran $(fc) -c Weathr.for -o $@

$(odir)/Sw.o:Sw.for $(odir)/io_mod.mod
	@ mkdir -p $(odir)
	gfortran $(fc) -c Sw.for -o $@

$(odir)/Plant.o:Plant.for $(odir)/io_mod.mod
	@ mkdir -p $(odir)
	gfortran $(fc) -c Plant.for -o $@

$(odir)/io_mod.o:io_mod.f90
	@ mkdir -p $(odir)
	gfortran $(fc) -c io_mod.f90 -o $@

$(odir)/io_mod.mod:io_mod.f90
	@ mkdir -p $(odir)
	gfortran $(fc) -c io_mod.f90 -o $(odir)/io_mod.o

$(odir)/io.o:io.cpp
	@ mkdir -p $(odir)
	g++ -c io.cpp -o $(odir)/io.o


orig:
	gfortran -g *.for -o Data/orig.so

testio:
	gfortran -g -c testmain.f90
	gfortran *.o -o Data/testio.so -lstdc++
