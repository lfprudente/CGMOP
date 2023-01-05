.SUFFIXES:
.SUFFIXES: .o .f90 .f

FC := gfortran
FFLAGS := -O3

ALGLIB := $(shell if [ -e $(CURDIR)/libalgencan.a ]; then echo true; fi)

ifneq ($(ALGLIB),true)

all: algencan nlcgvo

algencan: 
	$(MAKE) -C $(CURDIR)/algencan-3.1.1 
	mv -f $(CURDIR)/algencan-3.1.1/lib/libalgencan.a $(CURDIR)
endif

OBJECTS = globals.o myproblem.o quadfun.o scalefactor.o evalfuns.o checkd.o innersolver.o morethuente.o lsvecopt.o nlcgvo.o main.o

nlcgvo: $(OBJECTS)
	$(FC) $(FFLAGS) -o $@ $(OBJECTS) -L$(CURDIR) -lalgencan

globals.o: globals.f90
	$(FC) -c $(FFLAGS) globals.f90

globals.mod: globals.o

myproblem.o: globals.mod myproblem.f90
	$(FC) -c $(FFLAGS) myproblem.f90

myproblem.mod: myproblem.o

quadfun.o: globals.mod myproblem.mod quadfun.f90
	$(FC) -c $(FFLAGS) quadfun.f90

scalefactor.o: globals.mod myproblem.mod scalefactor.f90
	$(FC) -c $(FFLAGS) scalefactor.f90

evalfuns.o: globals.mod myproblem.mod evalfuns.f90
	$(FC) -c $(FFLAGS) evalfuns.f90

checkd.o: myproblem.mod checkd.f90
	$(FC) -c $(FFLAGS) checkd.f90
	
innersolver.o: globals.mod innersolver.f90
	$(FC) -c $(FFLAGS) innersolver.f90

morethuente.o: morethuente.f
	$(FC) -c $(FFLAGS) morethuente.f

lsvecopt.o: lsvecopt.f90
	$(FC) -c $(FFLAGS) lsvecopt.f90

nlcgvo.o: globals.mod myproblem.mod nlcgvo.f90
	$(FC) -c $(FFLAGS) nlcgvo.f90

main.o: globals.mod myproblem.mod main.f90
	$(FC) -c $(FFLAGS) main.f90

CLEAN:
	rm -f *.mod *.o nlcgvo

.PHONY: CLEAN
