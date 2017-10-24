objects = tsf2csv.o 
comp = gfortran
srclibdir = ../libsrc/
outdir = ~/bin/

tsf2csv: $(objects)
	$(comp) -o tsf2csv -static $(objects)

tsf2csv.o: tsf2csv.f90
	$(comp) -c tsf2csv.f90

install: tsf2csv
	mv ./tsf2csv $(outdir)

uninstall: tsf2csv
	rm $(outdir)/tsf2csv

clean:
	rm -rf *.mod
	rm -rf *.o
