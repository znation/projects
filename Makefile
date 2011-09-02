GCCLIBINCLUDES = -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include
GCCLIBPATH = -lglib-2.0 -lintl -liconv -lgmp -lmpfr
GCCOPTS = -Werror -Wall -pedantic -std=c99 -O2 -ffast-math -march=pentium4 -g -pg -DGCC $(GCCLIBINCLUDES)
GCC = gcc $(GCCOPTS)
GCCLINKER = $(GCC) -o $@

CLLIBINCLUDES = -Ic:/gtkbundle/include/glib-2.0 -Ic:/gtkbundle/lib/glib-2.0/include
CLLIBPATH = /LIBPATH:"c:/gtkbundle/lib" glib-2.0.lib intl.lib
CLOPTS = /O2 $(CLLIBINCLUDES)
CL = cl.exe $(CLOPTS)
LINK = link.exe $(CCLIBPATH) /out:$@

ifneq ($(VSINSTALLDIR),)
RM = del /F
OBJEXT = obj
OUT = /out:
LIBPATH = $(CLLIBPATH)
CC = $(CL)
LINKER = $(LINK)
PRECOMPUTE = Precompute.exe
else
RM = rm -f
OBJEXT = o
LIBPATH = $(GCCLIBPATH)
CC = $(GCC)
LINKER = $(GCCLINKER)
PRECOMPUTE = ./Precompute.exe
endif

PROBLEM = Problem051
PROBLEMSRC = $(PROBLEM).c
PROBLEMDEP = Answer.h
PROBLEMOBJ = $(PROBLEM).$(OBJEXT)
OBJECTS = Utility.$(OBJEXT) BoundedArray.$(OBJEXT)

all: Solver.exe

$(PROBLEMOBJ): $(PROBLEMSRC) $(PROBLEMDEP)
	$(CC) -c $(PROBLEMSRC)

Solver.$(OBJEXT): Solver.c
	$(CC) -c Solver.c

Utility.$(OBJEXT): Utility.c Utility.h 
	$(CC) -c Utility.c

BoundedArray.$(OBJEXT): BoundedArray.c BoundedArray.h
	$(CC) -c BoundedArray.c

PrecomputedPrimes.h: Precompute.exe
	$(PRECOMPUTE) > PrecomputedPrimes.h

Prime.$(OBJEXT): Prime.c Prime.h PrecomputedPrimes.h
	$(CC) -c Prime.c

Solver.exe: $(OBJECTS) Prime.$(OBJEXT) $(PROBLEMOBJ) Solver.$(OBJEXT)
	$(LINKER) $(PROBLEMOBJ) Solver.$(OBJEXT) $(OBJECTS) Prime.$(OBJEXT) $(LIBPATH)

Precompute.$(OBJEXT): Precompute.c
	$(CC) -c Precompute.c

Precompute.exe: Precompute.$(OBJEXT) Utility.$(OBJEXT)
	$(LINKER) Precompute.$(OBJEXT) $(OBJECTS) $(LIBPATH)

clean:
	$(RM) tags *.exe *.$(OBJEXT)

