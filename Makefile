FC = pgfortran
OPTFLAGS = -g 
#-O3
CUDAFLAGS = -acc -gpu=cc80,nordc,lineinfo -Mcuda -Mcudalib=cublas -Minfo=accel  
FLAGS = $(OPTFLAGS) $(CUDAFLAGS)
TARGET = main 

SOURCES = $(wildcard *.f90)
OBJS = $(SOURCES:.f90=.o)
#LIBS = -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread
#FLAMEDIR=/p/software/juwelsbooster/stages/Devel-2020/software/libFLAME/2.2-GCC-9.3.0-amd/lib
#BLISDIR=/p/software/juwelsbooster/stages/Devel-2020/software/BLIS/2.2-GCC-9.3.0-amd/lib/
#LIBS =  -Wl,-rpath,$(BLISDIR) -L$(BLISDIR) -lblis -Wl,-rpath,$(FLAMEDIR) -L$(FLAMEDIR) -lflame -pthread -lcusolver
LIBS = -lcusolver

all: $(OBJS)
	$(FC) $(CUDAFLAGS) -o $(TARGET) $(OBJS) $(LIBS)

%.o: %.f90
	$(FC) $(FLAGS) -c  $<

test.o: test.f90
	$(FC) $(FLAGS) -c  $<

clean:
	rm *.o *.mod $(TARGET)

include make.dep

