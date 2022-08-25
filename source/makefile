########################################################################
# Usage:
#        make ser        : Serial
#        make omp        : Parallel (OpenMP)
#        make mpi        : Parallel (SPMD MPI)
#        make mpispmd    : Parallel (SPMD MPI)
#        make mpims      : Parallel (Master/Slave MPI)
#        make hyb        : Hybrid-parallel (OpenMP+MPI(SPMD))
#        make hybspmd    : Hybrid-parallel (OpenMP+MPI(SPMD))
#        make hybms      : Hybrid-parallel (OpenMP+MPI(M/S))
#        make clean      : 
#        make allclean   : 
########################################################################
# Flag for MPI
USEMPI = true
# USEMPI = false
#
# MPI model
MPIMODEL = spmd
# MPIMODEL = ms
#
# Flag for OpenMP
USEOMP = true
# USEOMP = false
#
########################################################################

PROGRAM = sibyl

#
# OPT_STD = -fpp -O2
# OPT_STD = -fpp -O3
# OPT_STD = -fpp -O3 -xCORE-AVX2
# OPT_STD = -fpp -O3 -xCORE-AVX2 -no-prec-div -fp-model fast=2
OPT_STD = -fpp -O3 -xCORE-AVX2 -no-prec-div -fp-model fast=2 -align array64byte

# OPT_DBG = -g -traceback -check all  -no-vec -no-ftz -fpe0
# OPT_DBG = -g -traceback -check all,noarg_temp_created
# OPT_DBG = -g -traceback -parallel-source-info=2
# OPT_DBG = -g -traceback -mcmodel=large
# OPT_DBG = -g -traceback -ipo -qopt-report=5
# OPT_DBG = -g -traceback -p  # warning: using only without OpenMP/MPI
# OPT_DBG = -qopt-report=5
# OPT_DBG = -g -traceback -qopt-report=5
# OPT_DBG = -g -traceback -heap-arrays
# OPT_DBG = -g -traceback -check all
# OPT_DBG = -g -traceback -check all -DDEBUG
# OPT_DBG = -g -traceback -check all,noarg_temp_created -DDEBUG
# OPT_DBG = -g -traceback -DDEBUG
# OPT_DBG = -g -traceback
OPT_DBG =

# additional
OPT_ADD =
INC_ADD =
LIB_ADD =

########################################################################

MODULE_OBJECTS = \
common.o \
modtime.o

SIBYL_OBJECTS = \
main.o \
othersubs.o \
input.o \
restart.o \
allocate_arrays.o \
read_data.o \
response.o \
rw_restart_parameter.o \
rw_restart_data.o \
rw_restart_doses.o \
compute_doses.o \
attenuation.o \
output_result.o

MPI_OBJECTS = \
mpisubs.o

########################################################################

FC  = ifort
F77 = ifort
F90 = ifort

ifeq ($(strip $(USEMPI)), true)
  SIBYL_OBJECTS += $(MPI_OBJECTS)
  # -- MPT
  ifdef MPT_VERSION
    OPT_ADD += -DUSE_MPI
    LIB_ADD += -lmpi
    MSG_MPI = "with SGI MPT"
  endif
  # -- Intel MPI
  ifdef I_MPI_ROOT
    OPT_ADD += -DUSE_MPI
    LIB_ADD += -lmpifort -lmpi
    MSG_MPI = "with Intel MPI"
  endif

  ifeq ($(strip $(MPIMODEL)), ms)
    OPT_ADD += -DUSE_MPI_MS
    MSG_MODEL = " \(Master/Slave\)"
    EXT_ADD = _ms
  else
    OPT_ADD += -DUSE_MPI_SPMD
    MSG_MODEL = " \(SPMD\)"
    EXT_ADD = _spmd
  endif

  ifeq ($(strip $(USEOMP)), true)
    OPT_ADD += -qopenmp
    MSG_OMP = ", with OpenMP "
    EXT = .hyb${EXT_ADD}
  else
    MSG_OMP = " "
    EXT = .mpi${EXT_ADD}
  endif
else
  MSG_MPI = "without MPI"
  MSG_MODEL = ""

  ifeq ($(strip $(USEOMP)), true)
    OPT_ADD += -qopenmp
    MSG_OMP = ", with OpenMP "
    EXT = .omp
  else
    MSG_OMP = " "
    EXT = .ser
  endif
endif

MSG = "===== built ${MSG_MPI}${MSG_MODEL}${MSG_OMP}====="

########################################################################
# default target

PRG     := $(PROGRAM)$(EXT)
OBJS    := $(MODULE_OBJECTS) $(SIBYL_OBJECTS)
FFLAGS  := $(INC_ADD) $(OPT_STD) $(OPT_ADD) $(OPT_DBG)
LIBS    := $(LIB_ADD)

########################################################################

$(PRG): $(OBJS)
	$(FC) $(FFLAGS) $(OBJS) $(LIBS) -o $(PRG)
	@echo $(MSG)

########################################################################

ser:
	@$(MAKE) USEMPI=false USEOMP=false
serial:
	@$(MAKE) USEMPI=false USEOMP=false

mpi:
	@$(MAKE) USEMPI=true USEOMP=false MPIMODEL=spmd
mpispmd:
	@$(MAKE) USEMPI=true USEOMP=false MPIMODEL=spmd
mpims:
	@$(MAKE) USEMPI=true USEOMP=false MPIMODEL=ms

omp:
	@$(MAKE) USEMPI=false USEOMP=true
openmp:
	@$(MAKE) USEMPI=false USEOMP=true

hyb:
	@$(MAKE) USEMPI=true USEOMP=true MPIMODEL=spmd
hybspmd:
	@$(MAKE) USEMPI=true USEOMP=true MPIMODEL=spmd
hybms:
	@$(MAKE) USEMPI=true USEOMP=true MPIMODEL=ms
hybrid:
	@$(MAKE) USEMPI=true USEOMP=true MPIMODEL=spmd

########################################################################

.SUFFIXES: .f90 .F90 .f .F .c .o

.f90.o:
	$(FC) $(FFLAGS) -c $<

.F90.o:
	$(FC) $(FFLAGS) -c $<

.f.o:
	$(FC) $(FFLAGS) -c $<

.F.o:
	$(FC) $(FFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	/bin/rm *.o *.mod
#	/bin/rm *.o *.mod $(PROGRAM)*

allclean:
	/bin/rm -f *.o *.mod *.optrpt $(PROGRAM)*

########################################################################
# module dependency
#-----------------------------------------------------------------------

common.o:	common.f90
	$(FC) $(FFLAGS) -c $<
modtime.o:	modtime.f90
	$(FC) $(FFLAGS) -c $<

commondata.mod:	common.o common.f90
	@:
modtime.mod:	modtime.o modtime.f90
	@:

########################################################################
# special option
#
#attenuation.o:	attenuation.f90
#	$(FC) $(FFLAGS) -qopt-report=5 -c $<


