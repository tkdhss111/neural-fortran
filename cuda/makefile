EXE      := cuda_test
SRCS     := test_cublas.f90
#SRCS     := mod_kinds.f90 mod_io.f90 mod_activation.f90 mod_random.f90 mod_layer.f90 mod_mnist.f90 mod_parallel.f90 mod_network.f90 example_mnist.f90 
VPATH    := ../src/lib/ ../src/tests/
DIR_PROJ := neural-fortran/cuda/
 
include base.mk

FC     := /opt/pgi/linux86-64/19.4/bin/pgf90
CFLAGS := -cpp -DCUDA -Mlarge_arrays -fastsse -Mcuda -Minfo -lcublas
RFLAGS := $(addprefix -I, $(DIRS_INC_RLS)) $(CFLAGS) -O4
DFLAGS := $(addprefix -I, $(DIRS_INC_DBG)) $(CFLAGS) -O0 -g

.PHONY : all run debugrun release debug prep clean extra_clean install uninstall

all: prep release 

release: $(PATH_OBJS_RLS)
	$(FC) $(RFLAGS) -o $(PATH_EXE_RLS) $(PATH_OBJS_RLS) $(LIBS_RLS)

$(DIR_OBJ_RLS)%.o: %.f90
	$(FC) $(RFLAGS) -module $(DIR_OBJ_RLS) -o $@ -c $<

debug: $(PATH_OBJS_DBG)
	$(FC) $(DFLAGS) -o $(PATH_EXE_DBG) $(PATH_OBJS_DBG) $(LIBS_DBG)

$(DIR_OBJ_DBG)%.o: %.f90
	$(FC) $(DFLAGS) -module $(DIR_OBJ_DBG) -o $@ -c $< 

debugrun: prep debug
	$(DIR_PROJS)$(DIR_PROJ)bin/Debug/$(EXE)

prep:
	$(MKDIR) $(DIR_OBJ_RLS) $(DIR_OBJ_DBG) $(DIR_EXE_RLS) $(DIR_EXE_DBG)

clean:
	$(RM) $(PATH_EXE_RLS)
	$(RM) $(DIR_OBJ_RLS)*.*
	$(RM) $(PATH_EXE_DBG)
	$(RM) $(DIR_OBJ_DBG)*.*

run: prep release
	$(DIR_PROJS)$(DIR_PROJ)bin/Release/$(EXE)
