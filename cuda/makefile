EXE      := cuda_test
#SRCS     := test_cublas.f90
SRCS     := mod_kinds.f90 mod_io.f90 mod_activation.f90 mod_random.f90
SRCS     += mod_layer.f90 mod_mnist.f90 mod_parallel.f90 mod_network.f90
SRCS     += test_set_activation_function.f90
VPATH    := ../tests/

DIR_PROJS     := /home/jma/1_Projects/
DIR_PROJ      := neural-fortran/cuda/
DIR_EXE_RLS   := $(DIR_PROJS)$(DIR_PROJ)bin/Release/
DIR_EXE_DBG   := $(DIR_PROJS)$(DIR_PROJ)bin/Debug/
DIR_LIB_RLS   := $(DIR_PROJS)$(DIR_PROJ)lib/Release/
DIR_LIB_DBG   := $(DIR_PROJS)$(DIR_PROJ)lib/Debug/
DIR_OBJ_RLS   := $(DIR_PROJS)$(DIR_PROJ)obj/Release/$(EXE)/
DIR_OBJ_DBG   := $(DIR_PROJS)$(DIR_PROJ)obj/Debug/$(EXE)/

OBJS          := $(SRCS:.f90=.o)

PATH_EXE_RLS  := $(addprefix $(DIR_EXE_RLS), $(EXE))
PATH_EXE_DBG  := $(addprefix $(DIR_EXE_DBG), $(EXE))
PATH_OBJS_RLS := $(addprefix $(DIR_OBJ_RLS), $(OBJS))
PATH_OBJS_DBG := $(addprefix $(DIR_OBJ_DBG), $(OBJS))

CP            := cp
LN            := ln -s
RM            := rm -f
MKDIR         := @mkdir -p

FC     := /opt/pgi/linux86-64/19.4/bin/pgf90
CFLAGS := -cpp -DCUDA -Mcuda -lcublas
RFLAGS := $(addprefix -I, $(DIRS_INC_RLS)) $(CFLAGS) -O4
DFLAGS := $(addprefix -I, $(DIRS_INC_DBG)) $(CFLAGS) -O0 -g

.PHONY : all run debugrun release debug prep clean extra_clean install uninstall

all: prep release 


release: $(PATH_OBJS_RLS)
	$(FC) $(RFLAGS) -o $(PATH_EXE_RLS) $(PATH_OBJS_RLS) $(LIBS_RLS)

$(DIR_OBJ_RLS)%.o: %.f90
	$(FC) $(RFLAGS) -module $(DIR_OBJ_RLS) -o $@ -c $<

run: prep release
	$(DIR_PROJS)$(DIR_PROJ)bin/Release/$(EXE)


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

