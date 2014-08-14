
SRC = raytracer.hs
HI  = $(SRC:.hs=.hi)
OBJ = $(SRC:.hs=.o)
JUNK = $(OBJ) $(HI)
FLAGS = -fno-warn-missing-methods
GHC = ghc

all: build

again: clean all

build:
	$(GHC) $(FLAGS) $(SRC)

clean:
	rm $(JUNK)
