
SRC = raytracer.hs
HI  = $(SRC:.hs=.hi)
OBJ = $(SRC:.hs=.o)
JUNK = $(OBJ) $(HI)
GHC = ghc

all: build

again: clean all

build:
	$(GHC) $(SRC)

clean:
	rm $(JUNK)
