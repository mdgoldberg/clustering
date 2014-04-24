FILES=matrix.ml signatures.ml

all: $(FILES)
	corebuild matrix.native
	corebuild signatures.native
