FILES=matrix.ml signatures.ml stats.ml

all: $(FILES)
	corebuild matrix.native
	corebuild signatures.native
	corebuild stats.native
