FILES=matrix.ml signatures.ml stats.ml markov.ml Kruskal.ml

all: $(FILES)
	corebuild matrix.native
	corebuild signatures.native
	corebuild stats.native
	corebuild markov.native
	corebuild Kruskal.native
