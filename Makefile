FILES=matrix.ml signatures.ml stats.ml markov.ml cartesian_graph.ml 

all: $(FILES)
	corebuild matrix.native
	corebuild signatures.native
	corebuild stats.native
	corebuild markov.native
	corebuild cartesian_graph.native
