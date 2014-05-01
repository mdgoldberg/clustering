FILES=matrix.ml signatures.ml stats.ml markov.ml cartesian_graph.ml main.ml Kruskal.ml

all: $(FILES)
	corebuild matrix.native
	corebuild signatures.native
	corebuild stats.native
	corebuild markov.native
	corebuild main.native
	corebuild cartesian_graph.native
	corebuild Kruskal.native

