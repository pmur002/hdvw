
all: *.png
	Rscript generate.R
	make $?

%.png: %.dot
	dot -T png -o $@ $<
