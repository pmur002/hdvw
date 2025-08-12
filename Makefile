
all:
	Rscript -e 'quarto::quarto_render()'

.PHONY: pdf
pdf:
	Rscript -e 'quarto::quarto_render(output_format="all")'
