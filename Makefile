
all:
	Rscript -e 'quarto::quarto_render("how-data-vis-works.qmd")'
	## To allow debugging by simply source("how-data-vis-works.R")
	Rscript purl.R

