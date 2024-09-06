
all:
	Rscript -e 'quarto::quarto_render("framework.qmd")'
	## To allow debugging by simply source("framework.R")
	Rscript -e 'knitr::purl("framework.qmd")'
