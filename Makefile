
all:
	Rscript -e 'quarto::quarto_render("how-data-vis-works.qmd")'
	## To allow debugging by simply source("how-data-vis-works.R")
	Rscript purl.R
	## make web

web:
	cp how-data-vis-works.html ~/Web/HDVW/
	cp style.css ~/Web/HDVW/
	cp -r how-data-vis-works_files ~/Web/HDVW/
	cp -r Diagrams ~/Web/HDVW/
	cp -r Figures ~/Web/HDVW/
	cp -r Model ~/Web/HDVW/
	cp -r Mappings ~/Web/HDVW/
	cp -r MASSVIS ~/Web/HDVW/
	cp -r Images ~/Web/HDVW/
	cp -r Data/RWC/WordCloud ~/Web/HDVW/Data/RWC/

