
VERSION := $(shell cat VERSION)

all:
	Rscript how-to-cite.R
	bibtex2html -nokeys -noheader -nofooter -o - how-to-cite.bib | w3m -T text/html -dump > how-to-cite.txt
	Rscript -e 'quarto::quarto_render()'
	cd gdiff && Rscript gdiff.R 

.PHONY: indocker
indocker:
	# --no-init-file to avoid renv::activate() in .Rprofile (in Docker)
	# Set R_LIBS to use the renv library cache WITHIN the container
	R_LIBS=$(Rscript --no-init-file -e 'cat(renv::paths$cache())') Rscript --no-init-file how-to-cite.R
	bibtex2html -nokeys -noheader -nofooter -o - how-to-cite.bib | w3m -T text/html -dump > how-to-cite.txt
	R_LIBS=$(Rscript --no-init-file -e 'cat(renv::paths$cache())') Rscript --no-init-file -e 'quarto::quarto_render()'
	cd gdiff && Rscript --no-init-file gdiff.R 

.PHONY: pdf
pdf:
	Rscript --no-init-file -e 'quarto::quarto_render(output_format="all")'

.PHONY: docker
docker:
	sudo docker build -t pmur002/hdvw:$(VERSION) .
	sudo docker run -u "$(id -u):$(id -g)" -v "$(shell pwd)":/home/work/ -w /home/work --rm pmur002/hdvw:$(VERSION) make indocker

