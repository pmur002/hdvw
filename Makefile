
VERSION := $(shell cat VERSION)

all:
	Rscript how-to-cite.R
	bibtex2html -nokeys -noheader -nofooter -o - how-to-cite.bib | w3m -T text/html -dump > how-to-cite.txt
	Rscript -e 'quarto::quarto_render()'

.PHONY: pdf
pdf:
	Rscript -e 'quarto::quarto_render(output_format="all")'

.PHONY: docker
docker:
	sudo docker build -t pmur002/hdvw:$(VERSION) .
	sudo docker run -u "$(id -u):$(id -g)" -v "$(shell pwd)":/home/work/ -w /home/work --rm pmur002/hdvw:$(VERSION) make 

