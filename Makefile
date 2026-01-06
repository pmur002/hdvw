
all:
	Rscript how-to-cite.R
	bibtex2html -nokeys -noheader -nofooter -o - how-to-cite.bib | w3m -T text/html -dump > how-to-cite.txt
	Rscript -e 'quarto::quarto_render()'

.PHONY: pdf
pdf:
	Rscript -e 'quarto::quarto_render(output_format="all")'

docker:
	sudo docker build -t pmur002/hdvw .
	sudo docker run -v "$(shell pwd)":/home/work/ -w /home/work --rm pmur002/hdvw make 

