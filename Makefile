RANT=~/projects/rant/bin/R

all: readme site preview

readme:
	R -e 'rmarkdown::render("README.Rmd")'

site:
	$(RANT) -e 'pkgdown::build_site()'

preview:
	google-chrome docs/index.html


