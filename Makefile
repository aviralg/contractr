RANT=~/projects/rant/bin/R

all: clean readme site preview

readme:
	R -e 'rmarkdown::render("README.Rmd")'

site:
	$(RANT) -e 'pkgdown::build_site()'

preview:
	google-chrome docs/index.html

clean:
	rm -f README.md
	$(RANT) -e 'pkgdown::clean_site(pkg = ".", path = "docs")'



