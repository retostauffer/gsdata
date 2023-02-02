



# ---------------------------------------------------
# Devtools based cmds
# ---------------------------------------------------
document:
	Rscript -e "devtools::document()"

install: document
	Rscript -e "devtools::install()"

check: document
	Rscript -e "devtools::check()"

test: install
	Rscript -e "tinytest::test_package('geosphere')"
testwarn: install
	Rscript -e "tinytest::test_package('geosphere'); warnings()"

coverage: install
	Rscript -e 'covr::report(covr::package_coverage(), file = "../coverage.html")'

# ---------------------------------------------------
# Command line version
# ---------------------------------------------------
packageversion:=$(shell cat DESCRIPTION | egrep Version | sed 's/Version://g')
cmd: SHELL:=/bin/bash
cmd: cmd
	(cd ../ && \
		R CMD build --no-build-vignettes topmodels && \
		R CMD INSTALL geosphere_$(shell printf "%s"${packageversion}).tar.gz)
