



# ---------------------------------------------------
# Devtools based cmds
# ---------------------------------------------------
document:
	Rscript -e "devtools::document()"

.PHONY: docs
docs:
	Rscript -e "pkgdown::build_site()"

install: document
	Rscript -e "devtools::install()"

check: document
	Rscript -e "devtools::check()"

test: install
	Rscript -e "tinytest::test_package('gsdata')"
testwarn: install
	Rscript -e "tinytest::test_package('gsdata'); warnings()"

coverage: install
	Rscript -e 'covr::report(covr::package_coverage(), file = "../coverage.html")'

