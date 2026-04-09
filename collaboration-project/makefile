all:
	Rscript -e "targets::tar_make()"
	open readme.html

clean:
	Rscript -e "targets::tar_destroy()"

test:
	Rscript -e "testthat::test_dir('tests/testthat/')"

folder:
	open .
