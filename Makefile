## report   : Renders the .Rmd files
report: Rmd/data_transformations.html

Rmd/data_transformations.html: Rmd/data_transformations.Rmd data/raw/data_all.csv code/01_clean_data.R
	Rscript -e 'rmarkdown::render("$<")'

## data     : Processes the raw data
data: data/data_percentage_change.rda

data/data_percentage_change.rda: data/raw/data_all.csv code/01_clean_data.R
	R CMD BATCH code/01_clean_data.R

## install  : Installs all necessary packages
install:
	Rscript -e 'renv::restore()'

## clean    : Removes auto-generated files
clean:
	\rm -f *.Rout .Rdata

## cleanall : Removes auto-generated files, including processed data and figures
cleanall:
	\rm -f *.Rout .Rdata data/*.rda figures/*.tiff figures/*.png Rmd/*.html

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
