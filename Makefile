## data     : Processes the raw data
data: data/data_percentage_change.rda

data/data_percentage_change.rda: data/raw/data_LV.xlsx code/01_tidy_data.R
	R CMD BATCH code/01_tidy_data.R

## install  : Installs all necessary packages
install:
	Rscript -e 'renv::restore()'

## clean    : Removes auto-generated files
clean:
	\rm -f *.Rout .Rdata

## cleanall : Removes auto-generated files, including processed data and figures
cleanall:
	\rm -f *.Rout .Rdata data/*.rda figures/*.tiff figures/*.png

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
