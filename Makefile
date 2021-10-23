## report   : Renders the .Rmd files
report: Rmd/data_transformations.html

Rmd/data_transformations.html: Rmd/data_transformations.Rmd data/raw/data_all.csv code/01_clean_data.R
	Rscript -e 'rmarkdown::render("$<")'

## data     : Processes the raw data
data: data/data_percentage_change.rda

data/data_percentage_change.rda: data/raw/data_all.csv code/01_clean_data.R
	R CMD BATCH code/01_clean_data.R

## output   : Generates all ouput
output: output/ma_objects.rda output/meta_regression_objects.rda output/ma_radius_objects.rda output/meta_regression_radius_objects.rda output/ma_tibia_objects.rda output/meta_regression_tibia_objects.rda output/ma_DXA_QCT_objects.rda

output/ma_objects.rda output/meta_regression_objects.rda: data/data_percentage_change.rda code/02_analyze_bone_density.R code/funs.R
	R CMD BATCH code/02_analyze_bone_density.R

output/ma_radius_objects.rda output/meta_regression_radius_objects.rda: data/data_percentage_change.rda code/03_analyze_radius_bone_quality.R code/funs.R
	R CMD BATCH code/03_analyze_radius_bone_quality.R

output/ma_tibia_objects.rda output/meta_regression_tibia_objects.rda: data/data_percentage_change.rda code/04_analyze_tibia_bone_quality.R code/funs.R
	R CMD BATCH code/04_analyze_tibia_bone_quality.R

output/ma_DXA_QCT_objects.rda: data/data_percentage_change.rda code/05_compare_DXA_QCT.R code/funs.R

## figures  : Generates all figures
figures: figures/forest_plots.png figures/regplots.png figures/forest_plots_radius_bone_quality.png figures/forest_plots_tibia_bone_quality.png figures/regplots_radius_bone_quality.png figures/regplots_tibia_bone_quality.png figures/DXA_QCT.png

figures/forest_plots.png: output/ma_objects.rda figures/forest_plots.R code/funs.R
	R CMD BATCH figures/forest_plots.R

figures/regplots.png: output/ma_objects.rda output/meta_regression_objects.rda figures/regression_plot.R code/funs.R
	R CMD BATCH figures/regression_plot.R

figures/forest_plots_radius_bone_quality.png: output/ma_radius_objects.rda figures/forest_plot_radius_bone_quality.R code/funs.R
	R CMD BATCH figures/forest_plot_radius_bone_quality.R

figures/forest_plots_tibia_bone_quality.png: output/ma_tibia_objects.rda figures/forest_plot_tibia_bone_quality.R code/funs.R
	R CMD BATCH figures/forest_plot_tibia_bone_quality.R

figures/regplots_radius_bone_quality.png: output/ma_radius_objects.rda output/meta_regression_radius_objects.rda figures/regression_plots_radius_bone_quality.R code/funs.R
	R CMD BATCH figures/regression_plots_radius_bone_quality.R

figures/regplots_tibia_bone_quality.png: output/ma_tibia_objects.rda output/meta_regression_tibia_objects.rda figures/regression_plots_tibia_bone_quality.R code/funs.R
	R CMD BATCH figures/regression_plots_tibia_bone_quality.R

figures/DXA_QCT.png: output/ma_DXA_QCT_objects.rda figures/DXA_QCT.R code/funs.R
	R CMD BATCH figures/DXA_QCT.R

## install  : Installs all necessary packages
install:
	Rscript -e 'renv::restore()'

## clean    : Removes auto-generated files
clean:
	\rm -f *.Rout .Rdata

## cleanall : Removes auto-generated files, including processed data and figures
cleanall:
	\rm -f *.Rout .Rdata data/*.rda figures/*.tiff figures/*.png Rmd/*.html output/*.rda

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
