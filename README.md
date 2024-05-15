# Make Small-Scale Fisheries Figures

The figures generated by this code depend on confidential raw data. The code presented here generates the publicly available processed data that is then used to generate the main figure for the paper. 

## To Reproduce Figure

For general audience users

1. The repo is equipped with `renv` for dependency management. To run the code, open the project in RStudio, and run

`renv::restore()`

2. Run 01_make_ssf_data.R

3. Run 02_make_ssf_figs.R. 

4. Run 03_make_devolved_rights_fig.R

4. The resulting figures and tables will be located in the results/{your run name}/{NAs are treated as zero} folder
