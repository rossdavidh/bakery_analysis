# bakery_analysis
linear model of in-store sales at a bakery

1_clean_data.R will take the input csv, clean and format it, and output two files: post_clean.csv (mostly for human inspection),
and post_clean.rds

2_make_models.R will read in post_clean.rds, and output one model for each category, plus one for total_sales and also a text file
called logfile_2.txt (which has R-squared values, etc.).  This script assumes the existence of a subdirectory called "models".
More recent versions will also output Box-Cox plots for use in determining if the response variable should be transformed.

3_make_plots.R will read in post_clean.rds and all of the various category models from 2_make_models.R, and assumes the existence
of a subdirectory named plots to save a variety of plots to.
