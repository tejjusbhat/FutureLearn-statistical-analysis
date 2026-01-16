FutureLearn MOOC Learning Analytics Project

NOTE: The code was written on R 4.3.x please use this version for opening the file

How to Run the Analysis

Open the project by double-clicking the file:
futurelearn_project.Rproj

Restore the project package environment by running the following command in R:
renv::restore()

Run (optional):
library(ProjectTemplate)
load.project()

Open the analysis report file:
reports/analysis_report.Rmd

Click “Knit” to run the analysis and generate the report.

NOTE: To knit you nust ensure that you have latex installed in your system, if you are unsure please run tinytex::install_tinytex() in r console

The analysis uses ProjectTemplate. All data loading and preprocessing are handled automatically by the call to load.project() in the R Markdown file. No scripts need to be run manually.

Output Location

The final output of the analysis is a PDF report located at:

reports/analysis_report.pdf