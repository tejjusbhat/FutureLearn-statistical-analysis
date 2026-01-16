FutureLearn MOOC Learning Analytics Project

How to Run the Analysis

Open the project by double-clicking the file:
futurelearn_project.Rproj

Restore the project package environment by running the following command in R:
renv::restore()

Open the analysis report file:
reports/analysis_report.Rmd

Click “Knit” to run the analysis and generate the report.

The analysis uses ProjectTemplate. All data loading and preprocessing are handled automatically by the call to load.project() in the R Markdown file. No scripts need to be run manually.

Output Location

The final output of the analysis is a PDF report located at:

reports/analysis_report.pdf