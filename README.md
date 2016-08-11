# 2015aqrptR
Data and R code for graphs and analysis in 2015 Annual Air Quality Report for San Luis Obispo County.

## Details
The 2015 AQ Report is a product of the San Luis Obispo County Air Pollution Control District and will be available on the [District Website](http://www.slocleanair.org/library/air-quality-reports.php) once finalized.

## Data Sources
Almost all data used in the report were downloaded from EPA's Air Quality System (AQS). The raw data downloads are provided as `.txt` files. Data from California Department of Parks and Recreation's "S1" meteorology tower was obtained from their data management system, and is provided in the `S1_june2016.csv` file. The data in Tables 3 and 4 are from AQS AMP440 and AMP450 reports; these are provided as `.pdf` files.

## Analyses and Figures
Scripts for reproducing the analyses and figures in the report are provided as .R files. `AQSloader.R` contains a single function for loading certain types files spit out by AQS. Most of the other scripts make use of this function. Otherwise, the different scripts are independent, i.e. `ozone.R` depends on having sourced `AQSloader.R` but not on any other scripts. 

## Dependencies
The following packages will be needed: `openair`, `dplyr`, `reshape2`, `tree`, and `RColorBrewer`. All are available on [CRAN](https://cran.r-project.org/).