# 2015aqrptR
Data and R code for graphs and analysis in 2015 Annual Air Quality Report for San Luis Obispo County

## Notes
.txt and .csv files contain the data, and are generally raw, unaltered exports from EPA's Air Quality System (AQS) or
from State Parks DMS in the case of S1_june2016.csv.

.R files are scripts for reproducing the analyses and figures in the report. AQSloader.R contains a single function for loading certain types files spit out by AQS. Most of the other scripts make use of this function. Otherwise, the different scripts are independent, i.e. ozone.R depends on AQSloader.R but not on any other script. 

The following packages will be needed: openair, dplyr, reshape2, tree, and RColorBrewer.