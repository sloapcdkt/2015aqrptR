# 2015aqrptR
Data and R code for graphs and analysis in 2015 Annual Air Quality Report for San Luis Obispo County.

## Details
The 2015 AQ Report is a product of the [San Luis Obispo County Air Pollution Control District](http://www.slocleanair.org/) and will be available on the District website [here](http://www.slocleanair.org/library/air-quality-reports.php) once finalized.

## Data Sources
Almost all data used in the report were downloaded from EPA's [Air Quality System (AQS)](https://www.epa.gov/aqs). The raw data downloads are provided as `.txt` files. Data from California Department of Parks and Recreation's "S1" meteorology tower were obtained from their data management system, and is provided in the `S1_june2016.csv` file. The data in Tables 3 and 4 are from AQS AMP440 and AMP450 reports; these are provided as `.pdf` files.

## Analyses and Figures
Scripts for reproducing the analyses and figures in the report are provided as `.R` files. `AQSloader.R` contains a single function for loading certain types files spit out by AQS. Most of the other scripts make use of this function. Otherwise, the different scripts are independent, i.e. `ozone.R` depends on having sourced `AQSloader.R` but not on any of the other scripts. 

## Dependencies
The following packages will be needed: `openair`, `dplyr`, `reshape2`, `tree`, and `RColorBrewer`. All are available on [CRAN](https://cran.r-project.org/).

### Session Info:
```
R version 3.2.3 (2015-12-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 7 x64 (build 7601) Service Pack 1

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] tree_1.0-36        openair_1.6        maps_2.3-11        dplyr_0.4.3        lazyeval_0.1.10    RColorBrewer_1.1-2
[7] reshape2_1.4.1    

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.1         cluster_2.0.3       magrittr_1.5        lattice_0.20-33     R6_2.1.1           
 [6] mapdata_2.2-5       stringr_1.0.0       plyr_1.8.3          tools_3.2.3         parallel_3.2.3     
[11] grid_3.2.3          nlme_3.1-122        mgcv_1.8-9          png_0.1-7           latticeExtra_0.6-26
[16] DBI_0.3.1           assertthat_0.1      RJSONIO_1.3-0       Matrix_1.2-3        mapproj_1.2-4      
[21] stringi_0.5-5       RgoogleMaps_1.2.0.7 hexbin_1.27.1      
```