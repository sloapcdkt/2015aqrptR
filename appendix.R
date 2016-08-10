##############
## Appendix ##
##############

##############################
## Figure A1                ##
##############################

## pull exceedences counts from Annual AQ Reports. 
pm.trend<-data.frame(year=2010:2015, cdf=c(74, 65, 70, 93, 79, 62), mesa2=c(41, 33, 36, 55, 39, 30))

## get some pretty colors
library(RColorBrewer)
plotcols<-brewer.pal(10, "Spectral")

## set up plot area
plot(pm.trend$year, pm.trend$cdf, type="n",
     ylim=c(0, 100), las=2,
     ylab="Exceedences",
     xlab="",
     main="Days Exceedencing State 24-hr PM10 Standard")
abline(h=seq(0,100, by=20), lty=3, col="gray")

## CDF trend line
points(pm.trend$year, pm.trend$cdf,
       type="b",
       pch=16,
       lwd=2,
       col=plotcols[3])

## Mesa2 trend line
points(pm.trend$year, pm.trend$mesa2,
       type="b",
       pch=16,
       lwd=2,
       col=plotcols[8])

## legend 
legend(2010,100, legend=c("CDF", "Mesa2"), 
       lty=1, lwd=2, pch=16, col=plotcols[c(3,8)],
       bty="n")

## clean up
rm(pm.trend, plotcols)


################################
## Figures A2-A4              ##
################################

# load data. Historic hourly data from CDF was previous extracted from AQS and formatted
# for a different analysis, so just use that cleaned up file:
old<-read.csv("arch-cdf.csv", stringsAsFactors = FALSE) 

# format date, month, year variables
old$date<-as.POSIXct(old$date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
old$month<-format(old$date, "%m")
old$year<-format(old$date, "%Y")

# load libaries 
library(openair) # for wind rose function
library(dplyr)   # for data sorting

# plot function
plot.rose<-function(monthid){
  
  old %>%
    filter(month==monthid) %>%
    select(ws=wsv, wd=wdv, date) -> sub
  
  windRose(sub, main="", type="year", 
           paddle=FALSE, key.position="right", annotate = FALSE,
           key = list(footer="mph"), breaks=c(0, 3,6, 9, 12), grid.line=10)
}

# Figures A2-A4
plot.rose("04")   # April
plot.rose("05")   # May
plot.rose("06")   # June

# Clean up
rm(old, plot.rose)


######################################
## Filter Days Analysis             ##