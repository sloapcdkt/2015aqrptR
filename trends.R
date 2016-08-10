################################################
## Generate Ozone and PM trend graphs         ##
################################################



################################################
### ozone trends bar graphs (Figs 8 and 9)   ###
################################################

## load 2006 to 2015 data from AQS AMP501 Raqw Data extract
source('AQSloader.R')
ozone<-load.aqs("hourlyozone.txt")
names(ozone)<-c("date", "slo", "morro", "nrp", "paso", "atas_old",
                "atas", "red", "carrizo")

#merge old and new atascadero locations
ozone$atas[which(is.na(ozone$"atas"))]<-ozone$"atas_old"[which(is.na(ozone$"atas"))]
ozone$"atas_old"<-NULL


## generic plotting function for o3 trends
f4<-function(site, title, label=T, offset=100, data=NULL){
  
  # extract data
  if(is.null(data)==T) {
    col.id<-which(names(ozone)==site)  
    dat<-tapply(ozone[,col.id], format(ozone$date, "%Y"), function(x) sum(x>=65, na.rm=T))
    
  } else dat<-data
  
  # y-axis label or not?
  if(label==T) ylab="Hours at above 65 ppb" else ylab=""
  
  # set up plot area
  barplot(dat,
          ylim=c(0,1400),
          yaxt="n",
          ylab=ylab,
          col=0,
          main=title,
          las=2)
  
  # y-axis
  axis(2, at=seq(0,1400, by=200), las=1, cex.axis=0.8)
  
  # gridlines
  abline(h=seq(200,1400, by=200), lty="dotted", col="gray")
  abline(h=0)
  
  # plot data
  barplot(dat,
          yaxt="n",
          xaxt="n",
          ylab="",
          col="pink",
          add=T)
  
  #data labels
  labs<-as.character(dat)
  #if(site=="carrizo") {           #this is to suppress printing of "0" for
  #  labs[1:2]<-c("","")           #for years without data
  #}
  text(x=1:10*1.2-0.6, y=dat+offset, labels=labs, srt=90, cex=0.8)
}


##figure8
par(mfrow=c(2,2),
    mar=c(4, 4, 3, 2))

f4("paso", "Paso Robles", T)
f4("atas", "Atascadero", F)
f4("red", "Red Hills", T, offset=-100)
barplot(1361, col="pink", add=T, yaxt="n", xlab="") ; text(0.6, 1361-100, labels="1361", srt=90, cex=0.8)
f4("carrizo", "Carrizo Plains", F)

##figure9
par(mfrow=c(2,2),
    mar=c(4, 4, 3, 1))

f4("morro", "Morro Bay", T)
f4("slo", "San Luis Obispo", F)
f4("nrp", "Nipomo Regional Park", T)


##clean up
par(mfrow=c(1,1),
    mar=c(5, 4, 4, 2)+0.1) #reset plot window

rm(ozone, f4, load.aqs)


##########################################################################
### PM10 trends bar graph, figure 11                                    ##
##########################################################################

#read in fixed width file from AMP450 report
library(reshape2)
pm10annual<-read.fwf("AnnaulPM10.txt", widths=c(5,4,9,4, 159-22,6, 179-159+6), skip=5)
pm10annual<-pm10annual[,c(2,4,6)]
names(pm10annual)<-c("site", "year", "ave")
pm10annual<-dcast(pm10annual, year~site, value.var="ave", function(x) x[1]) #take first of duplicate entries
pm10annual<-pm10annual[,-c(6,8,10:12)] # rm morro, ofs, new atas, and whatever 4003 is
pm10annual$"8001"[10]<-18.9 # merge atas sites...
names(pm10annual)<-c("year", "paso", "mesa", "slo", "cdf", "nrp", "atas")
pm10annual<-pm10annual[,c(1, 2, 7, 4, 6, 3, 5)] #reorder for plotting

#fill in proper averages b/c of FRM/FEM stuff...
pm10annual$cdf[5]<-32.4 #from Annual AQ Reports. proper averaging of TEOM and BAM data.
pm10annual$paso[5:7]<-c(19.5, 20.8, 17.5) #from AQRs. AMP450 doesn't include BAM data.
pm10annual$slo[6] <- 16.0 #from 2011 AQR. AMP450 doesn't include BAM data.
pm10annual$slo[7] <- 14.8 #from 2012 AQR. AMP450 doesn't include BAM data.
pm10annual$cdf[1:4]<-0
pm10annual$paso[8]<-21.5 #from AQRs. AMP450 doesn't handle BAM data well.
pm10annual$atas[5]<-19.2 #from AQRs. AMP450 doesn't handle BAM data well.

#Figure 10:  PM10 annual average trends
library(RColorBrewer)
plotcols<-brewer.pal(10, "Spectral")
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2) + 0.5)

barplot(as.matrix(pm10annual[,c(2:7)]),
        beside=T,
        ylim=c(0,41), 
        col= 0,
        main=expression(paste("Trends in ",PM[10]," Annual Average")), 
        ylab=expression(paste(PM[10]," Annual Average ( ", mu, "g/",m^3,")")), 
        names=c("Paso Robles", "Atascadero", "SLO", "NRP", "Mesa2", "               CDF"),
        cex.names=0.6)
abline(h=0,col="black")
abline(h=c(10,30,40),col="grey")
abline(h=20, col="red", lty=2)
barplot(as.matrix(pm10annual[,c(2:7)]),
        beside=T,
        ylim=c(0,40), 
        col= plotcols,
        yaxt="n", 
        names=rep("",6), 
        add=T)
legend("topleft", legend=c(2006:2015), cex=0.6, fill=plotcols, bg="white", horiz=F)



##########################################################################
### PM25 trends bar graph, figure 12                                    ##
##########################################################################

## read data, clean up
library(reshape2)

pm25annual<-read.csv("AnnualPM25.txt", comment.char = "#", header=F)     ## From AQS Design Value Rpt:
pm25annual<-pm25annual[,c(3,5,20,23)]                             ## these are weighted averages.
names(pm25annual)<-c("site","year","ann.ave", "complete")
pm25annual<-pm25annual[which(pm25annual$complete=="Y"),c(1:3)]          ## Get rid on incomplete years
pm25annual$ann.ave<-as.numeric(as.character(pm25annual$ann.ave))
pm25annual<-dcast(pm25annual, year~site, value.var="ann.ave")     ## Reshape data
names(pm25annual)<-c("year",  "mesa2", "slo","cdf", "atas", "atas2")
pm25annual$atas2<-NULL
pm25annual$slo[10]<-5.6  ## from this AAQR
pm25annual$atas[10]<-6.0  ## from this AAQR
pm25annual<-pm25annual[, c(1,5,3,2,4)]


##Figure 11. plot data: 2006-2015 years
library(RColorBrewer)
plotcols<-brewer.pal(10, "Spectral")
par(mfrow = c(1, 1), mar = c(5, 4, 4, 3) + 0.5)

barplot(as.matrix(pm25annual[,2:5]),
        beside=T,
        space=c(0,2),
        ylim=c(0,16),
        main=expression("Trends in " * PM[2.5] * " Annual Average"),
        #xaxt="n",
        ylab=expression(paste(PM[2.5]," Annual Average ( ", mu, "g/",m^3,")")),
        xlab="",
        col=0, 
        names=c("Atascadero",  "SLO", "             Mesa2", "               CDF"),
        cex.names=0.6 
)

abline(h=0,col="black")
abline(h=c(5,10,15),col="grey")
abline(h=12, col="red", lty=2)

legend("topleft", legend=2006:2015, fill=plotcols, bg="white", cex=0.6)

barplot(as.matrix(pm25annual[,2:5]),
        beside=T,
        space=c(0,2),
        ylim=c(0,14),
        xlab="",
        xaxt="n",
        col=plotcols,
        add=T
)


##########################################
### Ozone Design value Graph, Figure 10 ##
##########################################

dv<-read.csv("ozoneDVs.txt", comment.char="#", header=F, as.is=T)
dv<-dv[c(3,6,35,36)]
names(dv)<-c("site", "year", "value", "valid")
dv$value[dv$valid=="N"]<-NA   ## remove design values that don't meet completeness requirements
dv$value<-dv$value*1000       ##convert ppm to ppb
dv<-dcast(dv[,-4], year~site)
dv<-dv[,-c(3,4,9)]
names(dv)<-c("year", "paso", "slo", "morro", "nrp", "atas", "red", "carrizo")

plotcols<-RColorBrewer::brewer.pal(8, "Set1")[-6]

plot(dv$year, dv$paso,
     ylim=c(0,90),
     type="b", pch = 16,
     xlab="", xaxt="n", 
     ylab="Design Value (ppb)",
     col=plotcols[1],
     main="")

axis(1, at=2006:2015, las=2)

abline(h=seq(0,90, by=10), lty=3, col="gray")
abline(h=75, lty=2, col="red")
abline(h=70, lty=1, col="red")

for(i in 1:6){
  lines(dv$year, dv[,i+2],
        type="b", pch = 16,
        col = plotcols[i+1])
}

legend("bottomleft", 
       legend = c("Paso Robles", "San Luis Obispo", "Morro Bay", "NRP",
                  "Atascadero", "Red Hills", "Carrizo Plains"),
       lty=1, pch=16,
       col=plotcols)

##################
## clean up     ##
##################

rm(dv, pm10annual, pm25annual, i, plotcols)
