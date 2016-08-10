##########################################################################
### Calculate total and by site 24-hr PM10 exceedences (state and fed) ###
### Generate monthly summary plots                                     ###
##########################################################################

#################################
## LOAD 24hr PM10 data from AQS##
#################################

# in AQS, exact 24-hr average PM10 by running
# AMP350MX selecting 24 hour PM10 (Standard Conditions; AQS code 88102) for the whole county.
# This ensures 40 CFR rules for data completeness and averaging and 
# rounding are applied. Resulting file is called "24hourPM10.txt".

source('AQSloader.R')
pm10<-load.aqs("24hourPM10.txt", format="AMP350MX") 
names(pm10)<-c("date", "mesa", "slo", "cdf", "nrp", "paso", "atas_old", "atas", "ofs")

# merge old and new atascadero data:

sum(is.na(pm10$"atas")==FALSE) # 302 samples at new location
sum(is.na(pm10$"atas_old")==FALSE) # 53 samples at old location
#old and new data don't overlap in time so..
pm10$atas[which(is.na(pm10$"atas"))]<-pm10$"atas_old"[which(is.na(pm10$"atas"))]
pm10$"atas_old"<-NULL


###############################
## Count up PM10 exceedences ##
###############################

#federal 24-hr standard

apply(pm10[,-1], 2, function(x) sum(x>155, na.rm=T))

#state 24-hr standard, by site
apply(pm10[,-1], 2, function(x) sum(x>50, na.rm=T))

#state 24-hr, countywide
sum(apply(pm10[,-1], 1, function(x) sum(x>50, na.rm=T)>0))

# number of valid sample days per site:
apply(pm10[,-1], 2, function(a) sum(is.na(a)==FALSE))


###################################################################
### figures 5 and 6                                             ###
###################################################################

#generic plotting function:
f1<-function(site, title, label=T){
  if(label==T) ylab="24-hr PM10 (ug/m3)" else ylab=""
  
  plot(0,0, pch=0, xlim=c(1,12), ylim=c(0,200), xaxt="n", xlab="",
       main=title, 
       ylab=ylab)
  
  axis(1, at=1:12, labels=unique(format(pm10$date, "%b")), las=2)
  abline(h=seq(0,200, by=50), lty="dotted", col="gray")
  abline(h=0, lty=1)
  abline(h=50, lty=2, col="red", lwd=2)
  abline(h=150, lty=1, col="red", lwd=2)
  
  plot(as.factor(format(pm10$date, "%m")), pm10[,site],
       col="pink" ,
       add=T,
       xaxt="n",
       range=0) 
}

#figure5 ##resize to 800 * 620 before cutting and pasting
par(mfrow=c(2,2),
    mar=c(4, 4, 3, 1))

f1("paso", "Paso Robles", T)
f1("atas", "Atascadero", F)
f1("slo", "San Luis Obispo", T)
f1("nrp", "Nipomo Regional Park", F)

#figure6
par(mfrow=c(2,2),
    mar=c(4, 4, 3, 1))

f1("mesa", "Mesa2", T)
f1("cdf", "CDF", F)
f1("ofs", "Oso Flaco", F)

#clean-up
rm(f1, load.aqs)
par(mfrow=c(1,1),
    mar=c(5, 4, 4, 2)+0.1) #reset plot window

