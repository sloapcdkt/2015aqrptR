###########################################################################
### Calculate total and by site 24-hr PM2.5 exceedences (state and fed) ###
### Generate monthly summary plots                                      ###
###########################################################################

#################################
## LOAD 24hr PM25 data from AQS##
#################################

# In AQS, run AMP350MX selecting 24 hour PM2.5 (88101) for the whole county
# This ensures 40 CFR rules for data completeness and averaging and 
# rounding are applied. Resulting file is called "24hourPM25.txt".

source('AQSloader.R')
pm25<-load.aqs("24hourPM25.txt", format="AMP350MX") 
names(pm25)<-c("date", "mesa", "slo", "cdf", "atas_old", "atas")

#merge old and new atascadero monitors
sum(is.na(pm25$"atas")==FALSE) # 288 24-hr averages from new location
sum(is.na(pm25$"atas_old")==FALSE) # 51 24-hr averages from old location
pm25$atas[which(is.na(pm25$"atas"))]<-pm25$"atas_old"[which(is.na(pm25$"atas"))]
pm25$"atas_old"<-NULL

################################
## Count up PM2.5 exceedences ##
################################

#fed 24-hr standard

apply(pm25[,-1], 2, function(x) sum(x>35.5, na.rm=T))
apply(pm25[,-1], 2, function(x) sum(x>35.0, na.rm=T))

#fed 24-hr, countywide
sum(apply(pm25[,-1], 1, function(x) sum(x>35.5, na.rm=T)>0))

#weighed average for atascadero, means from AQS AMP450 report:

(51*11.85+288*4.99)/(51+288)


###################################################################
### figure 7                                                   ###
###################################################################

f1<-function(site, title, label=T){
  if(label==T) ylab="24-hr PM2.5 (ug/m3)" else ylab=""
  
  plot(0,0, pch=0, xlim=c(1,12), ylim=c(0,45), xaxt="n", xlab="",
       main=title, 
       ylab=ylab)
  
  axis(1, at=1:12, labels=unique(format(pm25$date, "%b")), las=2)
  abline(h=seq(0,40, by=10), lty="dotted", col="gray")
  abline(h=0, lty=1)
  abline(h=35, lty=2, col="red", lwd=2)
  
  plot(as.factor(format(pm25$date, "%m")), pm25[,site],
       col="pink" ,
       add=T,
       xaxt="n",
       range=0) 
}

#figure7 
##resize to 800 * 620 before cutting and pasting
par(mfrow=c(2,2),
    mar=c(4, 4, 3, 1))

f1("atas", "Atascadero", T)
f1("slo", "San Luis Obispo", F)
f1("mesa", "Mesa2", T)
f1("cdf", "CDF", F)


#clean-up
rm(f1, load.aqs)
par(mfrow=c(1,1),
    mar=c(5, 4, 4, 2)+0.1) #reset plot window

