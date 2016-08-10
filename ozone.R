
####################################################################
### Calculate total and by site 8-hr exceedences (state and fed) ###
### Generate monthly ozone summary plots                         ###
####################################################################


#########################
## LOAD 8hr ozone data ##
#########################

# in AQS, exact daily max 8hr ozone by runing
# AMP350MX selecting 8 hour ozone and the whole county.
# Resulting file is called "max8ozone2015.txt".

# load AQS data download:
source('AQSloader.R')
o38hr<-load.aqs("max8ozone2015.txt", format="AMP350MX") 
names(o38hr)<-c("date", "slo", "morro", "nrp", "paso", "atas-old", "atas", "red", "carrizo")

# merge old and new atascadero sites:
o38hr$atas[which(is.na(o38hr$"atas"))]<-o38hr$"atas-old"[which(is.na(o38hr$"atas"))]
o38hr$"atas-old"<-NULL

################################
## Count up ozone exceedences ##
################################

# count exceedences of old fed 8-hr standard (75 ppb):
apply(o38hr[,2:8], 2, function(x) sum(x > 0.075, na.rm=T))

# count exceedences of new fed 8-hr standard (70 ppb) (by site):
apply(o38hr[,2:8], 2, function(x) sum(x > 0.070, na.rm=T))

# count countywide exceedences of new fed 8-hr standard (70 ppb):
sum(apply(o38hr[,2:8], 1, function(x) sum(x>0.070, na.rm=T)>0)) 

# double check county wide total against by-site totals
with(o38hr, which(red > 0.0701)) # 176 251 252 263
with(o38hr, which(carrizo > 0.0701)) # 252 263



###################################################################
### Figures 3 and 4: Monthly Ozone Summaries                    ###
###################################################################

# generic plotting function:
f1<-function(site, title, label=T){
  if(label==T) ylab="Daily Max8-hour Ozone (ppb)" else ylab=""
  
  plot(0,0, pch=0, xlim=c(1,12), ylim=c(0,90), xaxt="n", xlab="",
       main=title,
       ylab=ylab)
  
  axis(1, at=1:12, labels=unique(format(o38hr$date, "%b")), las=2)
  abline(h=seq(0,80, by=20), lty="dotted", col="gray")
  abline(h=70, lty=1, col="red", lwd=2)
  abline(h=75, lty=2, col="red", lwd=2)
  
  plot(as.factor(format(o38hr$date, "%m")), 1000*o38hr[,site],
       col="pink" ,
       add=T,
       xaxt="n",
       range=0) 
}

# figure3
par(mfrow=c(2,2),
    mar=c(4, 4, 3, 1))

f1("paso", "Paso Robles", T)
f1("atas", "Atascadero", F)
f1("red", "Red Hills", T)
f1("carrizo", "Carrizo Plains", F)

# figure4
par(mfrow=c(2,2),
    mar=c(4, 4, 3, 1))

f1("morro", "Morro Bay", T)
f1("slo", "San Luis Obispo", F)
f1("nrp", "Nipomo Regional Park", T)

# clean-up
rm(f1, load.aqs)
