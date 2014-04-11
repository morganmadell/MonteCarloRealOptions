library(timeSeries)
library(lubridate)
library(fitdistrplus)
library(timeDate)
library(stringr)
library(lubridate)
library(gdata)
library(timeSeries)
library(xts)
library(fitdistrplus)
library(grid)

### File Path to folder that contains the above files
path <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Price Differentials"
setwd(path)

###########################################
#### Length of Time Series
fc.periods <- 12*50 ####### 50 Years
n <- 1000 ######## Number of runs/iterations
###########################################

##################################################################################################################################
#### Import Necessary Pricing Files
# Henry Hub Import
setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/CSV Files/50 Years")
hh.hist.fc.upload <- read.csv("HH Monte Carlo - 1000 Price Series (most recent).csv",header=TRUE,sep=",",dec=".")
hh.dates <- as.character(hh.hist.fc.upload[,1])
hh.values <- as.matrix(hh.hist.fc.upload[,-1])
hh.hist.fc.ts <- timeSeries(hh.values,hh.dates)
# USD-CAD Exchange Import
setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/CAD US EX/CSV Files")
usd.cad.ex.hist.fc.upload <- read.csv("USD-CAD Monte Carlo - 1000 Price Series (most recent).csv",header=TRUE,sep=",",dec=".")
usd.cad.ex.dates <- as.character(usd.cad.ex.hist.fc.upload[,1])
usd.cad.ex.values <- as.matrix(usd.cad.ex.hist.fc.upload[,-1])
usd.cad.ex.hist.fc.ts <- timeSeries(usd.cad.ex.values,usd.cad.ex.dates)
# US CPI Import
setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/CAD US CPI/CSV Files")
cpi.index.year <- year(Sys.Date())-1
us.cpi.hist.fc.upload <- read.csv(paste("US Hist, FC CPI - Index Year ",cpi.index.year,".csv",sep=""),header=TRUE,sep=",",dec=".")
us.cpi.dates <- as.character(us.cpi.hist.fc.upload[,1])
us.cpi.values <- as.matrix(us.cpi.hist.fc.upload[,-1])
us.cpi.hist.fc.ts <- timeSeries(us.cpi.values,us.cpi.dates)

########################### Sproule Differentials ######################################
current.date <- Sys.Date()
current.year <- year(current.date)
current.month <- month(current.date)

# Web address of the current Sproule pricing
#http://www.sproule.com/system/resources/W1siZiIsIjIwMTQvMDEvMDcvMTcvMjQvMjkvMzk4LzIwMTMxMl9Fc2NhbGF0ZWQueGxzeCJdXQ
#sproule.root <- "http://www.sproule.com/system/resources/W1siZiIsIjIwMTQvMDEvMDcvMTcvMjQvMjkvMzk4LzIwMTMxMl9Fc2NhbGF0ZWQueGxzeCJdXQ/"

#if (current.month == 1) {last.sproule.month <- paste(current.year-1,12,sep="")
#} else {last.sproule.month <- paste(current.year, current.month-1,sep="")}
#if (current.month<10) {current.month <- paste("0",current.month,sep="")}
#current.sproule.month <- paste(current.year,current.month,sep="")

sproule.root <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Price Differentials/Sproule Historical/"
current.sproule.address <- paste(sproule.root,"Current Escalated.csv",sep="")
#last.sproule.address <- paste(sproule.root, last.sproule.month,"-Escalated.xlsx",sep="")

sproule.hist.df.raw <- read.csv(current.sproule.address)

# Combine Data
sproule.hist.df <- sproule.hist.df.raw[-c(1:7),]
colnames(sproule.hist.df) <- c("Date","WTI ($US/bbl)","Edm Par ($CA/bbl)","SCO Edm ($CA/bbl)","Cromer ($CA/bbl)","Brent ($US/bbl)","Hardisty Hvy ($CA/bbl)","WCS ($CA/bbl)","Hardisty Bow River ($CA/bbl)","Ethane ($CA/bbl)","Propane ($CA/bbl)","Butane ($CA/bbl)","Pentane ($CA/bbl)","Sulphur ($CA/Lt)","USD-CAD EX ($US/$CA)","USD-UKLB ($US/UKLB)","AB Gov Ref ($CA/mcf)","AB 30d Spot AECO ($CA/mcf)","Alliance ($CA/mcf)","Canwest WH ($CA/mcf)","BC EMP WH ($CA/mcf)","BC WC Station2 ($CA/mcf)","BC 30d Spot ($CA/mcf)","Dawn ($CA/mcf)","HH NYMEX Cl ($US/mmbtu)","IPE Britain NBP (LB/mmbtu)")
sproule.hist.df.del <- grep("Average",sproule.hist.df[,1],fixed=TRUE)
sproule.hist.df[sproule.hist.df==""] <- "NA" 
sproule.hist <- sproule.hist.df[-sproule.hist.df.del,]

d1 <- as.character(sproule.hist[,1])
date.con <- as.data.frame(matrix(nrow=length(d1),ncol=1))
for (i in 1:length(d1)){
if (is.na(d1[i])==FALSE) {
	sub.month <- str_sub(d1[i],start=0,end=3)
	sub.year <- str_sub(d1[i],start=5)
	if (as.numeric(sub.year) < 50){year.con <- paste("20",sub.year,sep="")} else {year.con <- paste("19",sub.year,sep="")}
	if (sub.month == "Jan"){month.con <- "01"
	} else if (sub.month == "Feb"){month.con <- "02"
	} else if (sub.month == "Mar"){month.con <- "03"
	} else if (sub.month == "Apr"){month.con <- "04"
	} else if (sub.month == "May"){month.con <- "05"
	} else if (sub.month == "Jun"){month.con <- "06"
	} else if (sub.month == "Jul"){month.con <- "07"
	} else if (sub.month == "Aug"){month.con <- "08"
	} else if (sub.month == "Sep"){month.con <- "09"
	} else if (sub.month == "Oct"){month.con <- "10"
	} else if (sub.month == "Nov"){month.con <- "11"
	} else if (sub.month == "Dec"){month.con <- "12"
	} else {month.con <- "NA"}
date.con[i,] <- paste(year.con,"-",month.con,"-01",sep="")
} else {date.con[i,] <- "NA"}}

dates.d <- date.con
dates.del <- grep("NA",dates.d[,1],fixed=TRUE)
dates <- dates.d[-dates.del,]
sproule.hist.ts <- timeSeries(sproule.hist[-dates.del,-1],dates,format='%Y-%m-%d')
wti.usd.ts <- removeNA(sproule.hist.ts[,1])
edm.par.cad.ts <- removeNA(sproule.hist.ts[,2])
sco.edm.cad.ts <- removeNA(sproule.hist.ts[,3])
cromer.cad.ts <- removeNA(sproule.hist.ts[,4])
brent.usd.ts <- removeNA(sproule.hist.ts[,5])
hard.hvy.cad.ts <- removeNA(sproule.hist.ts[,6])
wcs.cad.ts <- removeNA(sproule.hist.ts[,7])
hard.bow.r.cad.ts <- removeNA(sproule.hist.ts[,8])
ethane.cad.ts <- removeNA(sproule.hist.ts[,9])
propane.cad.ts <- removeNA(sproule.hist.ts[,10])
butane.cad.ts <- removeNA(sproule.hist.ts[,11])
pentane.cad.ts <- removeNA(sproule.hist.ts[,12])
sulphur.cad.ts <- removeNA(sproule.hist.ts[,13])
usd.cad.ex.ts <- removeNA(sproule.hist.ts[,14])
usd.uklb.ex.ts <- removeNA(sproule.hist.ts[,15])
ab.gov.ref.cad.ts <- removeNA(sproule.hist.ts[,16])
ab.30d.spot.aeco.cad.ts <- removeNA(sproule.hist.ts[,17])
alliance.cad.ts <- removeNA(sproule.hist.ts[,18])
canwest.wh.cad.ts <- removeNA(sproule.hist.ts[,19])
bc.emp.wh.cad.ts <- removeNA(sproule.hist.ts[,20])
bc.wc.station2.cad.ts <- removeNA(sproule.hist.ts[,21])
bc.30d.spot.sumas.cad.ts <- removeNA(sproule.hist.ts[,22])
dawn.cad.ts <- removeNA(sproule.hist.ts[,23])
hh.nymex.cl.usd.ts <- removeNA(sproule.hist.ts[,24])
ipe.britain.nbp.uklb.ts <- removeNA(sproule.hist.ts[,25])

################################### End of Sproule Data + 1 month ###########################################################
end.sproule <- end(ab.30d.spot.aeco.cad.ts)
end.sproule.y <- year(end.sproule)
end.sproule.m <- month(end.sproule)

############################### Henry Hub - Split Forecast from History ########################################################################
if (end.sproule.m==12) {start.fc <- as.Date(paste(end.sproule.y+1,"-01-01",sep=""),format='%Y-%m-%d')
	} else {start.fc <- as.Date(paste(end.sproule.y,"-",end.sproule.m+1,"-01",sep=""),format='%Y-%m-%d')}
start.fc.y <- year(start.fc)
start.fc.m <- month(start.fc)

#### Separate out FC and mutual history (mh)
start.dates <- c(start(hh.hist.fc.ts),start(usd.cad.ex.hist.fc.ts), start(us.cpi.hist.fc.ts))
start.date <- max(start.dates)
end.dates <- c(end(hh.hist.fc.ts),end(usd.cad.ex.hist.fc.ts), end(us.cpi.hist.fc.ts))
end.date <- min(end.dates)

#### Separate out FC windows
hh.fc.ts <- rev(window(hh.hist.fc.ts,start=start.fc,end.date))
usd.cad.ex.fc.ts <- rev(window(usd.cad.ex.hist.fc.ts,start=start.fc,end.date))
us.cpi.fc.ts <- window(us.cpi.hist.fc.ts,start=start.fc,end.date)

#### Create one window size for HH Hist&FC, CPI Hist&FC, USD-CAD Ex Hist&FC
hh.mh.fc.ts <- rev(window(hh.hist.fc.ts,start=start.date,end=end.date))
usd.cad.ex.mh.fc.ts <- rev(window(usd.cad.ex.hist.fc.ts,start=start.date,end=end.date))
us.cpi.mh.fc.ts <- window(us.cpi.hist.fc.ts,start=start.date,end=end.date)

################# Price Conversions
#### Create HH in Real Dollars
hh.real.fc.ts <- hh.fc.ts*us.cpi.fc.ts/100 # Just the forecast prices
hh.real.mh.fc.ts <- hh.mh.fc.ts*us.cpi.mh.fc.ts/100 # Entire communal time series

#### Create Real HH in CAD
hh.real.cad.fc.ts <- hh.real.fc.ts*usd.cad.ex.fc.ts # Just the forecast prices
hh.real.cad.mh.fc.ts <- hh.real.mh.fc.ts*usd.cad.ex.mh.fc.ts # Entire communal time series

#################### Create TS to compare against AECO 30D Spot###################
hh.real.cad_aeco.win <- window(hh.real.cad.mh.fc.ts[,1], start=start(ab.30d.spot.aeco.cad.ts), end=end.sproule)

#################### Create Linear Model between Real HH prices in CAD and AECO 30D Spot #########################################
hh.aeco.lm <- lm(ab.30d.spot.aeco.cad.ts~hh.real.cad_aeco.win)
dev.new()
plot(hh.real.cad_aeco.win,ab.30d.spot.aeco.cad.ts,xlab="Henry Hub ($CAD/mcf)",ylab="AECO 30 Day Spot ($CAD/mcf)",col="blue");grid()
abline(hh.aeco.lm,col="green")

# HH-AECO Details
hh.aeco.lm.intercept <- as.numeric(hh.aeco.lm$coefficients[1])
hh.aeco.lm.slope <- as.numeric(hh.aeco.lm$coefficients[2])
hh.aeco.lm.residuals <- as.numeric(hh.aeco.lm$residuals)

# Fit HH-AECO residual distribution ########################## Logistic Distribution is the best fit
hh.aeco.lm.logis <- fitdist(hh.aeco.lm.residuals,"logis")
summary(hh.aeco.lm.logis)
dev.new()
plot(hh.aeco.lm.logis,breaks=30)
title(main="Logistic")
#hh.aeco.lm.norm <- fitdist(hh.aeco.lm.residuals,"norm")
#summary(hh.aeco.lm.norm)
#dev.new()
#plot(hh.aeco.lm.norm,breaks=30)
#title(main="Normal")

# Create forecasted residuals
hh.aeco.dist.location <- as.numeric(summary(hh.aeco.lm.logis)[[1]][1])
hh.aeco.dist.scale <- as.numeric(summary(hh.aeco.lm.logis)[[1]][2])

#### AECO forecast
aeco.fc.ts <- rev(window(hh.hist.fc.ts,start=start.fc,end=end(hh.real.cad.fc.ts))) # Dummy series to create data structure

#### AECO-HH Residual forecast
aeco.res.fc.ts <- window(hh.hist.fc.ts,start=start.fc,end=end(hh.real.cad.fc.ts)) # Dummy series to create data structure
for (i in 1:length(aeco.res.fc.ts[,1])) {
	aeco.res.fc.ts[i,] <- rlogis(n, location=hh.aeco.dist.location, scale=hh.aeco.dist.scale)
}

################################ Create AECO Forecast ###########################################################
aeco.fc.ts <- hh.aeco.lm.slope * hh.real.cad.fc.ts + hh.aeco.lm.intercept + rev(aeco.res.fc.ts)

################################ AECO: Rebind Forecasts to History ##############################################
aeco.real.cad.hist.fc.ts <- rbind(ab.30d.spot.aeco.cad.ts,aeco.fc.ts[,1])
for (i in 2:length(aeco.fc.ts[1,])){
	aeco.real.cad.hist.fc.ts <- cbind(aeco.real.cad.hist.fc.ts, rbind(ab.30d.spot.aeco.cad.ts,aeco.fc.ts[,i]))}


setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/CSV Files/50 Years")
write.csv(hh.real.mh.fc.ts,file=paste("HH (Real,USD) Monte Carlo - ",n," series (most recent).csv",sep=""))
write.csv(hh.real.cad.mh.fc.ts,file=paste("HH (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(aeco.real.cad.hist.fc.ts,file=paste("AECO (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))

##################################################### AECO derivative forecasting ##################################################################################

##################################################### Compare Ethane versus AECO ########################################################################
###### Sproule's ethane price is just a ratio of AECO: $1/mcf AECO=$2.771/bbl
#aeco_ethane.win <- window(ab.30d.spot.aeco.cad.ts,start=start(ethane.cad.ts),end=end(ethane.cad.ts))
#aeco_ethane.lm <- lm(as.numeric(ethane.cad.ts[,1])~as.numeric(aeco_ethane.win[,1]))
#aeco_ethane.lm.summary <- summary(aeco_ethane.lm)
#aeco_ethane.lm.intercept <- aeco_ethane.lm.summary$coefficients[1,1]
#aeco_ethane.lm.slope <- aeco_ethane.lm.summary$coefficients[2,1]
#aeco_ethane.lm.res <- aeco_ethane.lm.summary$residuals
#dev.new()
#plot(as.numeric(aeco_ethane.win[,1]),as.numeric(ethane.cad.ts[,1]),xlab="AECO ($CA/mcf)",ylab="Ethane ($CA/bbl)",col="blue");grid()
#title(main="Ethane vs AECO")
#abline(aeco_ethane.lm,col="green")
## Logistic distribution - worse fit
#aeco_ethane.lm.logis <- fitdist(aeco_ethane.lm.res,"logis")
#summary(aeco_ethane.lm.logis)
#dev.new()
#plot(aeco_ethane.lm.logis,breaks=30)
#title(main="Logistic")
## Normal distribution - worse fit
#aeco_ethane.lm.norm <- fitdist(aeco_ethane.lm.res,"norm")
#summary(aeco_ethane.lm.norm)
#dev.new()
#plot(aeco_ethane.lm.norm,breaks=30)
#title(main="Normal")

################################################## Ethane Forecasting - Used ratio of Ethane:AECO Pricing on AECO Forecasts #################################
ethane.cad.hist_fc.dates <- as.character(timeSequence(start(aeco.real.cad.hist.fc.ts), length.out=length(aeco.real.cad.hist.fc.ts[,1]),by="month"))
#colnames(ethane.cad.hist_fc.dates) <- c("Date")
ethane.cad.hist_fc.values <- matrix(nrow=length(aeco.real.cad.hist.fc.ts[,1]),ncol=length(aeco.real.cad.hist.fc.ts[1,]))
for (i in 1:length(aeco.real.cad.hist.fc.ts[1,])) {ethane.cad.hist_fc.values[,i] <- as.numeric(aeco.real.cad.hist.fc.ts[,i]) * 2.771}
ethane.cad.hist_fc.values <- as.data.frame(ethane.cad.hist_fc.values)
ethane.cad.hist_fc.ts <- timeSeries(ethane.cad.hist_fc.values,ethane.cad.hist_fc.dates, format='%Y-%m-%d')

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(ethane.cad.hist_fc.ts,file=paste("Ethane (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))

########################################################### Compare AB Reference Price versus AECO #####################################################
#### Logistic Distribution better describes residuals
aeco_ab.ref.win <- window(ab.30d.spot.aeco.cad.ts,start=start(ab.gov.ref.cad.ts),end=end(ab.gov.ref.cad.ts))
aeco_ab.ref.lm <- lm(as.numeric(ab.gov.ref.cad.ts[,1])~as.numeric(aeco_ab.ref.win[,1]))
aeco_ab.ref.lm.summary <- summary(aeco_ab.ref.lm)
aeco_ab.ref.lm.intercept <- aeco_ab.ref.lm.summary$coefficients[1,1]
aeco_ab.ref.lm.slope <- aeco_ab.ref.lm.summary$coefficients[2,1]
aeco_ab.ref.lm.res <- aeco_ab.ref.lm.summary$residuals
dev.new()
plot(as.numeric(aeco_ab.ref.win[,1]),as.numeric(ab.gov.ref.cad.ts[,1]),xlab="AECO ($CA/mcf)",ylab="AB Reference Price ($CA/bbl)",col="blue");grid()
title(main="AB Reference Price vs AECO")
abline(aeco_ab.ref.lm,col="green")
## Logistic distribution
aeco_ab.ref.lm.logis <- fitdist(aeco_ab.ref.lm.res,"logis")
summary(aeco_ab.ref.lm.logis)
dev.new()
plot(aeco_ab.ref.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#aeco_ab.ref.lm.norm <- fitdist(aeco_ab.ref.lm.res,"norm")
#summary(aeco_ab.ref.lm.norm)
#dev.new()
#plot(aeco_ab.ref.lm.norm,breaks=30)

### AB Reference Price Forecast
ab.ref.location <- as.numeric(summary(aeco_ab.ref.lm.logis)[[1]][1])
ab.ref.scale <- as.numeric(summary(aeco_ab.ref.lm.logis)[[1]][2])
# Create distribution forecast
end.ab.ref <- end(ab.gov.ref.cad.ts)
if (month(end.ab.ref)==12) {ab.ref.fc.start <- as.Date(paste(year(end.ab.ref)+1,"-01-01",sep=""),format="%Y-%m-%d")
} else {ab.ref.fc.start <- as.Date(paste(year(end.ab.ref),"-",month(end.ab.ref)+1,"-01",sep=""),format="%Y-%m-%d")}

aeco_ab.ref.fc.win <- window(aeco.real.cad.hist.fc.ts,start=ab.ref.fc.start,end=end(aeco.real.cad.hist.fc.ts)) # Forecast Window

forecast.dist.ab.ref <- aeco_ab.ref.fc.win
for (i in 1:length(aeco_ab.ref.fc.win[,1])) {forecast.dist.ab.ref[i,] <- rlogis(n, location=ab.ref.location, scale=ab.ref.scale)}
colnames(forecast.dist.ab.ref) <- paste0("FC ",1:1000)

# Translate distribution FC into a time series
ab.ref.fc.values <- matrix(nrow=length(aeco_ab.ref.fc.win[,1]),ncol=length(aeco_ab.ref.fc.win[1,]))
for (i in 1:length(aeco_ab.ref.fc.win[1,])) {
	ab.ref.fc.values[,i] <- aeco_ab.ref.lm.slope * as.numeric(aeco_ab.ref.fc.win[,i]) + aeco_ab.ref.lm.intercept + as.numeric(forecast.dist.ab.ref[,i])}
ab.ref.fc.dates <- as.character(timeSequence(start(aeco_ab.ref.fc.win), length.out=length(aeco_ab.ref.fc.win[,1]),by="month"))
ab.ref.fc.ts <- timeSeries(ab.ref.fc.values, ab.ref.fc.dates, format='%Y-%m-%d')
ab.ref.hist_fc.ts <- rbind(ab.gov.ref.cad.ts, ab.ref.fc.ts[,1])
for (i in 2:length(ab.ref.fc.ts[1,])) {ab.ref.hist_fc.ts <- cbind(ab.ref.hist_fc.ts, rbind(ab.gov.ref.cad.ts, ab.ref.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(ab.ref.hist_fc.ts,file=paste("AB Reference (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


########################################################### Compare Alliance versus AECO #################################################################
#### Logistic Distribution better describes residuals
aeco_alliance.win <- window(ab.30d.spot.aeco.cad.ts,start=start(alliance.cad.ts),end=end(alliance.cad.ts))
aeco_alliance.lm <- lm(as.numeric(alliance.cad.ts[,1])~as.numeric(aeco_alliance.win[,1]))
aeco_alliance.lm.summary <- summary(aeco_alliance.lm)
aeco_alliance.lm.intercept <- aeco_alliance.lm.summary$coefficients[1,1]
aeco_alliance.lm.slope <- aeco_alliance.lm.summary$coefficients[2,1]
aeco_alliance.lm.res <- aeco_alliance.lm.summary$residuals
dev.new()
plot(as.numeric(aeco_alliance.win[,1]),as.numeric(alliance.cad.ts[,1]),xlab="AECO ($CA/mcf)",ylab="Alliance ($CA/bbl)",col="blue");grid()
title(main="Alliance vs AECO")
abline(aeco_alliance.lm,col="green")
## Logistic distribution
aeco_alliance.lm.logis <- fitdist(aeco_alliance.lm.res,"logis")
summary(aeco_alliance.lm.logis)
dev.new()
plot(aeco_alliance.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#aeco_alliance.lm.norm <- fitdist(aeco_alliance.lm.res,"norm")
#summary(aeco_alliance.lm.norm)
#dev.new()
#plot(aeco_alliance.lm.norm,breaks=30)
#title(main="Normal")

### Alliance Price Forecast
alliance.location <- as.numeric(summary(aeco_alliance.lm.logis)[[1]][1])
alliance.scale <- as.numeric(summary(aeco_alliance.lm.logis)[[1]][2])
# Create distribution forecast
end.alliance <- end(alliance.cad.ts)
if (month(end.alliance)==12) {alliance.fc.start <- as.Date(paste(year(end.alliance)+1,"-01-01",sep=""),format="%Y-%m-%d")
} else {alliance.fc.start <- as.Date(paste(year(end.alliance),"-",month(end.alliance)+1,"-01",sep=""),format="%Y-%m-%d")}

aeco_alliance.fc.win <- window(aeco.real.cad.hist.fc.ts,start=alliance.fc.start,end=end(aeco.real.cad.hist.fc.ts)) # Forecast Window

forecast.dist.alliance <- aeco_alliance.fc.win
for (i in 1:length(aeco_alliance.fc.win[,1])) {forecast.dist.alliance[i,] <- rlogis(n, location=alliance.location, scale=alliance.scale)}
colnames(forecast.dist.alliance) <- paste0("FC ",1:1000)

# Translate distribution FC into a time series
alliance.fc.values <- matrix(nrow=length(aeco_alliance.fc.win[,1]),ncol=length(aeco_alliance.fc.win[1,]))
for (i in 1:length(aeco_alliance.fc.win[1,])) {
	alliance.fc.values[,i] <- aeco_alliance.lm.slope * as.numeric(aeco_alliance.fc.win[,i]) + aeco_alliance.lm.intercept + as.numeric(forecast.dist.alliance[,i])}
alliance.fc.dates <- as.character(timeSequence(start(aeco_alliance.fc.win), length.out=length(aeco_alliance.fc.win[,1]),by="month"))
alliance.fc.ts <- timeSeries(alliance.fc.values, alliance.fc.dates, format='%Y-%m-%d')
alliance.hist_fc.ts <- rbind(alliance.cad.ts, alliance.fc.ts[,1])
for (i in 2:length(alliance.fc.ts[1,])) {alliance.hist_fc.ts <- cbind(alliance.hist_fc.ts, rbind(alliance.cad.ts, alliance.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(alliance.hist_fc.ts,file=paste("Alliance (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


############################################################# Compare BC EMP WH versus AECO ############################################################
#### Logistic Distribution better describes residuals
aeco_bc.emp.wh.win <- window(ab.30d.spot.aeco.cad.ts,start=start(bc.emp.wh.cad.ts),end=end(bc.emp.wh.cad.ts))
aeco_bc.emp.wh.lm <- lm(as.numeric(bc.emp.wh.cad.ts[,1])~as.numeric(aeco_bc.emp.wh.win[,1]))
aeco_bc.emp.wh.lm.summary <- summary(aeco_bc.emp.wh.lm)
aeco_bc.emp.wh.lm.intercept <- aeco_bc.emp.wh.lm.summary$coefficients[1,1]
aeco_bc.emp.wh.lm.slope <- aeco_bc.emp.wh.lm.summary$coefficients[2,1]
aeco_bc.emp.wh.lm.res <- aeco_bc.emp.wh.lm.summary$residuals
dev.new()
plot(as.numeric(aeco_bc.emp.wh.win[,1]),as.numeric(bc.emp.wh.cad.ts[,1]),xlab="AECO ($CA/mcf)",ylab="BC EMP WH ($CA/bbl)",col="blue");grid()
title(main="BC EMP WH vs AECO")
abline(aeco_bc.emp.wh.lm,col="green")
## Logistic distribution
aeco_bc.emp.wh.lm.logis <- fitdist(aeco_bc.emp.wh.lm.res,"logis")
summary(aeco_bc.emp.wh.lm.logis)
dev.new()
plot(aeco_bc.emp.wh.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#aeco_bc.emp.wh.lm.norm <- fitdist(aeco_bc.emp.wh.lm.res,"norm")
#summary(aeco_bc.emp.wh.lm.norm)
#dev.new()
#plot(aeco_bc.emp.wh.lm.norm,breaks=30)
#title(main="Normal")

### BC EMP WH Price Forecast
bc.emp.wh.location <- as.numeric(summary(aeco_bc.emp.wh.lm.logis)[[1]][1])
bc.emp.wh.scale <- as.numeric(summary(aeco_bc.emp.wh.lm.logis)[[1]][2])
# Create distribution forecast
end.bc.emp.wh <- end(bc.emp.wh.cad.ts)
if (month(end.bc.emp.wh)==12) {bc.emp.wh.fc.start <- as.Date(paste(year(end.bc.emp.wh)+1,"-01-01",sep=""),format="%Y-%m-%d")
} else {bc.emp.wh.fc.start <- as.Date(paste(year(end.bc.emp.wh),"-",month(end.bc.emp.wh)+1,"-01",sep=""),format="%Y-%m-%d")}

aeco_bc.emp.wh.fc.win <- window(aeco.real.cad.hist.fc.ts,start=bc.emp.wh.fc.start,end=end(aeco.real.cad.hist.fc.ts)) # Forecast Window

forecast.dist.bc.emp.wh <- aeco_bc.emp.wh.fc.win
for (i in 1:length(aeco_bc.emp.wh.fc.win[,1])) {forecast.dist.bc.emp.wh[i,] <- rlogis(n, location=bc.emp.wh.location, scale=bc.emp.wh.scale)}
colnames(forecast.dist.bc.emp.wh) <- paste0("FC ",1:1000)

# Translate distribution FC into a time series
bc.emp.wh.fc.values <- matrix(nrow=length(aeco_bc.emp.wh.fc.win[,1]),ncol=length(aeco_bc.emp.wh.fc.win[1,]))
for (i in 1:length(aeco_bc.emp.wh.fc.win[1,])) {
	bc.emp.wh.fc.values[,i] <- aeco_bc.emp.wh.lm.slope * as.numeric(aeco_bc.emp.wh.fc.win[,i]) + aeco_bc.emp.wh.lm.intercept + as.numeric(forecast.dist.bc.emp.wh[,i])}
bc.emp.wh.fc.dates <- as.character(timeSequence(start(aeco_bc.emp.wh.fc.win), length.out=length(aeco_bc.emp.wh.fc.win[,1]),by="month"))
bc.emp.wh.fc.ts <- timeSeries(bc.emp.wh.fc.values, bc.emp.wh.fc.dates, format='%Y-%m-%d')
bc.emp.wh.hist_fc.ts <- rbind(bc.emp.wh.cad.ts, bc.emp.wh.fc.ts[,1])
for (i in 2:length(bc.emp.wh.fc.ts[1,])) {bc.emp.wh.hist_fc.ts <- cbind(bc.emp.wh.hist_fc.ts, rbind(bc.emp.wh.cad.ts, bc.emp.wh.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(bc.emp.wh.hist_fc.ts,file=paste("BC EMP Wellhead (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


############################################################## Compare BC West Coast Station2 versus AECO ###############################################
#### Logistic Distribution better describes residuals
aeco_bc.wc.st2.win <- window(ab.30d.spot.aeco.cad.ts,start=start(bc.wc.station2.cad.ts),end=end(bc.wc.station2.cad.ts))
aeco_bc.wc.st2.lm <- lm(as.numeric(bc.wc.station2.cad.ts[,1])~as.numeric(aeco_bc.wc.st2.win[,1]))
aeco_bc.wc.st2.lm.summary <- summary(aeco_bc.wc.st2.lm)
aeco_bc.wc.st2.lm.intercept <- aeco_bc.wc.st2.lm.summary$coefficients[1,1]
aeco_bc.wc.st2.lm.slope <- aeco_bc.wc.st2.lm.summary$coefficients[2,1]
aeco_bc.wc.st2.lm.res <- aeco_bc.wc.st2.lm.summary$residuals
dev.new()
plot(as.numeric(aeco_bc.wc.st2.win[,1]),as.numeric(bc.wc.station2.cad.ts[,1]),xlab="AECO ($CA/mcf)",ylab="BC West Coast Station2 ($CA/bbl)",col="blue");grid()
title(main="BC West Coast Station2 vs AECO")
abline(aeco_bc.wc.st2.lm,col="green")
## Logistic distribution
aeco_bc.wc.st2.lm.logis <- fitdist(aeco_bc.wc.st2.lm.res,"logis")
summary(aeco_bc.wc.st2.lm.logis)
dev.new()
plot(aeco_bc.wc.st2.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#aeco_bc.wc.st2.lm.norm <- fitdist(aeco_bc.wc.st2.lm.res,"norm")
#summary(aeco_bc.wc.st2.lm.norm)
#dev.new()
#plot(aeco_bc.wc.st2.lm.norm,breaks=30)
#title(main="Normal")

### BC West Coast Station 2 Price Forecast
bc.wc.st2.location <- as.numeric(summary(aeco_bc.wc.st2.lm.logis)[[1]][1])
bc.wc.st2.scale <- as.numeric(summary(aeco_bc.wc.st2.lm.logis)[[1]][2])
# Create distribution forecast
end.bc.wc.st2 <- end(bc.wc.station2.cad.ts)
if (month(end.bc.wc.st2)==12) {bc.wc.st2.fc.start <- as.Date(paste(year(end.bc.wc.st2)+1,"-01-01",sep=""),format="%Y-%m-%d")
} else {bc.wc.st2.fc.start <- as.Date(paste(year(end.bc.wc.st2),"-",month(end.bc.wc.st2)+1,"-01",sep=""),format="%Y-%m-%d")}

aeco_bc.wc.st2.fc.win <- window(aeco.real.cad.hist.fc.ts,start=bc.wc.st2.fc.start,end=end(aeco.real.cad.hist.fc.ts)) # Forecast Window

forecast.dist.bc.wc.st2 <- aeco_bc.wc.st2.fc.win
for (i in 1:length(aeco_bc.wc.st2.fc.win[,1])) {forecast.dist.bc.wc.st2[i,] <- rlogis(n, location=bc.wc.st2.location, scale=bc.wc.st2.scale)}
colnames(forecast.dist.bc.wc.st2) <- paste0("FC ",1:1000)

# Translate distribution FC into a time series
bc.wc.st2.fc.values <- matrix(nrow=length(aeco_bc.wc.st2.fc.win[,1]),ncol=length(aeco_bc.wc.st2.fc.win[1,]))
for (i in 1:length(aeco_bc.wc.st2.fc.win[1,])) {
	bc.wc.st2.fc.values[,i] <- aeco_bc.wc.st2.lm.slope * as.numeric(aeco_bc.wc.st2.fc.win[,i]) + aeco_bc.wc.st2.lm.intercept + as.numeric(forecast.dist.bc.wc.st2[,i])}
bc.wc.st2.fc.dates <- as.character(timeSequence(start(aeco_bc.wc.st2.fc.win), length.out=length(aeco_bc.wc.st2.fc.win[,1]),by="month"))
bc.wc.st2.fc.ts <- timeSeries(bc.wc.st2.fc.values, bc.wc.st2.fc.dates, format='%Y-%m-%d')
bc.wc.st2.hist_fc.ts <- rbind(bc.wc.station2.cad.ts, bc.wc.st2.fc.ts[,1])
for (i in 2:length(bc.wc.st2.fc.ts[1,])) {bc.wc.st2.hist_fc.ts <- cbind(bc.wc.st2.hist_fc.ts, rbind(bc.wc.station2.cad.ts, bc.wc.st2.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(bc.wc.st2.hist_fc.ts,file=paste("BC WC Station2 (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


############################################################# Compare BC 30d Spot versus AECO ##############################################################
#### Logistic Distribution better describes residuals
aeco_bc.30d.spot.win <- window(ab.30d.spot.aeco.cad.ts,start=start(bc.30d.spot.sumas.cad.ts),end=end(bc.30d.spot.sumas.cad.ts))
aeco_bc.30d.spot.lm <- lm(as.numeric(bc.30d.spot.sumas.cad.ts[,1])~as.numeric(aeco_bc.30d.spot.win[,1]))
aeco_bc.30d.spot.lm.summary <- summary(aeco_bc.30d.spot.lm)
aeco_bc.30d.spot.lm.intercept <- aeco_bc.30d.spot.lm.summary$coefficients[1,1]
aeco_bc.30d.spot.lm.slope <- aeco_bc.30d.spot.lm.summary$coefficients[2,1]
aeco_bc.30d.spot.lm.res <- aeco_bc.30d.spot.lm.summary$residuals
dev.new()
plot(as.numeric(aeco_bc.30d.spot.win[,1]),as.numeric(bc.30d.spot.sumas.cad.ts[,1]),xlab="AECO ($CA/mcf)",ylab="BC 30d Spot ($CA/bbl)",col="blue");grid()
title(main="BC 30d Spot vs AECO")
abline(aeco_bc.30d.spot.lm,col="green")
## Logistic distribution
aeco_bc.30d.spot.lm.logis <- fitdist(aeco_bc.30d.spot.lm.res,"logis")
summary(aeco_bc.30d.spot.lm.logis)
dev.new()
plot(aeco_bc.30d.spot.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#aeco_bc.30d.spot.lm.norm <- fitdist(aeco_bc.30d.spot.lm.res,"norm")
#summary(aeco_bc.30d.spot.lm.norm)
#dev.new()
#plot(aeco_bc.30d.spot.lm.norm,breaks=30)
#title(main="Normal")

### BC 30 Day Spot Price Forecast
bc.30d.spot.location <- as.numeric(summary(aeco_bc.30d.spot.lm.logis)[[1]][1])
bc.30d.spot.scale <- as.numeric(summary(aeco_bc.30d.spot.lm.logis)[[1]][2])
# Create distribution forecast
end.bc.30d.spot <- end(bc.30d.spot.sumas.cad.ts)
if (month(end.bc.30d.spot)==12) {bc.30d.spot.fc.start <- as.Date(paste(year(end.bc.30d.spot)+1,"-01-01",sep=""),format="%Y-%m-%d")
} else {bc.30d.spot.fc.start <- as.Date(paste(year(end.bc.30d.spot),"-",month(end.bc.30d.spot)+1,"-01",sep=""),format="%Y-%m-%d")}

aeco_bc.30d.spot.fc.win <- window(aeco.real.cad.hist.fc.ts,start=bc.30d.spot.fc.start,end=end(aeco.real.cad.hist.fc.ts)) # Forecast Window

forecast.dist.bc.30d.spot <- aeco_bc.30d.spot.fc.win
for (i in 1:length(aeco_bc.30d.spot.fc.win[,1])) {forecast.dist.bc.30d.spot[i,] <- rlogis(n, location=bc.30d.spot.location, scale=bc.30d.spot.scale)}
colnames(forecast.dist.bc.30d.spot) <- paste0("FC ",1:1000)

# Translate distribution FC into a time series
bc.30d.spot.fc.values <- matrix(nrow=length(aeco_bc.30d.spot.fc.win[,1]),ncol=length(aeco_bc.30d.spot.fc.win[1,]))
for (i in 1:length(aeco_bc.30d.spot.fc.win[1,])) {
	bc.30d.spot.fc.values[,i] <- aeco_bc.30d.spot.lm.slope * as.numeric(aeco_bc.30d.spot.fc.win[,i]) + aeco_bc.30d.spot.lm.intercept + as.numeric(forecast.dist.bc.30d.spot[,i])}
bc.30d.spot.fc.dates <- as.character(timeSequence(start(aeco_bc.30d.spot.fc.win), length.out=length(aeco_bc.30d.spot.fc.win[,1]),by="month"))
bc.30d.spot.fc.ts <- timeSeries(bc.30d.spot.fc.values, bc.30d.spot.fc.dates, format='%Y-%m-%d')
bc.30d.spot.hist_fc.ts <- rbind(bc.30d.spot.sumas.cad.ts, bc.30d.spot.fc.ts[,1])
for (i in 2:length(bc.30d.spot.fc.ts[1,])) {bc.30d.spot.hist_fc.ts <- cbind(bc.30d.spot.hist_fc.ts, rbind(bc.30d.spot.sumas.cad.ts, bc.30d.spot.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(bc.30d.spot.hist_fc.ts,file=paste("BC 30 Day Spot (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


############################################################# Compare Dawn versus AECO #########################################################################
#### Logistic Distribution better describes residuals
aeco_dawn.win <- window(ab.30d.spot.aeco.cad.ts,start=start(dawn.cad.ts),end=end(dawn.cad.ts))
aeco_dawn.lm <- lm(as.numeric(dawn.cad.ts[,1])~as.numeric(aeco_dawn.win[,1]))
aeco_dawn.lm.summary <- summary(aeco_dawn.lm)
aeco_dawn.lm.intercept <- aeco_dawn.lm.summary$coefficients[1,1]
aeco_dawn.lm.slope <- aeco_dawn.lm.summary$coefficients[2,1]
aeco_dawn.lm.res <- aeco_dawn.lm.summary$residuals
dev.new()
plot(as.numeric(aeco_dawn.win[,1]),as.numeric(dawn.cad.ts[,1]),xlab="AECO ($CA/mcf)",ylab="Dawn ($CA/bbl)",col="blue");grid()
title(main="Dawn vs AECO")
abline(aeco_dawn.lm,col="green")
## Logistic distribution
aeco_dawn.lm.logis <- fitdist(aeco_dawn.lm.res,"logis")
summary(aeco_dawn.lm.logis)
dev.new()
plot(aeco_dawn.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#aeco_dawn.lm.norm <- fitdist(aeco_dawn.lm.res,"norm")
#summary(aeco_dawn.lm.norm)
#dev.new()
#plot(aeco_dawn.lm.norm,breaks=30)
#title(main="Normal")

### Dawn Price Forecast
dawn.location <- as.numeric(summary(aeco_dawn.lm.logis)[[1]][1])
dawn.scale <- as.numeric(summary(aeco_dawn.lm.logis)[[1]][2])
# Create distribution forecast
end.dawn <- end(dawn.cad.ts)
if (month(end.dawn)==12) {dawn.fc.start <- as.Date(paste(year(end.dawn)+1,"-01-01",sep=""),format="%Y-%m-%d")
} else {dawn.fc.start <- as.Date(paste(year(end.dawn),"-",month(end.dawn)+1,"-01",sep=""),format="%Y-%m-%d")}

aeco_dawn.fc.win <- window(aeco.real.cad.hist.fc.ts,start=dawn.fc.start,end=end(aeco.real.cad.hist.fc.ts)) # Forecast Window

forecast.dist.dawn <- aeco_dawn.fc.win
for (i in 1:length(aeco_dawn.fc.win[,1])) {forecast.dist.dawn[i,] <- rlogis(n, location=dawn.location, scale=dawn.scale)}
colnames(forecast.dist.dawn) <- paste0("FC ",1:1000)

# Translate distribution FC into a time series
dawn.fc.values <- matrix(nrow=length(aeco_dawn.fc.win[,1]),ncol=length(aeco_dawn.fc.win[1,]))
for (i in 1:length(aeco_dawn.fc.win[1,])) {
	dawn.fc.values[,i] <- aeco_dawn.lm.slope * as.numeric(aeco_dawn.fc.win[,i]) + aeco_dawn.lm.intercept + as.numeric(forecast.dist.dawn[,i])}
dawn.fc.dates <- as.character(timeSequence(start(aeco_dawn.fc.win), length.out=length(aeco_dawn.fc.win[,1]),by="month"))
dawn.fc.ts <- timeSeries(dawn.fc.values, dawn.fc.dates, format='%Y-%m-%d')
dawn.hist_fc.ts <- rbind(dawn.cad.ts, dawn.fc.ts[,1])
for (i in 2:length(dawn.fc.ts[1,])) {dawn.hist_fc.ts <- cbind(dawn.hist_fc.ts, rbind(dawn.cad.ts, dawn.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(dawn.hist_fc.ts,file=paste("Dawn (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))

################# PDF Reports
color.palate <- c("blue","green","orange","pink","red","purple","thistle","turquoise","grey","brown")
setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/PDF R-Reports/50 Years")
################################################# HH in Real USD
pdf("HH Volatility in Real USD.pdf")
plot(hh.real.mh.fc.ts[,1],xlim=c(start(hh.real.mh.fc.ts[,1]),end(hh.real.mh.fc.ts[,1])),ylim=c(0,50))
title(paste("HH in Real USD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(hh.real.mh.fc.ts[1,])-1) {lines(hh.real.mh.fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(hh.real.mh.fc.ts[,i],col="black")
dev.off()

################################################# HH in Real CAD
pdf("HH Volatility in Real CAD.pdf")
plot(hh.real.cad.mh.fc.ts[,1],xlim=c(start(hh.real.cad.mh.fc.ts[,1]),end(hh.real.cad.mh.fc.ts[,1])),ylim=c(0,50))
title(paste("HH in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(hh.real.cad.mh.fc.ts[1,])-1) {lines(hh.real.cad.mh.fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(hh.real.cad.mh.fc.ts[,i],col="black")
dev.off()

################################################# Canadian Gas Volatility in Real CAD
pdf("Canadian Gas Volatility (Real CAD).pdf")

####################### Correlation plot btw AECO & HH
plot(hh.real.cad_aeco.win,ab.30d.spot.aeco.cad.ts,col="green", main="AECO v. Henry Hub (CAD$/mcf)",panel.first=abline(h=seq(0,15,by=1),v=seq(0,15,by=1),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,15),ylim=c(0,15),xlab="Henry Hub (CAD$/mcf)",ylab="AECO (CAD$/mcf)")
axis(1,at=seq(0,15,by=1),las=1)
axis(2,at=seq(0,15,by=1),las=2)
abline(hh.aeco.lm,col="red")

# AECO Distribution fit
plot(hh.aeco.lm.logis, breaks=50);title("AECO-Henry Hub Residuals (Logistic Distribution)",line=3,col.main="blue")

# AECO (Real CAD) Forecasts
plot(aeco.real.cad.hist.fc.ts[,1],xlim=c(start(aeco.real.cad.hist.fc.ts[,1]),end(aeco.real.cad.hist.fc.ts[,1])),ylim=c(0,50),xlab="",ylab="AECO (CAD$/mcf)",axes=FALSE)
axis.POSIXct(1,at=seq(start(aeco.real.cad.hist.fc.ts[,1]),end(aeco.real.cad.hist.fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,50,5), las=2)
title(paste("AECO 30 Day Spot in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(aeco.real.cad.hist.fc.ts[1,])-1) {lines(aeco.real.cad.hist.fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(aeco.real.cad.hist.fc.ts[,i],col="black")

####################### Correlation plot btw Ethane & AECO
# Ethane (Real CAD) Forecasts
plot(ethane.cad.hist_fc.ts[,1],xlim=c(start(ethane.cad.hist_fc.ts[,1]),end(ethane.cad.hist_fc.ts[,1])),ylim=c(0,150),xlab="",ylab="Ethane (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(ethane.cad.hist_fc.ts[,1]),end(ethane.cad.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,150,10), las=2)
title(paste("Ethane in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(ethane.cad.hist_fc.ts[1,])-1) {lines(ethane.cad.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(ethane.cad.hist_fc.ts[,i],col="black")

####################### Correlation plot btw AB Reference Price & AECO
plot(as.numeric(aeco_ab.ref.win[,1]),as.numeric(ab.gov.ref.cad.ts[,1]),col="green", main="AB Reference v. AECO (CAD$/mcf)",panel.first=abline(h=seq(0,15,by=1),v=seq(0,15,by=1),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,15),ylim=c(0,15),xlab="AECO (CAD$/mcf)",ylab="AB Reference Price (CAD$/mcf)")
axis(1,at=seq(0,15,by=1),las=1)
axis(2,at=seq(0,15,by=1),las=2)
abline(aeco_ab.ref.lm,col="red")

# AB Reference Price Distribution fit
plot(aeco_ab.ref.lm.logis, breaks=50);title("AB Reference-AECO Residuals (Logistic Distribution)",line=3,col.main="blue")

# AB Reference Price (Real CAD) Forecasts
plot(ab.ref.hist_fc.ts[,1],xlim=c(start(ab.ref.hist_fc.ts[,1]),end(ab.ref.hist_fc.ts[,1])),ylim=c(0,50),xlab="",ylab="AB Reference Price (CAD$/mcf)",axes=FALSE)
axis.POSIXct(1,at=seq(start(ab.ref.hist_fc.ts[,1]),end(ab.ref.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,50,5), las=2)
title(paste("AB Reference Price in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(ab.ref.hist_fc.ts[1,])-1) {lines(ab.ref.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(ab.ref.hist_fc.ts[,i],col="black")

####################### Correlation plot btw Alliance & AECO
plot(as.numeric(aeco_alliance.win[,1]),as.numeric(alliance.cad.ts[,1]),col="green", main="Alliance v. AECO (CAD$/mcf)",panel.first=abline(h=seq(0,15,by=1),v=seq(0,15,by=1),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,15),ylim=c(0,15),xlab="AECO (CAD$/mcf)",ylab="Alliance (CAD$/mcf)")
axis(1,at=seq(0,15,by=1),las=1)
axis(2,at=seq(0,15,by=1),las=2)
abline(aeco_alliance.lm,col="red")

# Alliance Distribution fit
plot(aeco_alliance.lm.logis, breaks=50);title("Alliance-AECO Residuals (Logistic Distribution)",line=3,col.main="blue")

# Alliance (Real CAD) Forecasts
plot(alliance.hist_fc.ts[,1],xlim=c(start(alliance.hist_fc.ts[,1]),end(alliance.hist_fc.ts[,1])),ylim=c(0,50),xlab="",ylab="Alliance (CAD$/mcf)",axes=FALSE)
axis.POSIXct(1,at=seq(start(alliance.hist_fc.ts[,1]),end(alliance.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,50,5), las=2)
title(paste("Alliance in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(alliance.hist_fc.ts[1,])-1) {lines(alliance.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(alliance.hist_fc.ts[,i],col="black")

####################### Correlation plot btw BC EMP Wellhead & AECO
plot(as.numeric(aeco_bc.emp.wh.win[,1]),as.numeric(bc.emp.wh.cad.ts[,1]),col="green", main="BC EMP Wellhead v. AECO (CAD$/mcf)",panel.first=abline(h=seq(0,15,by=1),v=seq(0,15,by=1),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,15),ylim=c(0,15),xlab="AECO (CAD$/mcf)",ylab="BC EMP Wellhead (CAD$/mcf)")
axis(1,at=seq(0,15,by=1),las=1)
axis(2,at=seq(0,15,by=1),las=2)
abline(aeco_bc.emp.wh.lm,col="red")

# BC EMP Wellhead Distribution fit
plot(aeco_bc.emp.wh.lm.logis, breaks=50);title("BC EMP Wellhead-AECO Residuals (Logistic Distribution)",line=3,col.main="blue")

# BC EMP Wellhead (Real CAD) Forecasts
plot(bc.emp.wh.hist_fc.ts[,1],xlim=c(start(bc.emp.wh.hist_fc.ts[,1]),end(bc.emp.wh.hist_fc.ts[,1])),ylim=c(0,50),xlab="",ylab="BC EMP Wellhead (CAD$/mcf)",axes=FALSE)
axis.POSIXct(1,at=seq(start(bc.emp.wh.hist_fc.ts[,1]),end(bc.emp.wh.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,50,5), las=2)
title(paste("BC EMP Wellhead in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(bc.emp.wh.hist_fc.ts[1,])-1) {lines(bc.emp.wh.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(bc.emp.wh.hist_fc.ts[,i],col="black")

####################### Correlation plot btw BC West Coast Station2 & AECO
plot(as.numeric(aeco_bc.wc.st2.win[,1]),as.numeric(bc.wc.station2.cad.ts[,1]),col="green", main="BC WC Station2 v. AECO (CAD$/mcf)",panel.first=abline(h=seq(0,15,by=1),v=seq(0,15,by=1),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,15),ylim=c(0,15),xlab="AECO (CAD$/mcf)",ylab="BC WC Station2 (CAD$/mcf)")
axis(1,at=seq(0,15,by=1),las=1)
axis(2,at=seq(0,15,by=1),las=2)
abline(aeco_bc.wc.st2.lm,col="red")

# BC West Coast Station2 Distribution fit
plot(aeco_bc.wc.st2.lm.logis, breaks=50);title("BC WC Station2-AECO Residuals (Logistic Distribution)",line=3,col.main="blue")

# BC West Coast Station2 (Real CAD) Forecasts
plot(bc.wc.st2.hist_fc.ts[,1],xlim=c(start(bc.wc.st2.hist_fc.ts[,1]),end(bc.wc.st2.hist_fc.ts[,1])),ylim=c(0,50),xlab="",ylab="BC WC Station2 (CAD$/mcf)",axes=FALSE)
axis.POSIXct(1,at=seq(start(bc.wc.st2.hist_fc.ts[,1]),end(bc.wc.st2.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,50,5), las=2)
title(paste("BC WC Station2 in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(bc.wc.st2.hist_fc.ts[1,])-1) {lines(bc.wc.st2.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(bc.wc.st2.hist_fc.ts[,i],col="black")

####################### Correlation plot btw BC 30Day Spot & AECO
plot(as.numeric(aeco_bc.30d.spot.win[,1]),as.numeric(bc.30d.spot.sumas.cad.ts[,1]),col="green", main="BC 30 Day Spot v. AECO (CAD$/mcf)",panel.first=abline(h=seq(0,15,by=1),v=seq(0,15,by=1),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,15),ylim=c(0,15),xlab="AECO (CAD$/mcf)",ylab="BC 30 Day Spot (CAD$/mcf)")
axis(1,at=seq(0,15,by=1),las=1)
axis(2,at=seq(0,15,by=1),las=2)
abline(aeco_bc.30d.spot.lm,col="red")

# BC 30Day Spot Distribution fit
plot(aeco_bc.30d.spot.lm.logis, breaks=50);title("BC 30 Day Spot-AECO Residuals (Logistic Distribution)",line=3,col.main="blue")

# BC 30Day Spot (Real CAD) Forecasts
plot(bc.30d.spot.hist_fc.ts[,1],xlim=c(start(bc.30d.spot.hist_fc.ts[,1]),end(bc.30d.spot.hist_fc.ts[,1])),ylim=c(0,50),xlab="",ylab="BC 30 Day Spot (CAD$/mcf)",axes=FALSE)
axis.POSIXct(1,at=seq(start(bc.30d.spot.hist_fc.ts[,1]),end(bc.30d.spot.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,50,5), las=2)
title(paste("BC 30 Day Spot in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(bc.30d.spot.hist_fc.ts[1,])-1) {lines(bc.30d.spot.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(bc.30d.spot.hist_fc.ts[,i],col="black")

####################### Correlation plot btw Dawn & AECO
plot(as.numeric(aeco_dawn.win[,1]),as.numeric(dawn.cad.ts[,1]),col="green", main="Dawn v. AECO (CAD$/mcf)",panel.first=abline(h=seq(0,15,by=1),v=seq(0,15,by=1),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,15),ylim=c(0,15),xlab="AECO (CAD$/mcf)",ylab="Dawn (CAD$/mcf)")
axis(1,at=seq(0,15,by=1),las=1)
axis(2,at=seq(0,15,by=1),las=2)
abline(aeco_dawn.lm,col="red")

# Dawn Distribution fit
plot(aeco_dawn.lm.logis, breaks=50);title("Dawn-AECO Residuals (Logistic Distribution)",line=3,col.main="blue")

# Dawn (Real CAD) Forecasts
plot(dawn.hist_fc.ts[,1],xlim=c(start(dawn.hist_fc.ts[,1]),end(dawn.hist_fc.ts[,1])),ylim=c(0,50),xlab="",ylab="Dawn (CAD$/mcf)",axes=FALSE)
axis.POSIXct(1,at=seq(start(dawn.hist_fc.ts[,1]),end(dawn.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,50,5), las=2)
title(paste("Dawn in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(dawn.hist_fc.ts[1,])-1) {lines(dawn.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(dawn.hist_fc.ts[,i],col="black")

dev.off()
