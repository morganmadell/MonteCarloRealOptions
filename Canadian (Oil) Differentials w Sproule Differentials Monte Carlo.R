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

### File Names of .CSV Canadian Oil Postings
file.name.m <- "Monthly Historical Edmonton Light and Hardisty Heavy Crude Prices.csv"
file.name.d <- "Daily Historical Edmonton Light and Hardisty Heavy Crude Prices.csv"

### File Path to folder that contains the above files
path <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Price Differentials"
setwd(path)

### Import Monthly and Daily Data
oil.hist.m <- read.csv(file.name.m)
oil.hist.d <- read.csv(file.name.d)

### Re-organize data into usable R data structures
dates.m <- as.data.frame(oil.hist.m$X.3)
dates.m <- as.Date(dates.m[-1,],"%Y-%m-%d")
dates.d <- as.data.frame(oil.hist.d$X.1)
dates.d <- as.Date(dates.d[-1,],"%Y-%m-%d")
dates.d <- dates.d[-c(12787:12909)]#### Need to remove rows
edm.l.m <- oil.hist.m$"Cdn.Par...Edmonton"
hard.h.m <- oil.hist.m$"Cdn.Heavy...Hardisty"
wti.m <- oil.hist.m$"NYMEX.WTI...Chicago"
edm.l.d <- oil.hist.d$"Cdn.Par...Edmonton"
hard.h.d <- oil.hist.d$"Cdn.Heavy...Hardisty"
wti.d <- oil.hist.d$"NYMEX.WTI...Chicago"
# Remove 1st row
edm.l.m <- as.data.frame(edm.l.m[-1])
hard.h.m <- as.data.frame(hard.h.m[-1]) 
wti.m <- as.data.frame(wti.m[-1])
edm.l.d <- as.data.frame(edm.l.d[-1])
edm.l.d <- edm.l.d[-c(12787:12909),]#### Need to remove rows
hard.h.d <- as.data.frame(hard.h.d[-1])
hard.h.d <- hard.h.d[-c(12787:12909),]#### Need to remove rows
wti.d <- as.data.frame(wti.d[-1])
wti.d <- wti.d[-c(12787:12909),]#### Need to remove rows
# Combine into timeSeries
edm.l.m.df <- cbind(dates.m,edm.l.m); colnames(edm.l.m.df) <- c("Date","Price")
hard.h.m.df <- cbind(dates.m,hard.h.m); colnames(hard.h.m.df) <- c("Date","Price")
wti.m.df <- cbind(dates.m,wti.m); colnames(wti.m.df) <- c("Date","Price")
edm.l.d.df <- as.data.frame(matrix(nrow=length(dates.d),ncol=2))
edm.l.d.df[,1] <- dates.d; edm.l.d.df[,2] <- edm.l.d; colnames(edm.l.d.df) <- c("Date","Price")
edm.l.d.nona <- subset(edm.l.d.df, Price!="#N/A")
hard.h.d.df <- as.data.frame(matrix(nrow=length(dates.d),ncol=2))
hard.h.d.df[,1] <- dates.d; hard.h.d.df[,2] <- hard.h.d; colnames(hard.h.d.df) <- c("Date","Price")
hard.h.d.nona <- subset(hard.h.d.df, Price!="#N/A")
wti.d.df <- as.data.frame(matrix(nrow=length(dates.d),ncol=2))
wti.d.df[,1] <- dates.d; wti.d.df[,2] <- wti.d; colnames(wti.d.df) <- c("Date","Price")
wti.d.d.nona <- subset(wti.d.df, Price!="#N/A")
edm.l.m.ts <- as.timeSeries(edm.l.m.df$Price,edm.l.m.df$Date); colnames(edm.l.m.ts)<-c("Price")
hard.h.m.ts <- as.timeSeries(hard.h.m.df$Price,hard.h.m.df$Date); colnames(hard.h.m.ts)<-c("Price")
wti.m.ts <- as.timeSeries(wti.m.df$Price,wti.m.df$Date); colnames(wti.m.ts)<-c("Price")
edm.l.d.ts <- as.timeSeries(edm.l.d.nona$Price,edm.l.d.nona$Date); colnames(edm.l.d.ts)<-c("Price")
hard.h.d.ts <- as.timeSeries(hard.h.d.nona$Price,hard.h.d.nona$Date); colnames(hard.h.d.ts)<-c("Price")
wti.d.ts <- as.timeSeries(wti.d.d.nona$Price,wti.d.d.nona$Date); colnames(wti.d.ts)<-c("Price")

### Combine EdmL, HardH with WTI
edm.wti.d <- cbind(edm.l.d.ts, wti.d.ts); colnames(edm.wti.d)<-c("EdmL","WTI")
hard.wti.d <- cbind(hard.h.d.ts, wti.d.ts); colnames(hard.wti.d)<-c("HardH","WTI")
edm.wti.m <- cbind(edm.l.m.ts, wti.m.ts); colnames(edm.wti.m)<-c("EdmL","WTI")
hard.wti.m <- cbind(hard.h.m.ts, wti.m.ts); colnames(hard.wti.m)<-c("HardH","WTI")


#dev.new()
#par(mfrow=c(1,2))
#plot(edm.wti.d$WTI,edm.wti.d$EdmL, main="Edmonton Light v. WTI (CAD)",xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="EdmL (CAD)")
#plot(hard.wti.d$WTI,hard.wti.d$HardH, main="Hardisty Heavy v. WTI (CAD)",xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="HardH (CAD)")

### Only after July 1998 was Edmonton Light and Hardisty Heavy traded daily (according to data source)
#start.date <- "1998-07-01"
start.date <- "2004-01-01"
edm.wti.d.dt <- removeNA(window(edm.wti.d,start="1998-07-01",end=end(edm.wti.d)))
hard.wti.d.dt <- removeNA(window(hard.wti.d,start="1998-07-01",end=end(hard.wti.d)))
edm.wti.m.dt <- removeNA(window(edm.wti.m,start="1991-01-01",end=end(edm.wti.m)))
edm.na.loc <- grep("#N/A",edm.wti.m.dt[,1],fixed=TRUE)
edm.wti.m.dt <- edm.wti.m.dt[-edm.na.loc,]
hard.wti.m.dt <- removeNA(window(hard.wti.m,start="1991-01-01",end=end(hard.wti.m)))
hard.na.loc <- grep("#N/A",hard.wti.m.dt[,1],fixed=TRUE)
hard.wti.m.dt <- hard.wti.m.dt[-hard.na.loc,]

### Linear model between EdmL, HardH and WTI (Daily)
lm.EdmL <- lm(as.numeric(edm.wti.d.dt$EdmL)~as.numeric(edm.wti.d.dt$WTI))
summary(lm.EdmL)
lm.HardH <- lm(as.numeric(hard.wti.d.dt$HardH)~as.numeric(hard.wti.d.dt$WTI))
summary(lm.HardH)
dev.new()
plot(edm.wti.d.dt$WTI,edm.wti.d.dt$EdmL,col="green", main="Edmonton Light v. WTI (CAD)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="EdmL (CAD)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(lm.EdmL,col="red")
dev.new()
plot(hard.wti.d.dt$WTI,hard.wti.d.dt$HardH,col="green", main="Hardisty Heavy v. WTI (CAD)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="HardH (CAD)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(lm.HardH,col="red")

### Linear model between EdmL, HardH and WTI (Monthly)
lm.EdmL.m <- lm(as.numeric(edm.wti.m.dt$EdmL)~as.numeric(edm.wti.m.dt$WTI))
summary(lm.EdmL.m)
lm.HardH.m <- lm(as.numeric(hard.wti.m.dt$HardH)~as.numeric(hard.wti.m.dt$WTI))
summary(lm.HardH.m)
dev.new()
par(mfrow=c(1,2))
plot(edm.wti.m.dt$WTI,edm.wti.m.dt$EdmL,col="green", main="Edmonton Light v. WTI (CAD)",xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="EdmL (CAD)")
abline(lm.EdmL.m,col="red")
plot(hard.wti.m.dt$WTI,hard.wti.m.dt$HardH,col="green", main="Hardisty Heavy v. WTI (CAD)",xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="HardH (CAD)")
abline(lm.HardH.m,col="red")

### Extract Linear Model Parameters
# Daily
lm.edml.wti.intercept <- summary(lm.EdmL)$coefficients[1,1]
lm.edml.wti.slope <- summary(lm.EdmL)$coefficients[2,1]
lm.edml.wti.intercept.stderr <- summary(lm.EdmL)$coefficients[1,2]
lm.edml.wti.slope.stderr <- summary(lm.EdmL)$coefficients[2,2]
lm.hardh.wti.intercept <- summary(lm.HardH)$coefficients[1,1]
lm.hardh.wti.slope <- summary(lm.HardH)$coefficients[2,1]
lm.hardh.wti.intercept.stderr <- summary(lm.HardH)$coefficients[1,2]
lm.hardh.wti.slope.stderr <- summary(lm.HardH)$coefficients[2,2]
# Monthly
lm.edml.wti.intercept.m <- summary(lm.EdmL.m)$coefficients[1,1]
lm.edml.wti.slope.m <- summary(lm.EdmL.m)$coefficients[2,1]
lm.edml.wti.intercept.stderr.m <- summary(lm.EdmL.m)$coefficients[1,2]
lm.edml.wti.slope.stderr.m <- summary(lm.EdmL.m)$coefficients[2,2]
lm.hardh.wti.intercept.m <- summary(lm.HardH.m)$coefficients[1,1]
lm.hardh.wti.slope.m <- summary(lm.HardH.m)$coefficients[2,1]
lm.hardh.wti.intercept.stderr.m <- summary(lm.HardH.m)$coefficients[1,2]
lm.hardh.wti.slope.stderr.m <- summary(lm.HardH.m)$coefficients[2,2]

### Fit Residual Values with Distributions
#dev.new(); descdist(summary(lm.EdmL)$residuals);title(main="Edmonton Light",line=2.7,col.main="blue")
#lm.edml.wti.resid.norm <- fitdist(summary(lm.EdmL)$residuals,"norm")
#summary(lm.edml.wti.resid.norm)
#dev.new(); plot(lm.edml.wti.resid.norm, breaks=50);title("Edmonton Light (Normal Distribution)",line=3,col.main="blue")
lm.edml.wti.resid.logis <- fitdist(summary(lm.EdmL)$residuals,"logis")
summary(lm.edml.wti.resid.logis)
dev.new(); plot(lm.edml.wti.resid.logis, breaks=50);title("Edmonton Light (Logistic Distribution)",line=3,col.main="blue")
#lm.edml.wti.resid.cauchy <- fitdist(summary(lm.EdmL)$residuals,"cauchy")
#summary(lm.edml.wti.resid.cauchy)
#dev.new(); plot(lm.edml.wti.resid.cauchy, breaks=50);title("Edmonton Light (Cauchy Distribution)",line=3,col.main="blue")
#dev.new();descdist(summary(lm.HardH)$residuals);title(main="Hardisty Heavy",line=2.7,col.main="blue")
#lm.hardh.wti.resid.norm <- fitdist(summary(lm.HardH)$residuals,"norm")
#summary(lm.hardh.wti.resid.norm)
#dev.new(); plot(lm.hardh.wti.resid.norm, breaks=50);title("Hardisty Heavy (Normal Distribution)",line=3,col.main="blue")
lm.hardh.wti.resid.logis <- fitdist(summary(lm.HardH)$residuals,"logis")
summary(lm.hardh.wti.resid.logis)
dev.new(); plot(lm.hardh.wti.resid.logis, breaks=50);title("Hardisty Heavy (Logistic Distribution)",line=3,col.main="blue")
#lm.hardh.wti.resid.cauchy <- fitdist(summary(lm.HardH)$residuals,"cauchy")
#summary(lm.hardh.wti.resid.cauchy)
#dev.new(); plot(lm.hardh.wti.resid.cauchy, breaks=50);title("Hardisty Heavy (Cauchy Distribution)",line=3,col.main="blue")

#Monthly
lm.edml.wti.resid.logis.m <- fitdist(summary(lm.EdmL.m)$residuals,"logis")
summary(lm.edml.wti.resid.logis.m)
dev.new(); plot(lm.edml.wti.resid.logis.m, breaks=50);title("Edmonton Light (Logistic Distribution)",line=3,col.main="blue")
lm.hardh.wti.resid.logis.m <- fitdist(summary(lm.HardH.m)$residuals,"logis")
summary(lm.hardh.wti.resid.logis.m)
dev.new(); plot(lm.hardh.wti.resid.logis.m, breaks=50);title("Hardisty Heavy (Logistic Distribution)",line=3,col.main="blue")

### Estimate Canadian Prices
### Use the Logistic Distribution for Edmonton Light
# Create the expected value
edml.exp <- wti.d.ts
edml.exp <- cbind(edml.exp,edml.exp)
colnames(edml.exp) <- c("WTI","EdmL LFit")
edml.exp[,2] <- lm.edml.wti.slope.m*as.numeric(edml.exp[,1])+lm.edml.wti.intercept.m
dev.new()
plot(edml.exp[,1],edml.exp[,2], main="Edmonton Light v. WTI (CAD)",xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="EdmL (CAD)")
abline(lm.EdmL,col="red")
### Use the Logistc Distribution for Hardisty Heavy
# Create the expected value
hardh.exp <- wti.d.ts
hardh.exp <- cbind(hardh.exp,hardh.exp)
colnames(hardh.exp) <- c("WTI","HardH LFit")
hardh.exp[,2] <- lm.hardh.wti.slope.m*as.numeric(hardh.exp[,1])+lm.hardh.wti.intercept.m
dev.new()
plot(hardh.exp[,1],hardh.exp[,2], main="Hardisty Heavy v. WTI (CAD)",xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="HardH (CAD)")
abline(lm.HardH,col="red")

# Create distribution
n.series <- 10 ############# Number of Forecast Series
fc.df <- matrix(nrow=length(edml.exp[,1]),ncol=1,0)
fc.names <- vector(length=n.series)
for (i in 1:n.series) {fc.names[i]<- paste("FC",i,sep="")}

# Edmonton Light - Use Logistic Distribution
edml.fc.dist <- cbind(edml.exp,fc.df)
for (i in 2:n.series) {edml.fc.dist <- cbind(edml.fc.dist,fc.df)}
colnames(edml.fc.dist) <- unique(c(c("WTI","EdmL LFit"),fc.names))
edml.dist.location <- as.numeric(summary(lm.edml.wti.resid.logis)[[1]][1])
edml.dist.scale <- as.numeric(summary(lm.edml.wti.resid.logis)[[1]][2])

dist.scale.edm <- 1 #### Adjust to match distribution
#i<-1
for (i in 1:length(edml.fc.dist[,1])) {
edml.dist.temp <- rlogis(n.series,location=edml.dist.location,scale=edml.dist.scale/dist.scale.edm)+as.numeric(edml.fc.dist[i,2])
	for (j in 1:n.series) {
		edml.fc.dist[i,j+2] <- edml.dist.temp[j]
	}
}

# Test Edmonton Light Model
lm.edm.fc.list <- vector("list",length=n.series)
for (i in 1:n.series) {
	lm.edm.fc.list[[i]] <- lm(as.numeric(edml.fc.dist[,i+2])~as.numeric(edml.fc.dist[,1]))
}
dev.new()
par(mfrow=c(3,3))
plot(edm.wti.d.dt$WTI,edm.wti.d.dt$EdmL,col="green", main="Edmonton Light v. WTI (CAD)",xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="EdmL (CAD)")
abline(lm.EdmL,col="red")
for (i in 3:n.series) {
	plot(as.numeric(edml.fc.dist[,1]),as.numeric(edml.fc.dist[,i]),xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="EdmL (CAD)")
	abline(lm.EdmL,col="red")
	abline(lm.edm.fc.list[[i]],col="blue")}
# Create loop for distribution fits
lm.edm.fc.dist.list <- vector("list",length=n.series)
for (i in 1:n.series) {
	lm.edm.fc.dist.list[[i]] <- fitdist(summary(lm.edm.fc.list[[i]])$residuals,"logis")
}

summary(lm.edml.wti.resid.logis)
dev.new();par(mfrow=c(3,3))
plot(lm.edml.wti.resid.logis, breaks=50);title("Edmonton Light (Logistic Distribution)",line=3,col.main="blue")
for (i in 3:n.series) {
	plot(lm.edm.fc.dist.list[[i]], breaks=50)
	print(summary(lm.edm.fc.dist.list[[i]]))}
		

# Use the Logistic Distribution for Hardisty Heavy
hardh.fc.dist <- cbind(hardh.exp,fc.df)
for (i in 2:n.series) {hardh.fc.dist <- cbind(hardh.fc.dist,fc.df)}
colnames(hardh.fc.dist) <- unique(c(c("WTI","HardH LFit"),fc.names))
hardh.dist.location <- as.numeric(summary(lm.hardh.wti.resid.logis)[[1]][1])
hardh.dist.scale <- as.numeric(summary(lm.hardh.wti.resid.logis)[[1]][2])

dist.scale.hard <- 1 #### Adjust to match distribution
#i<-1
for (i in 1:length(hardh.fc.dist[,1])) {
hardh.dist.temp <- rlogis(n.series,location=hardh.dist.location,scale=hardh.dist.scale/dist.scale.hard)+as.numeric(hardh.fc.dist[i,2])
	for (j in 1:n.series) {
		hardh.fc.dist[i,j+2] <- hardh.dist.temp[j]
	}
}

# Test Hardisty Heavy Model
lm.hard.fc.list <- vector("list",length=n.series)
for (i in 1:n.series) {
	lm.hard.fc.list[[i]] <- lm(as.numeric(hardh.fc.dist[,i+2])~as.numeric(hardh.fc.dist[,1]))}
dev.new()
par(mfrow=c(3,3))
plot(hard.wti.d.dt$WTI,hard.wti.d.dt$HardH,col="green", main="Hardisty Heavy v. WTI (CAD)",xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="HardH (CAD)")
abline(lm.HardH,col="red")
for (i in 3:n.series) {
	plot(as.numeric(hardh.fc.dist[,1]),as.numeric(hardh.fc.dist[,i]),xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="HardH (CAD)")
	abline(lm.HardH,col="red")
	abline(lm.hard.fc.list[[i]],col="blue")}
# Create loop for distribution fits
lm.hard.fc.dist.list <- vector("list",length=n.series)
for (i in 1:n.series) {
	lm.hard.fc.dist.list[[i]] <- fitdist(summary(lm.hard.fc.list[[i]])$residuals,"logis")
}
summary(lm.hardh.wti.resid.logis)
dev.new();par(mfrow=c(3,3))
plot(lm.hardh.wti.resid.logis, breaks=50);title("Hardisty Heavy (Logistic Distribution)",line=3,col.main="blue")
for (i in 3:n.series) {
	plot(lm.hard.fc.dist.list[[i]], breaks=50)
	print(summary(lm.hard.fc.dist.list[[i]]))}

############################################################
edml.dist.location.m <- as.numeric(summary(lm.edml.wti.resid.logis.m)[[1]][1])
edml.dist.scale.m <- as.numeric(summary(lm.edml.wti.resid.logis.m)[[1]][2])
hardh.dist.location.m <- as.numeric(summary(lm.hardh.wti.resid.logis.m)[[1]][1])
hardh.dist.scale.m <- as.numeric(summary(lm.hardh.wti.resid.logis.m)[[1]][2])
dist.scale.edm.m <- 1
dist.scale.hard.m <- 1

#### Create forward forecasts for Edmonton Light and Hardisty Heavy
last.date.edml <- end(edm.wti.d.dt)
last.date.hardh <- end(hard.wti.d.dt)
last.y.edml <- year(last.date.edml)
last.m.edml <- month(last.date.edml)
last.y.hardh <- year(last.date.hardh)
last.m.hardh <- month(last.date.hardh)

##################################################################################################################################
#### Import Necessary Pricing Files
# WTI Import
setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/CSV Files/50 Years")
wti.hist.fc.upload <- read.csv("WTI Monte Carlo - 1000 Price Series (most recent).csv",header=TRUE,sep=",",dec=".")
wti.dates <- as.character(wti.hist.fc.upload[,1])
wti.values <- as.matrix(wti.hist.fc.upload[,-1])
wti.hist.fc.ts <- timeSeries(wti.values,wti.dates)
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

start.dates <- c(start(edm.wti.m),start(wti.hist.fc.ts),start(usd.cad.ex.hist.fc.ts), start(us.cpi.hist.fc.ts))
start.date <- max(start.dates)
end.dates <- c(end(wti.hist.fc.ts),end(usd.cad.ex.hist.fc.ts), end(us.cpi.hist.fc.ts))
end.date <- min(end.dates)


if (last.m.edml == 12) {
	start.date.edml <- as.Date(paste(last.y.edml+1,"01-01",sep=""),"%Y-%m-%d")
} else {
	start.date.edml <- as.Date(paste(last.y.edml,"-",last.m.edml+1,"-01",sep=""),"%Y-%m-%d")}
if (last.m.hardh == 12) {
	start.date.hardh <- as.Date(paste(last.y.hardh+1,"01-01",sep=""),"%Y-%m-%d")
} else {
	start.date.hardh <- as.Date(paste(last.y.hardh,"-",last.m.hardh+1,"-01",sep=""),"%Y-%m-%d")}


#### Separate out FC windows
wti.fc.ts <- rev(window(wti.hist.fc.ts,start=start.date.edml,end=end.date))
usd.cad.ex.fc.ts <- rev(window(usd.cad.ex.hist.fc.ts,start=start.date.edml,end=end.date))
us.cpi.fc.ts <- window(us.cpi.hist.fc.ts,start=start.date.edml,end=end.date)

#### Separate out FC and mutual history (mh)
wti.mh.fc.ts <- rev(window(wti.hist.fc.ts,start=start.date,end=end.date))
usd.cad.ex.mh.fc.ts <- rev(window(usd.cad.ex.hist.fc.ts,start=start.date,end=end.date))
us.cpi.mh.fc.ts <- window(us.cpi.hist.fc.ts,start=start.date,end=end.date)


###########################################
#### Length of Time Series
fc.periods <- length(wti.fc.ts[,1]) ####### 50 Years
n <- 1000 ######## Number of runs/iterations
###########################################
forecast.ts.edml <- as.data.frame(timeSequence(start.date.edml, length.out=fc.periods,by="month"))
colnames(forecast.ts.edml) <- c("Date")
forecast.ts.hardh <- as.data.frame(timeSequence(start.date.hardh, length.out=fc.periods,by="month"))
colnames(forecast.ts.hardh) <- c("Date")

forecast.dist.edml <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
forecast.dist.hardh <- as.data.frame(matrix(nrow=fc.periods,ncol=n))

for (i in 1:fc.periods) {
	forecast.dist.edml[i,] <- rlogis(n,location=edml.dist.location.m,scale=edml.dist.scale.m/dist.scale.edm.m)
	forecast.dist.hardh[i,] <- rlogis(n,location=hardh.dist.location.m,scale=hardh.dist.scale.m/dist.scale.hard.m)
}

fc.dist.ts.edml <- as.timeSeries(cbind(forecast.ts.edml,forecast.dist.edml))
fc.dist.ts.hardh <- as.timeSeries(cbind(forecast.ts.hardh,forecast.dist.hardh))

################# Price Conversions
#### Create WTI in Real Dollars
wti.real.fc.ts <- wti.fc.ts*us.cpi.fc.ts/100
wti.real.mh.fc.ts <- wti.mh.fc.ts*us.cpi.mh.fc.ts/100

#### Create Real WTI in CAD
wti.real.cad.fc.ts <- wti.real.fc.ts*usd.cad.ex.fc.ts
wti.real.cad.mh.fc.ts <- wti.real.mh.fc.ts*usd.cad.ex.mh.fc.ts

################# Convert to Canadian Prices
edml.hist.end <- as.Date(paste(year(last.date.edml),"-",month(last.date.edml),"-",1,sep=""),format="%Y-%m-%d")
edml.hist <- window(edm.wti.m.dt[,1],start=start(edm.wti.m.dt[,1]),end=edml.hist.end)
hardh.hist.end <- as.Date(paste(year(last.date.hardh),"-",month(last.date.hardh),"-",1,sep=""),format="%Y-%m-%d")
hardh.hist <- window(hard.wti.m.dt[,1],start=start(hard.wti.m.dt[,1]),end=hardh.hist.end)
edml.real.cad.fc.ts <- lm.edml.wti.slope.m*wti.real.cad.fc.ts+lm.edml.wti.intercept.m+fc.dist.ts.edml
hardh.real.cad.fc.ts <- lm.hardh.wti.slope.m*wti.real.cad.fc.ts+lm.hardh.wti.intercept.m+fc.dist.ts.hardh

edml.real.cad.hist.fc.ts <- rbind(edml.hist,edml.real.cad.fc.ts[,1])
for (i in 2:length(edml.real.cad.fc.ts[1,])){
	edml.real.cad.hist.fc.ts <- cbind(edml.real.cad.hist.fc.ts,rbind(edml.hist,edml.real.cad.fc.ts[,i]))}
hardh.real.cad.hist.fc.ts <- rbind(hardh.hist,hardh.real.cad.fc.ts[,1])
for (i in 2:length(hardh.real.cad.fc.ts[1,])){
	hardh.real.cad.hist.fc.ts <- cbind(hardh.real.cad.hist.fc.ts,rbind(hardh.hist,hardh.real.cad.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/CSV Files/50 Years")
write.csv(wti.real.mh.fc.ts,file=paste("WTI (Real,USD) Monte Carlo - ",n," series (most recent).csv",sep=""))
write.csv(wti.real.cad.mh.fc.ts,file=paste("WTI (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(edml.real.cad.hist.fc.ts,file=paste("EdmL (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))
write.csv(hardh.real.cad.hist.fc.ts,file=paste("HardH (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))

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

#### Create the basic data structures
# Edmonton Light Base
forecast.ts.edml.base <- as.data.frame(timeSequence(start.date.edml, length.out=fc.periods,by="month"))
colnames(forecast.ts.edml.base) <- c("Date")
forecast.dist.edml.base <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
forecast.ts.hardh.base <- as.data.frame(timeSequence(start.date.hardh, length.out=fc.periods,by="month"))
colnames(forecast.ts.hardh.base) <- c("Date")
forecast.dist.hardh.base <- as.data.frame(matrix(nrow=fc.periods,ncol=n))

####################################################################### SCO at Edmonton ###########################################################################
# Compare SCO at Edmonton versus Edmonton Par Price
edm.par_sco.win <- window(edm.par.cad.ts,start=start(sco.edm.cad.ts),end=end(sco.edm.cad.ts))
edm.par_sco.lm <- lm(as.numeric(sco.edm.cad.ts[,1])~as.numeric(edm.par_sco.win[,1]))
edm.par_sco.lm.summary <- summary(edm.par_sco.lm)
edm.par_sco.lm.intercept <- edm.par_sco.lm.summary$coefficients[1,1]
edm.par_sco.lm.slope <- edm.par_sco.lm.summary$coefficients[2,1]
edm.par_sco.lm.res <- edm.par_sco.lm.summary$residuals
dev.new()
plot(as.numeric(edm.par_sco.win[,1]),as.numeric(sco.edm.cad.ts[,1]),xlab="EdmL ($CA/bbl)",ylab="SCO ($CA/bbl)",col="blue");grid()
title(main="SCO at Edmonton vs Edmonton Par Price")
abline(edm.par_sco.lm,col="green")
## Logistic distribution - worse fit
#edm.par_sco.lm.logis <- fitdist(edm.par_sco.lm.res,"logis")
#summary(edm.par_sco.lm.logis)
#dev.new()
#plot(edm.par_sco.lm.logis,breaks=30)
#title(main="Logistic")
## Normal distribution - worse fit
edm.par_sco.lm.norm <- fitdist(edm.par_sco.lm.res,"norm")
summary(edm.par_sco.lm.norm)
dev.new()
plot(edm.par_sco.lm.norm,breaks=30)
title(main="Normal")

### SCO Forecast
sco.dist.mean.m <- as.numeric(summary(edm.par_sco.lm.norm)[[1]][1])
sco.dist.stddev.m <- as.numeric(summary(edm.par_sco.lm.norm)[[1]][2])
# Create distribution forecast
forecast.dist.sco <- forecast.dist.edml.base
for (i in 1:fc.periods) {
	forecast.dist.sco[i,] <- rnorm(n,mean=sco.dist.mean.m,sd=sco.dist.stddev.m)
}
# Translate distribution FC into a time series
fc.dist.ts.sco <- as.timeSeries(cbind(forecast.ts.edml.base,forecast.dist.sco))
sco.real.cad.fc.ts <- edm.par_sco.lm.slope * edml.real.cad.fc.ts + edm.par_sco.lm.intercept + fc.dist.ts.sco
for (i in 1:n) {
	colnames(sco.real.cad.fc.ts[,i]) <- paste("SCO.FC.",i,sep="")
}
sco.end.date <- as.Date(paste(year(start(sco.real.cad.fc.ts)),"-",month(start(sco.real.cad.fc.ts))-1,"-",day(start(sco.real.cad.fc.ts)),sep=""))
sco.cad.hist_fc.ts <- rbind(window(sco.edm.cad.ts,start=start(sco.edm.cad.ts),end=sco.end.date),sco.real.cad.fc.ts[,1])
for (i in 1:n){
	sco.cad.hist_fc.ts <- cbind(sco.cad.hist_fc.ts,rbind(window(sco.edm.cad.ts,start=start(sco.edm.cad.ts),end=sco.end.date),sco.real.cad.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(sco.cad.hist_fc.ts,file=paste("SCO (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


############################################################### Compare Cromer versus Edmonton Par Price ############################################################
edm.par_cromer.win <- window(edm.par.cad.ts,start=start(cromer.cad.ts),end=end(cromer.cad.ts))
edm.par_cromer.lm <- lm(as.numeric(cromer.cad.ts[,1])~as.numeric(edm.par_cromer.win[,1]))
edm.par_cromer.lm.summary <- summary(edm.par_cromer.lm)
edm.par_cromer.lm.intercept <- edm.par_cromer.lm.summary $coefficients[1,1]
edm.par_cromer.lm.slope <- edm.par_cromer.lm.summary$coefficients[2,1]
edm.par_cromer.lm.res <- edm.par_cromer.lm.summary$residuals
dev.new()
plot(as.numeric(edm.par_cromer.win[,1]),as.numeric(cromer.cad.ts[,1]),xlab="EdmL ($CA/bbl)",ylab="Cromer ($CA/bbl)",col="blue");grid()
title(main="Cromer vs Edmonton Par Price")
abline(edm.par_cromer.lm,col="green")
## Logistic distribution - Fits are equally questionable but it appears, in general, a logistic distribution better describes residual distributions around a linear model
edm.par_cromer.lm.logis <- fitdist(edm.par_cromer.lm.res,"logis")
summary(edm.par_cromer.lm.logis)
dev.new()
plot(edm.par_cromer.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#edm.par_cromer.lm.norm <- fitdist(edm.par_cromer.lm.res,"norm")
#summary(edm.par_cromer.lm.norm)
#dev.new()
#plot(edm.par_cromer.lm.norm,breaks=30)
#title(main="Normal")

### Cromer Forecast
cromer.dist.location.m <- as.numeric(summary(edm.par_cromer.lm.logis)[[1]][1])
cromer.dist.scale.m <- as.numeric(summary(edm.par_cromer.lm.logis)[[1]][2])
# Create distribution forecast
forecast.dist.cromer<- forecast.dist.edml.base
for (i in 1:fc.periods) {
	forecast.dist.cromer[i,] <- rlogis(n,location=cromer.dist.location.m,scale=cromer.dist.scale.m)
}
# Translate distribution FC into a time series
fc.dist.ts.cromer<- as.timeSeries(cbind(forecast.ts.edml.base,forecast.dist.cromer))
cromer.real.cad.fc.ts <- edm.par_cromer.lm.slope * edml.real.cad.fc.ts + edm.par_cromer.lm.intercept + fc.dist.ts.cromer
for (i in 1:n) {
	colnames(cromer.real.cad.fc.ts[,i]) <- paste("Cromer.FC.",i,sep="")
}
cromer.end.date <- as.Date(paste(year(start(cromer.real.cad.fc.ts)),"-",month(start(cromer.real.cad.fc.ts))-1,"-",day(start(cromer.real.cad.fc.ts)),sep=""))
cromer.cad.hist_fc.ts <- rbind(window(cromer.cad.ts,start=start(cromer.cad.ts),end=cromer.end.date),cromer.real.cad.fc.ts[,1])
for (i in 1:n){
	cromer.cad.hist_fc.ts <- cbind(cromer.cad.hist_fc.ts,rbind(window(cromer.cad.ts,start=start(cromer.cad.ts),end=cromer.end.date),cromer.real.cad.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(cromer.cad.hist_fc.ts,file=paste("Cromer (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


###################################################################### Compare WCS versus Edmonton Par Price #######################################################
###### WCS is better correlated to Hardisty Heavy
#edm.par_wcs.win <- window(edm.par.cad.ts,start=start(wcs.cad.ts),end=end(wcs.cad.ts))
#edm.par_wcs.lm <- lm(as.numeric(wcs.cad.ts[,1])~as.numeric(edm.par_wcs.win[,1]))
#edm.par_wcs.lm.summary <- summary(edm.par_wcs.lm)
#edm.par_wcs.lm.intercept <- edm.par_wcs.lm.summary $coefficients[1,1]
#edm.par_wcs.lm.slope <- edm.par_wcs.lm.summary$coefficients[2,1]
#edm.par_wcs.lm.res <- edm.par_wcs.lm.summary$residuals
#dev.new()
#plot(as.numeric(edm.par_wcs.win[,1]),as.numeric(wcs.cad.ts[,1]),xlab="EdmL ($CA/bbl)",ylab="WCS ($CA/bbl)",col="blue");grid()
#title(main="WCS vs Edmonton Par Price")
#abline(edm.par_wcs.lm,col="green")
## Logistic distribution - worse fit
#edm.par_wcs.lm.logis <- fitdist(edm.par_wcs.lm.res,"logis")
#summary(edm.par_wcs.lm.logis)
#dev.new()
#plot(edm.par_wcs.lm.logis,breaks=30)
#title(main="Logistic")
## Normal distribution - worse fit
#edm.par_wcs.lm.norm <- fitdist(edm.par_wcs.lm.res,"norm")
#summary(edm.par_wcs.lm.norm)
#dev.new()
#plot(edm.par_wcs.lm.norm,breaks=30)
#title(main="Normal")

################################################################## Compare WCS versus Hardisty Heavy ###############################################
###### WCS is better correlated to Hardisty Heavy
hard.h_wcs.win <- window(hard.hvy.cad.ts,start=start(wcs.cad.ts),end=end(wcs.cad.ts))
hard.h_wcs.lm <- lm(as.numeric(wcs.cad.ts[,1])~as.numeric(hard.h_wcs.win[,1]))
hard.h_wcs.lm.summary <- summary(hard.h_wcs.lm)
hard.h_wcs.lm.intercept <- hard.h_wcs.lm.summary $coefficients[1,1]
hard.h_wcs.lm.slope <- hard.h_wcs.lm.summary$coefficients[2,1]
hard.h_wcs.lm.res <- hard.h_wcs.lm.summary$residuals
dev.new()
plot(as.numeric(hard.h_wcs.win[,1]),as.numeric(wcs.cad.ts[,1]),xlab="HardH ($CA/bbl)",ylab="WCS ($CA/bbl)",col="blue");grid()
title(main="WCS vs Hardisty Heavy")
abline(hard.h_wcs.lm,col="green")
## Logistic distribution - better fit
hard.h_wcs.lm.logis <- fitdist(hard.h_wcs.lm.res,"logis")
summary(hard.h_wcs.lm.logis)
dev.new()
plot(hard.h_wcs.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#hard.h_wcs.lm.norm <- fitdist(hard.h_wcs.lm.res,"norm")
#summary(hard.h_wcs.lm.norm)
#dev.new()
#plot(hard.h_wcs.lm.norm,breaks=30)
#title(main="Normal")

### WCS Forecast
wcs.dist.location.m <- as.numeric(summary(hard.h_wcs.lm.logis)[[1]][1])
wcs.dist.scale.m <- as.numeric(summary(hard.h_wcs.lm.logis)[[1]][2])
# Create distribution forecast
forecast.dist.wcs <- forecast.dist.hardh.base
for (i in 1:fc.periods) {
	forecast.dist.wcs [i,] <- rlogis(n,location=wcs.dist.location.m,scale=wcs.dist.scale.m)
}
# Translate distribution FC into a time series
fc.dist.ts.wcs <- as.timeSeries(cbind(forecast.ts.hardh.base,forecast.dist.wcs))
wcs.real.cad.fc.ts <- hard.h_wcs.lm.slope * hardh.real.cad.fc.ts + hard.h_wcs.lm.intercept + fc.dist.ts.wcs
for (i in 1:n) {
	colnames(wcs.real.cad.fc.ts[,i]) <- paste("WCS.FC.",i,sep="")
}
wcs.end.date <- as.Date(paste(year(start(wcs.real.cad.fc.ts)),"-",month(start(wcs.real.cad.fc.ts))-1,"-",day(start(wcs.real.cad.fc.ts)),sep=""))
wcs.cad.hist_fc.ts <- rbind(window(wcs.cad.ts,start=start(wcs.cad.ts),end=wcs.end.date),wcs.real.cad.fc.ts[,1])
for (i in 1:n){
	wcs.cad.hist_fc.ts <- cbind(wcs.cad.hist_fc.ts,rbind(window(wcs.cad.ts,start=start(wcs.cad.ts),end=wcs.end.date),wcs.real.cad.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(wcs.cad.hist_fc.ts,file=paste("WCS (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


############################################################## Compare Hardisty Bow River versus Edmonton Par Price ###########################################
###### Hardisty Bow River is better correlated to Hardisty Heavy
#edm.par_hard.bow.r.win <- window(edm.par.cad.ts,start=start(hard.bow.r.cad.ts),end=end(hard.bow.r.cad.ts))
#edm.par_hard.bow.r.lm <- lm(as.numeric(hard.bow.r.cad.ts[,1])~as.numeric(edm.par_hard.bow.r.win[,1]))
#edm.par_hard.bow.r.lm.summary <- summary(edm.par_hard.bow.r.lm)
#edm.par_hard.bow.r.lm.intercept <- edm.par_hard.bow.r.lm.summary $coefficients[1,1]
#edm.par_hard.bow.r.lm.slope <- edm.par_hard.bow.r.lm.summary$coefficients[2,1]
#edm.par_hard.bow.r.lm.res <- edm.par_hard.bow.r.lm.summary$residuals
#dev.new()
#plot(as.numeric(edm.par_hard.bow.r.win[,1]),as.numeric(hard.bow.r.cad.ts[,1]),xlab="EdmL ($CA/bbl)",ylab="WCS ($CA/bbl)",col="blue");grid()
#title(main="Hardisty Bow River vs Edmonton Par Price")
#abline(edm.par_wcs.lm,col="green")
## Logistic distribution - worse fit
#edm.par_hard.bow.r.lm.logis <- fitdist(edm.par_hard.bow.r.lm.res,"logis")
#summary(edm.par_hard.bow.r.lm.logis)
#dev.new()
#plot(edm.par_hard.bow.r.lm.logis,breaks=30)
#title(main="Logistic")
## Normal distribution - worse fit
#edm.par_hard.bow.r.lm.norm <- fitdist(edm.par_hard.bow.r.lm.res,"norm")
#summary(edm.par_hard.bow.r.lm.norm)
#dev.new()
#plot(edm.par_hard.bow.r.lm.norm,breaks=30)
#title(main="Normal")

############################################################## Compare Hardisty Bow River versus Hardisty Heavy #############################################
###### Hardisty Bow River is better correlated to Hardisty Heavy
hard.h_hard.bow.r.win <- window(hard.hvy.cad.ts,start=start(hard.bow.r.cad.ts),end=end(hard.bow.r.cad.ts))
hard.h_hard.bow.r.lm <- lm(as.numeric(hard.bow.r.cad.ts[,1])~as.numeric(hard.h_hard.bow.r.win[,1]))
hard.h_hard.bow.r.lm.summary <- summary(hard.h_hard.bow.r.lm)
hard.h_hard.bow.r.lm.intercept <- hard.h_hard.bow.r.lm.summary $coefficients[1,1]
hard.h_hard.bow.r.lm.slope <- hard.h_hard.bow.r.lm.summary$coefficients[2,1]
hard.h_hard.bow.r.lm.res <- hard.h_hard.bow.r.lm.summary$residuals
dev.new()
plot(as.numeric(hard.h_hard.bow.r.win[,1]),as.numeric(hard.bow.r.cad.ts[,1]),xlab="HardH ($CA/bbl)",ylab="Hard BowR ($CA/bbl)",col="blue");grid()
title(main="Hardisty Bow River vs Hardisty Heavy")
abline(hard.h_hard.bow.r.lm,col="green")
## Logistic distribution - better fit
hard.h_hard.bow.r.lm.logis <- fitdist(hard.h_hard.bow.r.lm.res,"logis")
summary(hard.h_hard.bow.r.lm.logis)
dev.new()
plot(hard.h_hard.bow.r.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#hard.h_hard.bow.r.lm.norm <- fitdist(hard.h_hard.bow.r.lm.res,"norm")
#summary(hard.h_hard.bow.r.lm.norm)
#dev.new()
#plot(hard.h_hard.bow.r.lm.norm,breaks=30)
#title(main="Normal")

### Hardisty Bow River Forecast
hard.bow.r.dist.location.m <- as.numeric(summary(hard.h_hard.bow.r.lm.logis)[[1]][1])
hard.bow.r.dist.scale.m <- as.numeric(summary(hard.h_hard.bow.r.lm.logis)[[1]][2])
# Create distribution forecast
forecast.dist.hard.bow.r <- forecast.dist.hardh.base
for (i in 1:fc.periods) {
	forecast.dist.hard.bow.r[i,] <- rlogis(n,location=hard.bow.r.dist.location.m,scale=hard.bow.r.dist.scale.m)
}
# Translate distribution FC into a time series
fc.dist.ts.hard.bow.r <- as.timeSeries(cbind(forecast.ts.hardh.base,forecast.dist.hard.bow.r))
hard.bow.r.real.cad.fc.ts <- hard.h_hard.bow.r.lm.slope * hardh.real.cad.fc.ts + hard.h_hard.bow.r.lm.intercept + fc.dist.ts.hard.bow.r
for (i in 1:n) {
	colnames(hard.bow.r.real.cad.fc.ts[,i]) <- paste("HardBR.FC.",i,sep="")
}
hard.bow.r.end.date <- as.Date(paste(year(start(hard.bow.r.real.cad.fc.ts)),"-",month(start(hard.bow.r.real.cad.fc.ts))-1,"-",day(start(hard.bow.r.real.cad.fc.ts)),sep=""))
hard.bow.r.cad.hist_fc.ts <- rbind(window(hard.bow.r.cad.ts,start=start(hard.bow.r.cad.ts),end=hard.bow.r.end.date),hard.bow.r.real.cad.fc.ts[,1])
for (i in 1:n){
	hard.bow.r.cad.hist_fc.ts <- cbind(hard.bow.r.cad.hist_fc.ts,rbind(window(hard.bow.r.cad.ts,start=start(hard.bow.r.cad.ts),end=hard.bow.r.end.date),hard.bow.r.real.cad.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(hard.bow.r.cad.hist_fc.ts,file=paste("Hardisty Bow River (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


############################################################# Compare Ethane versus Edmonton Par Price #######################################################
###### Sproule's ethane price is just a ratio of AECO: $1/mcf AECO=$0.3608805/bbl
#edm.par_ethane.win <- window(edm.par.cad.ts,start=start(ethane.cad.ts),end=end(ethane.cad.ts))
#edm.par_ethane.lm <- lm(as.numeric(ethane.cad.ts[,1])~as.numeric(edm.par_ethane.win[,1]))
#edm.par_ethane.lm.summary <- summary(edm.par_ethane.lm)
#edm.par_ethane.lm.intercept <- edm.par_ethane.lm.summary $coefficients[1,1]
#edm.par_ethane.lm.slope <- edm.par_ethane.lm.summary$coefficients[2,1]
#edm.par_ethane.lm.res <- edm.par_ethane.lm.summary$residuals
#dev.new()
#plot(as.numeric(edm.par_ethane.win[,1]),as.numeric(ethane.cad.ts[,1]),xlab="EdmL ($CA/bbl)",ylab="Ethane ($CA/bbl)",col="blue");grid()
#title(main="Ethane vs Edmonton Par Price")
#abline(edm.par_ethane.lm,col="green")
## Logistic distribution - worse fit
#edm.par_ethane.lm.logis <- fitdist(edm.par_ethane.lm.res,"logis")
#summary(edm.par_ethane.lm.logis)
#dev.new()
#plot(edm.par_ethane.lm.logis,breaks=30)
#title(main="Logistic")
## Normal distribution - worse fit
#edm.par_ethane.lm.norm <- fitdist(edm.par_ethane.lm.res,"norm")
#summary(edm.par_ethane.lm.norm)
#dev.new()
#plot(edm.par_ethane.lm.norm,breaks=30)
#title(main="Normal")

############################################################# Propane linear model doesn't appear to be a good fit - NEEDS WORK!!!! ###########################
####################### Propane is experiencing a break in the linear model over the last couple of months.... ################################################
# Compare Propane versus Edmonton Par Price
edm.par_propane.win <- window(edm.par.cad.ts,start=start(propane.cad.ts),end=end(propane.cad.ts))
edm.par_propane.lm <- lm(as.numeric(propane.cad.ts[,1])~as.numeric(edm.par_propane.win[,1]))
edm.par_propane.lm.summary <- summary(edm.par_propane.lm)
edm.par_propane.lm.intercept <- edm.par_propane.lm.summary $coefficients[1,1]
edm.par_propane.lm.slope <- edm.par_propane.lm.summary$coefficients[2,1]
edm.par_propane.lm.res <- edm.par_propane.lm.summary$residuals
dev.new()
plot(as.numeric(edm.par_propane.win[,1]),as.numeric(propane.cad.ts[,1]),xlab="EdmL ($CA/bbl)",ylab="Propane ($CA/bbl)",col="blue");grid()
title(main="Propane vs Edmonton Par Price")
abline(edm.par_propane.lm,col="green")
## Logistic distribution - better fit
edm.par_propane.lm.logis <- fitdist(edm.par_propane.lm.res,"logis")
summary(edm.par_propane.lm.logis)
dev.new()
plot(edm.par_propane.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#edm.par_propane.lm.norm <- fitdist(edm.par_propane.lm.res,"norm")
#summary(edm.par_propane.lm.norm)
#dev.new()
#plot(edm.par_propane.lm.norm,breaks=30)
#title(main="Normal")

### Propane Forecast
propane.dist.location.m <- as.numeric(summary(edm.par_propane.lm.logis)[[1]][1])
propane.dist.scale.m <- as.numeric(summary(edm.par_propane.lm.logis)[[1]][2])
# Create distribution forecast
forecast.dist.propane <- forecast.dist.edml.base
for (i in 1:fc.periods) {
	forecast.dist.propane[i,] <- rlogis(n,location=propane.dist.location.m,scale=propane.dist.scale.m)
}
# Translate distribution FC into a time series
fc.dist.ts.propane <- as.timeSeries(cbind(forecast.ts.edml.base,forecast.dist.propane))
propane.real.cad.fc.ts <- edm.par_propane.lm.slope * edml.real.cad.fc.ts + edm.par_propane.lm.intercept + fc.dist.ts.propane
for (i in 1:n) {
	colnames(propane.real.cad.fc.ts[,i]) <- paste("Pro.FC.",i,sep="")
}
propane.end.date <- as.Date(paste(year(start(propane.real.cad.fc.ts)),"-",month(start(propane.real.cad.fc.ts))-1,"-",day(start(propane.real.cad.fc.ts)),sep=""))
propane.cad.hist_fc.ts <- rbind(window(propane.cad.ts,start=start(propane.cad.ts),end=propane.end.date),propane.real.cad.fc.ts[,1])
for (i in 1:n){
	propane.cad.hist_fc.ts <- cbind(propane.cad.hist_fc.ts,rbind(window(propane.cad.ts,start=start(propane.cad.ts),end=propane.end.date),propane.real.cad.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(propane.cad.hist_fc.ts,file=paste("Propane (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


################################################################### Compare Butane versus Edmonton Par Price #######################################################
edm.par_butane.win <- window(edm.par.cad.ts,start=start(butane.cad.ts),end=end(butane.cad.ts))
edm.par_butane.lm <- lm(as.numeric(butane.cad.ts[,1])~as.numeric(edm.par_butane.win[,1]))
edm.par_butane.lm.summary <- summary(edm.par_butane.lm)
edm.par_butane.lm.intercept <- edm.par_butane.lm.summary $coefficients[1,1]
edm.par_butane.lm.slope <- edm.par_butane.lm.summary$coefficients[2,1]
edm.par_butane.lm.res <- edm.par_butane.lm.summary$residuals
dev.new()
plot(as.numeric(edm.par_butane.win[,1]),as.numeric(butane.cad.ts[,1]),xlab="Butane ($CA/bbl)",ylab="EdmL ($CA/bbl)",col="blue");grid()
title(main="Butane vs Edmonton Par Price")
abline(edm.par_butane.lm,col="green")
## Logistic distribution
edm.par_butane.lm.logis <- fitdist(edm.par_butane.lm.res,"logis")
summary(edm.par_butane.lm.logis)
dev.new()
plot(edm.par_butane.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#edm.par_butane.lm.norm <- fitdist(edm.par_butane.lm.res,"norm")
#summary(edm.par_butane.lm.norm)
#dev.new()
#plot(edm.par_butane.lm.norm,breaks=30)
#title(main="Normal")

### Butane Forecast
butane.dist.location.m <- as.numeric(summary(edm.par_butane.lm.logis)[[1]][1])
butane.dist.scale.m <- as.numeric(summary(edm.par_butane.lm.logis)[[1]][2])
# Create distribution forecast
forecast.dist.butane <- forecast.dist.edml.base
for (i in 1:fc.periods) {
	forecast.dist.butane[i,] <- rlogis(n,location=butane.dist.location.m,scale=butane.dist.scale.m)
}
# Translate distribution FC into a time series
fc.dist.ts.butane <- as.timeSeries(cbind(forecast.ts.edml.base,forecast.dist.butane))
butane.real.cad.fc.ts <- edm.par_butane.lm.slope * edml.real.cad.fc.ts + edm.par_butane.lm.intercept + fc.dist.ts.butane
for (i in 1:n) {
	colnames(butane.real.cad.fc.ts[,i]) <- paste("Pro.FC.",i,sep="")
}
butane.end.date <- as.Date(paste(year(start(butane.real.cad.fc.ts)),"-",month(start(butane.real.cad.fc.ts))-1,"-",day(start(butane.real.cad.fc.ts)),sep=""))
butane.cad.hist_fc.ts <- rbind(window(butane.cad.ts,start=start(butane.cad.ts),end=butane.end.date),butane.real.cad.fc.ts[,1])
for (i in 1:n){
	butane.cad.hist_fc.ts <- cbind(butane.cad.hist_fc.ts,rbind(window(butane.cad.ts,start=start(butane.cad.ts),end=butane.end.date),butane.real.cad.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(butane.cad.hist_fc.ts,file=paste("Butane (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


################################################################### Compare Pentane versus Edmonton Par Price ###########################################################
edm.par_pentane.win <- window(edm.par.cad.ts,start=start(pentane.cad.ts),end=end(pentane.cad.ts))
edm.par_pentane.lm <- lm(as.numeric(pentane.cad.ts[,1])~as.numeric(edm.par_pentane.win[,1]))
edm.par_pentane.lm.summary <- summary(edm.par_pentane.lm)
edm.par_pentane.lm.intercept <- edm.par_pentane.lm.summary $coefficients[1,1]
edm.par_pentane.lm.slope <- edm.par_pentane.lm.summary$coefficients[2,1]
edm.par_pentane.lm.res <- edm.par_pentane.lm.summary$residuals
dev.new()
plot(as.numeric(edm.par_pentane.win[,1]),as.numeric(pentane.cad.ts[,1]),xlab="Pentane ($CA/bbl)",ylab="EdmL ($CA/bbl)",col="blue");grid()
title(main="Pentane vs Edmonton Par Price")
abline(edm.par_pentane.lm,col="green")
## Logistic distribution - worse fit
edm.par_pentane.lm.logis <- fitdist(edm.par_pentane.lm.res,"logis")
summary(edm.par_pentane.lm.logis)
dev.new()
plot(edm.par_pentane.lm.logis,breaks=30)
title(main="Logistic")
## Normal distribution - worse fit
#edm.par_pentane.lm.norm <- fitdist(edm.par_pentane.lm.res,"norm")
#summary(edm.par_pentane.lm.norm)
#dev.new()
#plot(edm.par_pentane.lm.norm,breaks=30)
#title(main="Normal")

### Pentane Forecast
pentane.dist.location.m <- as.numeric(summary(edm.par_pentane.lm.logis)[[1]][1])
pentane.dist.scale.m <- as.numeric(summary(edm.par_pentane.lm.logis)[[1]][2])
# Create distribution forecast
forecast.dist.pentane <- forecast.dist.edml.base
for (i in 1:fc.periods) {
	forecast.dist.pentane[i,] <- rlogis(n,location=pentane.dist.location.m,scale=pentane.dist.scale.m)
}
# Translate distribution FC into a time series
fc.dist.ts.pentane <- as.timeSeries(cbind(forecast.ts.edml.base,forecast.dist.pentane))
pentane.real.cad.fc.ts <- edm.par_pentane.lm.slope * edml.real.cad.fc.ts + edm.par_pentane.lm.intercept + fc.dist.ts.pentane
for (i in 1:n) {
	colnames(pentane.real.cad.fc.ts[,i]) <- paste("Pro.FC.",i,sep="")
}
pentane.end.date <- as.Date(paste(year(start(pentane.real.cad.fc.ts)),"-",month(start(pentane.real.cad.fc.ts))-1,"-",day(start(pentane.real.cad.fc.ts)),sep=""))
pentane.cad.hist_fc.ts <- rbind(window(pentane.cad.ts,start=start(pentane.cad.ts),end=pentane.end.date),pentane.real.cad.fc.ts[,1])
for (i in 1:n){
	pentane.cad.hist_fc.ts <- cbind(pentane.cad.hist_fc.ts,rbind(window(pentane.cad.ts,start=start(pentane.cad.ts),end=pentane.end.date),pentane.real.cad.fc.ts[,i]))}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Prices")
write.csv(pentane.cad.hist_fc.ts,file=paste("Pentane (Real,CAD) Monte Carlo - ",n," series (most recent).csv",sep=""))


################# PDF Reports
color.palate <- c("blue","green","orange","pink","red","purple","thistle","turquoise","grey","brown")
setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/PDF R-Reports/50 Years")

### WTI in Real USD
pdf("WTI Volatility in Real USD.pdf")
plot(wti.real.mh.fc.ts[,1],xlim=c(start(wti.real.mh.fc.ts[,1]),end(wti.real.mh.fc.ts[,1])),ylim=c(0,600))
title(paste("WTI in Real USD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(wti.real.mh.fc.ts[1,])-1) {lines(wti.real.mh.fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(wti.real.mh.fc.ts[,i],col="black")
dev.off()

### WTI in Real CAD
pdf("WTI Volatility in Real CAD.pdf")
plot(wti.real.cad.mh.fc.ts[,1],xlim=c(start(wti.real.cad.mh.fc.ts[,1]),end(wti.real.cad.mh.fc.ts[,1])),ylim=c(0,600))
title(paste("WTI in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(wti.real.cad.mh.fc.ts[1,])-1) {lines(wti.real.cad.mh.fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(wti.real.cad.mh.fc.ts[,i],col="black")
dev.off()

### Canadian Oil Prices and Oil Derivatives
pdf("Canadian Oil Volatility (Real CAD).pdf")
########################### Correlation plot btw EdmL & WTI
plot(edm.wti.d.dt$WTI,edm.wti.d.dt$EdmL,col="green", main="Edmonton Light v. WTI (CAD$/bbl)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD$/bbl)",ylab="Edmonton Light (CAD$/bbl)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(lm.EdmL,col="red")

# EdmL Distribution fit
plot(lm.edml.wti.resid.logis, breaks=50);title("Edmonton Light-WTI Residuals (Logistic Distribution)",line=3,col.main="blue")

# EdmL (Real CAD) Forecasts
plot(edml.real.cad.hist.fc.ts[,1],xlim=c(start(edml.real.cad.hist.fc.ts[,1]),end(edml.real.cad.hist.fc.ts[,1])),ylim=c(0,600),xlab="",ylab="Edmonton Light (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(edml.real.cad.hist.fc.ts[,1]),end(edml.real.cad.hist.fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,600,50), las=2)
title(paste("Edmonton Light in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(edml.real.cad.hist.fc.ts[1,])-1) {lines(edml.real.cad.hist.fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(edml.real.cad.hist.fc.ts[,i],col="black")

############################ Correlation plot btw SCO & EdmL
plot(as.numeric(edm.par_sco.win[,1]),as.numeric(sco.edm.cad.ts[,1]),col="blue", main="SCO @ Edmonton v. Edmonton Light (CAD$/bbl)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="Edmonton Light (CAD$/bbl)",ylab="SCO @ Edmonton (CAD$/bbl)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(edm.par_sco.lm,col="green")

# SCO Distribution fit
plot(edm.par_sco.lm.norm,breaks=30);title("SCO @ Edmonton-Edmonton Light Residuals (Normal Distribution)",line=3,col.main="blue")

# SCO (Real CAD) Forecasts
plot(sco.cad.hist_fc.ts[,1],xlim=c(start(sco.cad.hist_fc.ts[,1]),end(sco.cad.hist_fc.ts[,1])),ylim=c(0,600),xlab="",ylab="SCO @ Edmonton (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(sco.cad.hist_fc.ts[,1]),end(sco.cad.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,600,50), las=2)
title(paste("SCO @ Edmonton in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(sco.cad.hist_fc.ts[1,])-1) {lines(sco.cad.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(sco.cad.hist_fc.ts[,i],col="black")

############################ Correlation plot btw Cromer & EdmL
plot(as.numeric(edm.par_cromer.win[,1]),as.numeric(cromer.cad.ts[,1]),col="blue", main="Cromer v. Edmonton Light (CAD$/bbl)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="Edmonton Light (CAD$/bbl)",ylab="Cromer (CAD$/bbl)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(edm.par_cromer.lm,col="green")

# Cromer Distribution fit
plot(edm.par_cromer.lm.logis,breaks=30);title("Cromer-Edmonton Light Residuals (Logistic Distribution)",line=3,col.main="blue")

# Cromer (Real CAD) Forecasts
plot(cromer.cad.hist_fc.ts[,1],xlim=c(start(cromer.cad.hist_fc.ts[,1]),end(cromer.cad.hist_fc.ts[,1])),ylim=c(0,600),xlab="",ylab="Cromer (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(cromer.cad.hist_fc.ts[,1]),end(cromer.cad.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,600,50), las=2)
title(paste("Cromer in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(cromer.cad.hist_fc.ts[1,])-1) {lines(cromer.cad.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(cromer.cad.hist_fc.ts[,i],col="black")

############################ Correlation plot btw Propane & EdmL
plot(as.numeric(edm.par_propane.win[,1]),as.numeric(propane.cad.ts[,1]),col="blue", main="Propane v. Edmonton Light (CAD$/bbl)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="Edmonton Light (CAD$/bbl)",ylab="Propane (CAD$/bbl)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(edm.par_propane.lm,col="green")

# Propane Distribution fit
plot(edm.par_propane.lm.logis,breaks=30);title("Propane-Edmonton Light Residuals (Logistic Distribution)",line=3,col.main="blue")

# Propane (Real CAD) Forecasts
plot(propane.cad.hist_fc.ts[,1],xlim=c(start(propane.cad.hist_fc.ts[,1]),end(propane.cad.hist_fc.ts[,1])),ylim=c(0,600),xlab="",ylab="Propane (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(propane.cad.hist_fc.ts[,1]),end(propane.cad.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,600,50), las=2)
title(paste("Propane in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(propane.cad.hist_fc.ts[1,])-1) {lines(propane.cad.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(propane.cad.hist_fc.ts[,i],col="black")

############################ Correlation plot btw Butane & EdmL
plot(as.numeric(edm.par_butane.win[,1]),as.numeric(butane.cad.ts[,1]),col="blue", main="Butane v. Edmonton Light (CAD$/bbl)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="Edmonton Light (CAD$/bbl)",ylab="Butane (CAD$/bbl)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(edm.par_butane.lm,col="green")

# Butane Distribution fit
plot(edm.par_butane.lm.logis,breaks=30);title("Butane-Edmonton Light Residuals (Logistic Distribution)",line=3,col.main="blue")

# Butane (Real CAD) Forecasts
plot(butane.cad.hist_fc.ts[,1],xlim=c(start(butane.cad.hist_fc.ts[,1]),end(butane.cad.hist_fc.ts[,1])),ylim=c(0,600),xlab="",ylab="Butane (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(butane.cad.hist_fc.ts[,1]),end(butane.cad.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,600,50), las=2)
title(paste("Butane in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(butane.cad.hist_fc.ts[1,])-1) {lines(butane.cad.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(butane.cad.hist_fc.ts[,i],col="black")

############################ Correlation plot btw Pentane & EdmL
plot(as.numeric(edm.par_pentane.win[,1]),as.numeric(pentane.cad.ts[,1]),col="blue", main="Pentane v. Edmonton Light (CAD$/bbl)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="Edmonton Light (CAD$/bbl)",ylab="Pentane (CAD$/bbl)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(edm.par_pentane.lm,col="green")

# Pentane Distribution fit
plot(edm.par_pentane.lm.logis,breaks=30);title("Pentane-Edmonton Light Residuals (Logistic Distribution)",line=3,col.main="blue")

# Pentane (Real CAD) Forecasts
plot(pentane.cad.hist_fc.ts[,1],xlim=c(start(pentane.cad.hist_fc.ts[,1]),end(pentane.cad.hist_fc.ts[,1])),ylim=c(0,600),xlab="",ylab="Pentane (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(pentane.cad.hist_fc.ts[,1]),end(pentane.cad.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,600,50), las=2)
title(paste("Pentane in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(pentane.cad.hist_fc.ts[1,])-1) {lines(pentane.cad.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(pentane.cad.hist_fc.ts[,i],col="black")

############################ Correlation plot btw HardH & WTI
plot(hard.wti.d.dt$WTI,hard.wti.d.dt$HardH,col="green", main="Hardisty Heavy v. WTI (CAD)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="WTI (CAD)",ylab="HardH (CAD)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(lm.HardH,col="red")

# HardH Distribution fit
plot(lm.hardh.wti.resid.logis, breaks=50);title("Hardisty Heavy-WTI Residuals (Logistic Distribution)",line=3,col.main="blue")

# HardH (Real CAD) Forecasts
plot(hardh.real.cad.hist.fc.ts[,1],xlim=c(start(hardh.real.cad.hist.fc.ts[,1]),end(hardh.real.cad.hist.fc.ts[,1])),ylim=c(0,600),xlab="",ylab="Hardisty Heavy (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(hardh.real.cad.hist.fc.ts[,1]),end(hardh.real.cad.hist.fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,600,50), las=2)
title(paste("Hardisty Heavy in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(hardh.real.cad.hist.fc.ts[1,])-1) {lines(hardh.real.cad.hist.fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(hardh.real.cad.hist.fc.ts[,i],col="black")

############################ Correlation plot btw WCS & HardH
plot(as.numeric(hard.h_wcs.win[,1]),as.numeric(wcs.cad.ts[,1]),col="blue", main="WCS v. Hardisty Heavy (CAD$/bbl)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="Hardisty Heavy (CAD$/bbl)",ylab="WCS (CAD$/bbl)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(hard.h_wcs.lm,col="green")

# WCS Distribution fit
plot(hard.h_wcs.lm.logis,breaks=30);title("WCS-Hardisty Heavy Residuals (Logistic Distribution)",line=3,col.main="blue")

# WCS (Real CAD) Forecasts
plot(wcs.cad.hist_fc.ts[,1],xlim=c(start(wcs.cad.hist_fc.ts[,1]),end(wcs.cad.hist_fc.ts[,1])),ylim=c(0,600),xlab="",ylab="WCS (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(wcs.cad.hist_fc.ts[,1]),end(wcs.cad.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,600,50), las=2)
title(paste("WCS in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(wcs.cad.hist_fc.ts[1,])-1) {lines(wcs.cad.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(wcs.cad.hist_fc.ts[,i],col="black")

############################ Correlation plot btw HardBR & HardH
plot(as.numeric(hard.h_hard.bow.r.win[,1]),as.numeric(hard.bow.r.cad.ts[,1]),col="blue", main="Hardisty Bow River v. Hardisty Heavy (CAD$/bbl)",panel.first=abline(h=seq(0,150,by=10),v=seq(0,150,by=10),lty=3,col="gray"),axes=FALSE,
	xlim=c(0,150),ylim=c(0,150),xlab="Hardisty Heavy (CAD$/bbl)",ylab="Hardisty Bow River (CAD$/bbl)")
axis(1,at=seq(0,150,by=10),las=2)
axis(2,at=seq(0,150,by=10),las=2)
abline(hard.h_hard.bow.r.lm,col="green")

# Hardisty Bow River Distribution fit
plot(hard.h_hard.bow.r.lm.logis,breaks=30);title("Hardisty Bow River-Hardisty Heavy Residuals (Logistic Distribution)",line=3,col.main="blue")

# Hardisty Bow River (Real CAD) Forecasts
plot(hard.bow.r.cad.hist_fc.ts[,1],xlim=c(start(hard.bow.r.cad.hist_fc.ts[,1]),end(hard.bow.r.cad.hist_fc.ts[,1])),ylim=c(0,600),xlab="",ylab="Hardisty Bow River (CAD$/bbl)",axes=FALSE)
axis.POSIXct(1,at=seq(start(hard.bow.r.cad.hist_fc.ts[,1]),end(hard.bow.r.cad.hist_fc.ts[,1]),by="5 years"),las=2)
axis(2, at=seq(0,600,50), las=2)
title(paste("Hardisty Bow River in Real CAD Dollars - Index Year ",cpi.index.year,sep=""))
for (i in 2:length(hard.bow.r.cad.hist_fc.ts[1,])-1) {lines(hard.bow.r.cad.hist_fc.ts[,i],col=color.palate[as.numeric(str_sub(i,start=-1))])}
lines(hard.bow.r.cad.hist_fc.ts[,i],col="black")

dev.off()
