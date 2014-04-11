library(Quandl)
library(quantmod)
library(fImport)
library(fitdistrplus)
library(timeSeries)
library(timeDate)
library(zoo)
library(xts)
library(schwartz97)
library(forecast)
library(lubridate)
library(sn)
library(stringr)

#### Download US & Canada CPI
cpi.us.m <- fredSeries("CPIAUCNS", from="1985-01-01",to=Sys.Date()) #Index 1982-1984=100

last.year.cpi.us <- year(end(cpi.us.m))
last.month.cpi.us <- month(end(cpi.us.m)) 
last.day.cpi.us <- day(end(cpi.us.m)) 

#### Re-index the CPI curves to the last full year (average)
index.year.cpi.us <- last.year.cpi.us

cpi.us.index <- mean(window(cpi.us.m,start=as.Date(paste(index.year.cpi.us,"-01-01",sep=""),format="%Y-%m-%d"),end=as.Date(paste(index.year.cpi.us,"-12-31",sep=""),format="%Y-%m-%d")))

cpi.us.m.last.index <- cpi.us.m/cpi.us.index*100

dev.new()
plot(cpi.us.m.last.index,main="US Historical CPI",xlab="",ylab=paste("US CPI: Indexed to ",index.year.cpi.us,sep=""))

#### Calculate Monthly Returns
cpi.us.m.returns <- as.numeric(returns(cpi.us.m.last.index))*100 # Scale up

#### Fit the monthly returns to Logistic Distribution
cpi.us.m.ret.fit.log <- fitdist(cpi.us.m.returns, distr= "logis")
dev.new()
plot(cpi.us.m.ret.fit.log, breaks=30)
title(main="US Inflation")

#### Fit monthly returns with Skew-Normal and Skew-T distributions
#fit.sk.norm <- msn.fit(y=cpi.us.m.returns,plot.it=TRUE)
#fit.sk.t <- mst.fit(y=cpi.us.m.returns,plot.it=TRUE)

#### Fit the monthly returns to Normal Distribution (Poor fit)
#####cpi.us.m.ret.fit.norm <- fitdist(cpi.us.m.returns, distr= "norm")
#####dev.new()
#####plot(cpi.us.m.ret.fit.norm, breaks=30)

# Find the forecast start date
cpi.us.last.month <- end(cpi.us.m)
us.new.ts.start.yr <- as.numeric(format(cpi.us.last.month,"%Y"))
us.new.ts.start.m <- as.numeric(format(cpi.us.last.month,"%m"))+1
if (us.new.ts.start.m > 12) {
	us.new.ts.start.yr<-us.new.ts.start.yr+1
	us.new.ts.start.m<-1
}
us.new.ts.start <- as.Date(paste(us.new.ts.start.yr,"-",us.new.ts.start.m,"-01",sep=""),"%Y-%m-%d") # US forecast start date

##### Number of forecast periods (months): 50 year forecast
fc.periods <- 12*50+1

# Create forecast matrices
forward.forecast.us <- as.data.frame(timeSequence(us.new.ts.start,length.out=fc.periods, by="month"))
colnames(forward.forecast.us) <- c("Date")

n <- 1000 # Number of samples
forecast.matrix.us <- matrix(nrow=fc.periods,ncol=n)
log.dist.us <- matrix(nrow=fc.periods, ncol=n)

# Generate random returns
log.dist.us[1,] <- rlogis(n, location = cpi.us.m.ret.fit.log$estimate[1], scale = cpi.us.m.ret.fit.log$estimate[2])/100
# Calculate new CPI from the random returns
new.month.us <- log.dist.us[1,] * as.numeric(cpi.us.m.last.index[end(cpi.us.m.last.index),]) + as.numeric(cpi.us.m.last.index[end(cpi.us.m.last.index),])
# Populate
forecast.matrix.us[1,] <- new.month.us

for (i in 2:fc.periods) {
	log.dist.us[i,] <- rlogis(n, location = cpi.us.m.ret.fit.log$estimate[1], scale = cpi.us.m.ret.fit.log$estimate[2])/100
	new.month.us <- log.dist.us[i,] * as.numeric(forecast.matrix.us[i-1,]) + as.numeric(forecast.matrix.us[i-1,])
	forecast.matrix.us[i,] <- new.month.us
}

# Convert forecasts into timeSeries
forecast.df.us <- as.data.frame(forecast.matrix.us)
forecast.us <- cbind(forward.forecast.us,forecast.df.us)
forecast.ts.us <- as.timeSeries(forecast.us)

# Fill Data.Frame with both historical and forecasted CPI
cpi.us.m.hist.fc <- rbind(cpi.us.m.last.index,forecast.ts.us[,1])
for (i in 2:n) {
	cpi.us.m.hist.fc <- cbind(cpi.us.m.hist.fc,rbind(cpi.us.m.last.index,forecast.ts.us[,i]))
}

# Write CSV files for future use
setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/CAD US CPI/CSV Files") # Working directory at the Office
#write.csv(cpi.us.m.last.index,file=paste("US Historical CPI - Index Year ",index.year.cpi.us,".csv",sep=""))
#write.csv(forecast.ts.us,file=paste("US Forecast CPI - Index Year ",index.year.cpi.us,".csv",sep=""))
write.csv(cpi.us.m.hist.fc,file=paste("US Hist, FC CPI.csv",sep=""))

### Plotting
cpi.us.m.hist.fc.ret <- returns(cpi.us.m.hist.fc)

write.csv(cpi.us.m.hist.fc.ret,file=paste("US Hist, FC Monthly Inflation.csv",sep=""))

color.palate <- c("blue","green","orange","pink","red","purple","thistle","turquoise","grey","brown")
dev.new()
plot(cpi.us.m.hist.fc[,1],main="US Historical & Forecast CPI",xlab="",ylab=paste("US CPI: Indexed to ",index.year.cpi.us,sep=""))
for (m in 2:n-1) {lines(cpi.us.m.hist.fc[,m],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(cpi.us.m.hist.fc[,n],col="black")

dev.new()
plot(cpi.us.m.hist.fc.ret[,1],main="US Historical & Forecast Inflation",xlab="",ylab="US Inflation [%]")
for (m in 2:n-1) {lines(cpi.us.m.hist.fc.ret[,m],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(cpi.us.m.hist.fc.ret[,n],col="black")

#print(summary(cpi.us.m.ret.fit.log))

c1 <-1
#### Test Histogram and Density functions of the US CPI Forecast Prices
while (c1 < n) 
{
	price.fc.temp <- forecast.ts.us[,c1]
	us.cpi.fc.returns <- returns(price.fc.temp)
	### Fitting WTI Monthly Returns Distribution
	us.cpi.fc.returns.num <- as.numeric(us.cpi.fc.returns)*100
	us.cpi.fc.returns.fit.norm <- fitdist(us.cpi.fc.returns.num, distr= "logis") #Fits US CPI returns with Logistic Distribution
	print(summary(us.cpi.fc.returns.fit.norm))
	dev.new(); plot(us.cpi.fc.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
	title(paste("Gaussian ",c1,sep=""))
	c1 <- c1 + trunc(n/10)
}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/PDF R-Reports/50 Years")
pdf(file="US CPI Volatility Report.pdf")
#Plot of Historical US CPI
plot(cpi.us.m.last.index,main="US Historical CPI",xlab="",ylab=paste("US CPI: Indexed to ",index.year.cpi.us,sep=""))
#Distribution of CPI returns
plot(cpi.us.m.ret.fit.log, breaks=30)
title(main="US Inflation")
#Plot of Future Price Curves
plot(cpi.us.m.hist.fc[,1],main="US Historical & Forecast CPI",xlab="",ylab=paste("US CPI: Indexed to ",index.year.cpi.us,sep=""))
for (m in 2:n-1) {lines(cpi.us.m.hist.fc[,m],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(cpi.us.m.hist.fc[,n],col="black")
#Plot of Future Price Returns
plot(cpi.us.m.hist.fc.ret[,1],main="US Historical & Forecast Inflation",xlab="",ylab="US Inflation [%]")
for (m in 2:n-1) {lines(cpi.us.m.hist.fc.ret[,m],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(cpi.us.m.hist.fc.ret[,n],col="black")
dev.off()

