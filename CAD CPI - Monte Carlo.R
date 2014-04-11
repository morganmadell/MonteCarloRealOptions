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

#### Download Canada CPI
cpi.cad.m <- fredSeries("CANCPIALLMINMEI", from="1985-01-01",to=Sys.Date()) #Index 1982-1984=100

last.year.cpi.cad <- year(end(cpi.cad.m))
last.month.cpi.cad <- month(end(cpi.cad.m)) 
last.day.cpi.cad <- day(end(cpi.cad.m)) 

#### Re-index the CPI curves to the last full year (average)
index.year.cpi.cad <- last.year.cpi.cad

cpi.cad.index <- mean(window(cpi.cad.m,start=as.Date(paste(index.year.cpi.cad,"-01-01",sep=""),format="%Y-%m-%d"),end=as.Date(paste(index.year.cpi.cad,"-12-31",sep=""),format="%Y-%m-%d")))

cpi.cad.m.last.index <- cpi.cad.m/cpi.cad.index*100

dev.new()
plot(cpi.cad.m.last.index,main="Canada Historical CPI",xlab="",ylab=paste("CAN CPI: Indexed to ",index.year.cpi.cad,sep=""))

#### Calculate Monthly Returns
cpi.cad.m.returns <- as.numeric(returns(cpi.cad.m.last.index))*100 # Scale up

#### Fit the monthly returns to Logistic Distribution
cpi.cad.m.ret.fit.log <- fitdist(cpi.cad.m.returns, distr= "logis")
dev.new()
plot(cpi.cad.m.ret.fit.log, breaks=30)
title(main="Canada Inflation")

#### Fit monthly returns with Skew-Normal and Skew-T distributions
#fit.sk.norm <- msn.fit(y=cpi.cad.m.returns,plot.it=TRUE)
#fit.sk.t <- mst.fit(y=cpi.cad.m.returns,plot.it=TRUE)

#### Fit the monthly returns to Normal Distribution (Poor fit)
#####cpi.cad.m.ret.fit.norm <- fitdist(cpi.cad.m.returns, distr= "norm")
#####dev.new()
#####plot(cpi.cad.m.ret.fit.norm, breaks=30)

# Find the forecast start date
cpi.cad.last.month <- end(cpi.cad.m)
cad.new.ts.start.yr <- as.numeric(format(cpi.cad.last.month,"%Y"))
cad.new.ts.start.m <- as.numeric(format(cpi.cad.last.month,"%m"))+1
if (cad.new.ts.start.m > 12) {
	cad.new.ts.start.yr<-cad.new.ts.start.yr+1
	cad.new.ts.start.m<-1
}
cad.new.ts.start <- as.Date(paste(cad.new.ts.start.yr,"-",cad.new.ts.start.m,"-01",sep=""),"%Y-%m-%d") # Canada forecast start date

##### Number of forecast periods (months): 50 year forecast
fc.periods <- 12*50+1

# Create forecast matrices
forward.forecast.cad <- as.data.frame(timeSequence(cad.new.ts.start,length.out=fc.periods, by="month"))
colnames(forward.forecast.cad) <- c("Date")

n <- 1000 # Number of samples
forecast.matrix.cad <- matrix(nrow=fc.periods,ncol=n)
log.dist.cad <- matrix(nrow=fc.periods, ncol=n)

# Generate random returns
log.dist.cad[1,] <- rlogis(n, location = cpi.cad.m.ret.fit.log$estimate[1], scale = cpi.cad.m.ret.fit.log$estimate[2])/100
# Calculate new CPI from the random returns
new.month.cad <- log.dist.cad[1,] * as.numeric(cpi.cad.m.last.index[end(cpi.cad.m.last.index),]) + as.numeric(cpi.cad.m.last.index[end(cpi.cad.m.last.index),])
# Populate
forecast.matrix.cad[1,] <- new.month.cad

for (i in 2:fc.periods) {
	log.dist.cad[i,] <- rlogis(n, location = cpi.cad.m.ret.fit.log$estimate[1], scale = cpi.cad.m.ret.fit.log$estimate[2])/100
	new.month.cad <- log.dist.cad[i,] * as.numeric(forecast.matrix.cad[i-1,]) + as.numeric(forecast.matrix.cad[i-1,])
	forecast.matrix.cad[i,] <- new.month.cad
}

# Convert forecasts into timeSeries
forecast.df.cad <- as.data.frame(forecast.matrix.cad)
forecast.cad <- cbind(forward.forecast.cad,forecast.df.cad)
forecast.ts.cad <- as.timeSeries(forecast.cad)

# Fill Data.Frame with both historical and forecasted CPI
cpi.cad.m.hist.fc <- rbind(cpi.cad.m.last.index,forecast.ts.cad[,1])
for (i in 2:n) {
	cpi.cad.m.hist.fc <- cbind(cpi.cad.m.hist.fc,rbind(cpi.cad.m.last.index,forecast.ts.cad[,i]))
}

# Write CSV files for future use
setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/CAD US CPI/CSV Files") # Working directory at the Office
#write.csv(cpi.cad.m.last.index,file=paste("Canada Historical CPI - Index Year ",index.year.cpi.cad,".csv",sep=""))
#write.csv(forecast.ts.cad,file=paste("Canada Forecast CPI - Index Year ",index.year.cpi.cad,".csv",sep=""))
write.csv(cpi.cad.m.hist.fc,file=paste("Canada Hist, FC CPI.csv",sep=""))

### Plotting
cpi.cad.m.hist.fc.ret <- returns(cpi.cad.m.hist.fc)
write.csv(cpi.cad.m.hist.fc.ret,file=paste("Canada Hist, FC Monthly Inflation.csv",sep=""))

color.palate <- c("blue","green","orange","pink","red","purple","thistle","turquoise","grey","brown")
dev.new()
plot(cpi.cad.m.hist.fc[,1],main="Canada Historical & Forecast CPI",xlab="",ylab=paste("Canada CPI: Indexed to ",index.year.cpi.cad,sep=""))
for (m in 2:n-1) {lines(cpi.cad.m.hist.fc[,m],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(cpi.cad.m.hist.fc[,n],col="black")

dev.new()
plot(cpi.cad.m.hist.fc.ret[,1],main="Canada Historical & Forecast Inflation",xlab="",ylab="Canada Inflation [%]")
for (m in 2:n-1) {lines(cpi.cad.m.hist.fc.ret[,m],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(cpi.cad.m.hist.fc.ret[,n],col="black")

print(summary(cpi.cad.m.ret.fit.log))

c1 <-1
#### Test Histogram and Density functions of the Canada CPI Forecast Prices
while (c1 < n) 
{
	price.fc.temp <- forecast.ts.cad[,c1]
	cad.cpi.fc.returns <- returns(price.fc.temp)
	### Fitting WTI Monthly Returns Distribution
	cad.cpi.fc.returns.num <- as.numeric(cad.cpi.fc.returns)*100
	cad.cpi.fc.returns.fit.norm <- fitdist(cad.cpi.fc.returns.num, distr= "logis") #Fits Canada CPI returns with Logistic Distribution
	print(summary(cad.cpi.fc.returns.fit.norm))
	dev.new(); plot(cad.cpi.fc.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
	title(paste("Gaussian ",c1,sep=""))
	c1 <- c1 + trunc(n/10)
}

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/PDF R-Reports/50 Years")
pdf(file="Canada CPI Volatility Report.pdf")
#Plot of Historical Canada CPI
plot(cpi.cad.m.last.index,main="Canada Historical CPI",xlab="",ylab=paste("Canada CPI: Indexed to ",index.year.cpi.cad,sep=""))
#Distribution of CPI returns
plot(cpi.cad.m.ret.fit.log, breaks=30)
title(main="Canada Inflation")
#Plot of Future Price Curves
plot(cpi.cad.m.hist.fc[,1],main="Canada Historical & Forecast CPI",xlab="",ylab=paste("Canada CPI: Indexed to ",index.year.cpi.cad,sep=""))
for (m in 2:n-1) {lines(cpi.cad.m.hist.fc[,m],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(cpi.cad.m.hist.fc[,n],col="black")
#Plot of Future Price Returns
plot(cpi.cad.m.hist.fc.ret[,1],main="Canada Historical & Forecast Inflation",xlab="",ylab="Canada Inflation [%]")
for (m in 2:n-1) {lines(cpi.cad.m.hist.fc.ret[,m],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(cpi.cad.m.hist.fc.ret[,n],col="black")
dev.off()

