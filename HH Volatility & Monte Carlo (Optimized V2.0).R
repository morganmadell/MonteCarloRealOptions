library(timeSeries)
library(zoo)
library(quantmod)
library(fImport)
library(lubridate)
library(vrtest)
library(tseries)
library(MASS)
library(fitdistrplus)
library(fUnitRoots)
library(stringr)
library(rpart)
library(foreach)
require(compiler)
library(Quandl)
enableJIT(3)

start.run <- Sys.timeDate()

# Set CPI Index Year
if (month(start.run)!=1){
cpi.base.year <- year(Sys.Date()) ##### This needs to be updated every evaluation #####
} else
{cpi.base.year <- year(Sys.Date())-1
}
cpi.base.year.start <- as.Date(paste(cpi.base.year,"-01-01",sep=""),"%Y-%m-%d")
cpi.base.year.end <- as.Date(paste(cpi.base.year,"-12-31",sep=""),"%Y-%m-%d")

# Historical data limit of HH prices
HH_start_date <- "1993-11-01"

# Set working directory path for HH import
pathdir.hh <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/CSV Files/50 Years/"
setwd(pathdir.hh)
# Import HH Historical Prices
###hh.spot <- read.csv("cl01.csv")
###hh.spot <- as.timeSeries(hh.spot[,-1])
hh.spot.m <- rev(fredSeries("GASPRICE",from=HH_start_date,to=Sys.Date()))

# Import CPI Historical Prices (All Items)
cpi.us <- rev(fredSeries("CPILEGSL",from="1957-01-01",to=Sys.Date())) #CPI for All Urban Consumers: All Items less Energy
cpi.us.short <- window(cpi.us,start=HH_start_date,end=end(cpi.us))

if (month(end(hh.spot.m))==12) {
	daily.start <- as.Date(paste(year(end(hh.spot.m))+1,"-01-01",sep=""),"%Y-%m-%d")
} else {
	daily.start <- as.Date(paste(year(end(hh.spot.m)),"-",month(end(hh.spot.m))+1,"-01",sep=""),"%Y-%m-%d")
}

Quandl.auth("zVhoLfyQE2Zss2d73snH")
hh.spot.d <- try(Quandl("WSJ/NG_HH",start_date=daily.start,end_date=Sys.Date()))
daily.end <- as.Date(paste(year(hh.spot.d[1,1]),"-",month(hh.spot.d[1,1]),"-01",sep=""),"%Y-%m-%d")
if (class(hh.spot.d)!="try-error"){
	by.d <- timeSequence(from=daily.start,to=daily.end,by="month")
	df <- as.data.frame(matrix(nrow=1,ncol=2))
	df[1,1] <- as.character(by.d); df[1,2] <- mean(hh.spot.d[,2])
	hh.spot.d.m <- as.timeSeries(df)
	#hh.spot.d.m <- timeSeries(mean(hh.spot.d),by=by.d)### This function stopped working...
	hh.spot.m <- rbind(hh.spot.d.m,hh.spot.m)}
colnames(hh.spot.m) <- c("Price")

# Renormalize the CPI Indices to CPI Index Year (cpi.base.year)
cpi.us.base.year.mean <- mean(window(cpi.us.short,start=cpi.base.year.start,end=cpi.base.year.end))
cpi.us.real <- cpi.us.short/cpi.us.base.year.mean*100
dev.new();par(mfrow=c(2,1))
plot(cpi.us.short,type="l",format="%Y",main="US CPI, 2002 Base")
plot(cpi.us.real,type="l",format="%Y",main=paste("CDN US, ",cpi.base.year," Base",sep=""))

# Calculate the difference in months between the end of the daily data and the end of the CPI data
if ((month(end(hh.spot.d.m))-month(end(cpi.us.real)))<0){
	daily.diff.mths <- 12+(month(end(hh.spot.d.m))-month(end(cpi.us.real)))
} else {daily.diff.mths <- (month(end(hh.spot.d.m))-month(end(cpi.us.real)))}

# Calculate Real Prices
hh.spot.m.real <- hh.spot.m.nom

# Calculate Nominal prices of the HH prices beyond the CPI index (estimate of the most recent annual inflation)
annual.infl <- (as.numeric(cpi.us.real[end(cpi.us.real)])-as.numeric(cpi.us.real[as.Date(paste(year(end(cpi.us.real))-1,"-",month(end(cpi.us.real)),"-",day(end(cpi.us.real)),sep=""),,"%Y-%m-%d")]))/100
if (class(hh.spot.d)!="try-error"){
	hh.spot.d.m.real <- hh.spot.d.m/(as.numeric(cpi.us.real[end(cpi.us.real)])*((1+((1+annual.infl)^(1/12)-1))^daily.diff.mths))*100}

# Resize data to only consider actual historical real prices
hh.spot.m.nom <- window(hh.spot.m,start=HH_start_date,end=end(cpi.us.real)) # matches the time window to CPI

# Calculate the difference in months between the end of the monthly data and the end of the CPI data
if ((month(end(hh.spot.m))-month(end(cpi.us.real)))<0){
	monthly.diff.mths <- 12+(month(end(hh.spot.d.m))-month(end(cpi.us.real)))-1
} else {monthly.diff.mths <- (month(end(hh.spot.d.m))-month(end(cpi.us.real)))-1}
if (monthly.diff.mths>0) {
	if (month(end(cpi.us.real))!=12){
		m.lmths.start <- as.Date(paste(year(end(cpi.us.real)),"-",month(end(cpi.us.real))+1,"-01",sep=""))
		} else {m.lmths.start <- as.Date(paste(year(end(cpi.us.real))+1,"-01-01",sep=""))}
	hh.spot.m.lmths.nom <- window(hh.spot.m,start=m.lmths.start,end=end(hh.spot.m[2,]))
	hh.spot.m.lmths.real <- hh.spot.m.lmths.nom/(as.numeric(cpi.us.real[end(cpi.us.real)])*((1+((1+annual.infl)^(1/12)-1))^monthly.diff.mths))*100}

hh.spot.m.real <- hh.spot.m.nom/cpi.us.real*100

if (monthly.diff.mths>0) {hh.spot.m.real <- rbind(hh.spot.m.lmths.real,hh.spot.m.real)}
if (class(hh.spot.d)!="try-error"){hh.spot.m.real <- rbind(hh.spot.d.m.real,hh.spot.m.real)}

dev.new(); plot(hh.spot.m.nom,type="l",format="%Y",col="red",main="Historical HH Nominal Prices ($US)",xlab="Time",ylab="$US")
dev.new(); plot(hh.spot.m.real,type="l",format="%Y",col="red",main="Historical HH Real Prices ($US)",xlab="Time",ylab="$US")

#### Plot Histogram and Density functions of the HH Real Prices
hh.m.real.returns <- returns(hh.spot.m.real)
### Fitting HH Monthly Returns Distribution
hh.m.real.returns.num <- as.numeric(hh.m.real.returns)
hh.m.real.returns.fit.norm <- fitdist(hh.m.real.returns.num, "norm") #Fits HH returns with Gaussian (Normal) Distribution
hh.m.real.returns.fit.logis <- fitdist(hh.m.real.returns.num, "logis") #Fits HH returns with Logistic Distribution
#hh.m.real.returns.fit.cauchy <- fitdist(hh.m.real.returns.num, "cauchy") #Fits HH returns with Cauchy Distribution
summary(hh.m.real.returns.fit.norm)
#summary(hh.m.real.returns.fit.logis)
#summary(hh.m.real.returns.fit.cauchy)
dev.new(); plot(hh.m.real.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
title("Gaussian")
#dev.new(); plot(hh.m.real.returns.fit.logis,breaks=30) ### Model with Logistic Distribution (better than Gaussian)
#title("Logistic")
#dev.new(); #plot(hh.m.real.returns.fit.cauchy,breaks=30) ### Model with Cauchy Distribution (not a good model)
#title("Cauchy")

#### Variance Ratio Test
hh.real.vrtest <- VR.minus.1(returns(hh.spot.m.real), seq(3,40*12,3))
n <- which.min((hh.real.vrtest$VR.kvec+1)[2:length(hh.real.vrtest$VR.kvec)])
hh.real.vrtest2 <- -hh.real.vrtest$VR.kvec+1
if (n > 2) {hh.real.vrtest2[1:n] <- hh.real.vrtest$VR.kvec[1:n]+1}
dev.new()
plot(hh.real.vrtest$Holding.Periods, hh.real.vrtest2, ylim=c(0,1.5), xlab="Time Difference (Months)", ylab="Variance Ratio", type="l")
for (i in 1:length(hh.real.vrtest2)) {
	if (hh.real.vrtest2[i]<=0.05) { ##### Assumes Variance Ratio needs confidence interval of 95%
		variance.ratio.loc <- i
		break
	}
}

variance.ratio.months <- hh.real.vrtest$Holding.Periods[variance.ratio.loc]

#### Calculate an OU Model using the Calculated Mean Reversion Time
ln.hh.spot.m.real <- log(removeNA(hh.spot.m.real))
if (adf.test(as.numeric(ln.hh.spot.m.real))$statistic < -1.931) {
   lm.hh.m.real <- lm(ln.hh.spot.m.real ~ lag(ln.hh.spot.m.real,k=1))
   mu.hh.real <- exp(coefficients(lm.hh.m.real)[1]/(1-coefficients(lm.hh.m.real)[2]))
   lambda.hh.real <- -log(coefficients(lm.hh.m.real)[2])/(1/12)
   sigma_0.hh.real <- sqrt(anova(lm.hh.m.real)$"Mean Sq"[2])
   sigma.hh.real <- sigma_0.hh.real*sqrt(-2*log(coefficients(lm.hh.m.real)[2])/((1-coefficients(lm.hh.m.real)[2]^2)*(1/12)))
} else if (adf.test(as.numeric(ln.hh.spot.m.real))$p.value<=0.1) {
   lm.hh.m.real <- lm(ln.hh.spot.m.real ~ lag(ln.hh.spot.m.real,k=1))
   mu.hh.real <- exp(coefficients(lm.hh.m.real)[1]/(1-coefficients(lm.hh.m.real)[2]))
   lambda.hh.real <- -log(coefficients(lm.hh.m.real)[2])/(1/12)
   sigma_0.hh.real <- sqrt(anova(lm.hh.m.real)$"Mean Sq"[2])
   sigma.hh.real <- sigma_0.hh.real*sqrt(-2*log(coefficients(lm.hh.m.real)[2])/((1-coefficients(lm.hh.m.real)[2]^2)*(1/12)))
} else {
   sigma.hh.real <- NA
}

#### Construct an Ornstein-Uhlenbeck (OU) Mean Reversion Time Series
date1 <- start(ln.hh.spot.m.real)
########## Mean Reversion Time for HH is 1.6 Years
date2 <- date1 + 9.0*365.242198781*24*60*60
##########
mu.roll <- 0
lambda.roll <- 0
sigma.roll <- 0
moving.avg <- 0
date.parameters <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
date.ma <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
j <- 1
h <- 1
pc1 <- .2 #Band within Mean Reversion Values are Accepted (+/-20% of Moving Average) [Above $7.50/mcf]
pc2 <- .5 #Band within Mean Reversion Values are Accepted (+/-50% of Moving Average) [Below $7.50/mcf]
# Calculate mu, lambda, sigma on a monthly 4 year average
while(as.timeDate(as.Date(paste(year(date2),"-",month(date2),"-",day(date2),sep=""))) <= end(ln.hh.spot.m.real)) {
	temp <- window(ln.hh.spot.m.real, date1, date2)
	moving.avg[h] <- mean(window(hh.spot.m.real, date1, date2))
	date.ma[h] <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
	if (adf.test(as.numeric(temp))$statistic < -1.931)
	{
		lm.hh.m.real.temp <- lm(temp ~ lag(temp,k=1))
		mu.temp <- exp(coefficients(lm.hh.m.real.temp)[1]/(1-coefficients(lm.hh.m.real.temp)[2]))
		lambda.temp <- -log(coefficients(lm.hh.m.real.temp)[2])/(1/12)
		sigma.temp <- sigma_0.hh.real*sqrt(-2*log(coefficients(lm.hh.m.real.temp)[2])/((1-coefficients(lm.hh.m.real.temp)[2]^2)*(1/12)))
		date.temp <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
		if (mu.temp>7.5){pc <- pc1} else {pc <- pc2}
		if (as.numeric(mu.temp) < moving.avg[h]*(1+pc) & as.numeric(mu.temp) > moving.avg[h]*(1-pc)) {
			mu.roll[j] <- mu.temp; lambda.roll[j] <- lambda.temp; sigma.roll[j] <- sigma.temp
			date.parameters[j] <- date.temp; j <- j+1}
	} else if (adf.test(as.numeric(temp))$p.value<=0.1) 
	{
		lm.hh.m.real.temp <- lm(temp ~ lag(temp,k=1))
		mu.temp <- exp(coefficients(lm.hh.m.real.temp)[1]/(1-coefficients(lm.hh.m.real.temp)[2]))
		lambda.temp <- -log(coefficients(lm.hh.m.real.temp)[2])/(1/12)
		sigma.temp <- sigma_0.hh.real*sqrt(-2*log(coefficients(lm.hh.m.real.temp)[2])/((1-coefficients(lm.hh.m.real.temp)[2]^2)*(1/12)))
		date.temp <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
		if (mu.temp>7.5){pc <- pc1} else {pc <- pc2}
		if (as.numeric(mu.temp) < moving.avg[h]*(1+pc) & as.numeric(mu.temp) > moving.avg[h]*(1-pc)) {
			mu.roll[j] <- mu.temp; lambda.roll[j] <- lambda.temp; sigma.roll[j] <- sigma.temp
			date.parameters[j] <- date.temp; j <- j+1}
	}	
	date1 <- date1 + 365.242198781/12*24*60*60
	date2 <- date2 + 365.242198781/12*24*60*60
	h <- h+1
}
by <- timeSequence(from=start(date.parameters), to=end(date.parameters), by="month")
by.ma <- timeSequence(from=start(date.ma), to=end(date.ma), by="month")
mu.ts <- aggregate(timeSeries(mu.roll,date.parameters),by,mean)
lambda.ts <- aggregate(timeSeries(lambda.roll,date.parameters),by,mean)
sigma.ts <- aggregate(timeSeries(sigma.roll,date.parameters),by,mean)
moving.average.ts <- timeSeries(moving.avg,date.ma)
dev.new()
plot(hh.spot.m.real,ylim=c(0,15))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)
dev.new()
plot(window(hh.spot.m.real,start="2008-01-01",end=end(hh.spot.m.real)),ylim=c(0,15))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)

####################################################################################################################################
#### Forecast out 1 time period -> 1 Month
delta.t <- 1
price.t <- hh.spot.m$Price[1]
last.price.date <-  end(hh.spot.m)

########## Number of runs
n <- 1000 #
##########
fc.periods <- 12*50 # 50 years
if (month(last.price.date)==12) {
	fc.ts.time.seq.start <- as.Date(paste(year(last.price.date)+1,"-01-01",sep=""),"%Y-%m-%d")
} else {
	fc.ts.time.seq.start <- as.Date(paste(year(last.price.date),"-",month(last.price.date)+1,"-01",sep=""),"%Y-%m-%d")
}
# Create an empty forecast timeSeries
forecast.ts <- as.data.frame(timeSequence(fc.ts.time.seq.start, length.out=fc.periods,by="month"))
colnames(forecast.ts) <- c("Date")

# Create a data.frame for the Brownian Motion variable
brownian.motion <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
#### Mean is centred around historical mean returns
brownian.motion[1,] <- rnorm(n, mean=0, sd=1) # Fill the first forecast month brownian motion
b.motion.ts <- as.timeSeries(cbind(forecast.ts$Date,brownian.motion))

################################## A function that calculates the Price at t+1
price.fc.plus1 <- function(price, mu, lambda, sigma, b.motion, dt) {
	#### Sigma is modified by mu/3.55 to fit historical standard deviation
	##### lambda.fc <- 0.2, b.motion factor = 4.0
	lambda.fc <- sigma.hh.real*1.0
	price.plus1.step <- (1-exp(-lambda*dt*lambda.fc))*mu + exp(-lambda*dt*lambda.fc)*price + sigma*mu/3.52*sqrt((1-exp(-2*lambda*dt*lambda.fc))/(2*lambda*lambda.fc))*b.motion
	return(price.plus1.step)
} ##################################

#### Calculate the 1st Forecast Month Price based on historical data
price.fc.matrix <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
for (i in 1:n) {
	price.fc.matrix[1,i] <- price.fc.plus1(price.t,as.numeric(mu.ts[end(mu.ts)]),as.numeric(lambda.hh.real),
		as.numeric(sigma.hh.real),brownian.motion[1,i], delta.t)
}
price.fc.ts <- as.timeSeries(cbind(forecast.ts$Date,price.fc.matrix))
ln.price.fc.ts <- log(price.fc.ts)

date1 <- date1 + 365.242198781/12*24*60*60
date2 <- date2 + 365.242198781/12*24*60*60

mu.fc.roll <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
lambda.fc.roll <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
sigma.fc.roll <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
dates.fc <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
for (i in 1:n) {
	dates.fc[1,i] <- as.character(as.Date(date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2),format="%Y-%m-%d"))
}

mu.fc.ts.list <- vector("pairlist",n)
lambda.fc.ts.list <- vector("pairlist",n)
sigma.fc.ts.list <- vector("pairlist",n)
price.hist.fc.list <- vector("pairlist",n)
ln.price.hist.fc.list <- vector("pairlist",n)
for (i in 1:n) {
	mu.fc.ts.list[[i]] <- as.data.frame(matrix(nrow=1,ncol=1))
	mu.fc.ts.list[[i]] <- mu.ts
	lambda.fc.ts.list[[i]] <- as.data.frame(matrix(nrow=1,ncol=1))
	lambda.fc.ts.list[[i]] <- lambda.ts
	sigma.fc.ts.list[[i]] <- as.data.frame(matrix(nrow=1,ncol=1))
	sigma.fc.ts.list[[i]] <- sigma.ts
	price.hist.fc.list[[i]] <- as.data.frame(matrix(nrow=1,ncol=1))
	ln.price.hist.fc.list[[i]] <- as.data.frame(matrix(nrow=1,ncol=1))
	price.hist.fc.list[[i]] <- rbind(rev(price.fc.ts[1,i]),hh.spot.m.real)
	ln.price.hist.fc.list[[i]] <- rbind(rev(ln.price.fc.ts[1,i]),ln.hh.spot.m.real)
}

moving.avg.fc <- 0
price.fc.v.temp <- 0
ln.price.v.ts.temp <- 0
date.fc.temp <- 0
price.fc.ts.temp <-0
ln.price.fc.ts.temp <- 0
h <-1
k.count <- 1

for (j in 2:fc.periods) {
date1 <- date1 + 365.242198781/12*24*60*60
date2 <- date2 + 365.242198781/12*24*60*60
for (i in 1:n) {
	# Combine log prices of the Real HH prices, prices between the end of the CPI series and the end of the HH series
	# (prices cut off by CPI data are adjusted to the most recent CPI value)
	# and the newly forecasted prices
	temp <- window(ln.price.hist.fc.list[[i]], date1, date2)
	moving.avg.fc[h] <- mean(window(price.hist.fc.list[[i]], date1, date2))
	if (adf.test(as.numeric(temp))$statistic < -1.931)
	{
		lm.temp <- lm(temp ~ lag(temp,k=1))
		mu.fc.temp <- exp(coefficients(lm.temp)[1]/(1-coefficients(lm.temp)[2]))
		lambda.fc.temp <- -log(coefficients(lm.temp)[2])/(1/12)
		sigma.fc.temp <- sigma_0.hh.real*sqrt(-2*log(coefficients(lm.temp)[2])/((1-coefficients(lm.temp)[2]^2)*(1/12)))
		dates.fc.temp <- as.character(as.Date(date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2),format="%Y-%m-%d"))
		if (mu.fc.temp>7.5){pc <- pc1} else {pc <- pc2}
		if (as.numeric(mu.fc.temp) < moving.avg.fc[h]*(1+pc) & as.numeric(mu.fc.temp) > moving.avg.fc[h]*(1-pc)) {
			mu.fc.roll[k.count,i] <- mu.fc.temp
			lambda.fc.roll[k.count,i] <- lambda.fc.temp
			sigma.fc.roll[k.count,i] <- sigma.fc.temp
			dates.fc[k.count,i] <- dates.fc.temp
			mu.fc.temp.ts <- timeSeries(mu.fc.roll[k.count,i],dates.fc[k.count,i])
			mu.fc.hist.temp.ts <- rbind(mu.fc.ts.list[[i]],mu.fc.temp.ts)
			colnames(mu.fc.hist.temp.ts) <- c("FC Price")
			mu.fc.ts.list[[i]] <- na.omit(mu.fc.hist.temp.ts)
			#lambda.fc.temp <- timeSeries(lambda.fc.roll[k.count,i],dates.fc[k.count,i])
			#lambda.fc.temp.ts <- rbind(lambda.fc.ts.list,lambda.fc.temp)
			#lambda.fc.ts.list[[i]] <- lambda.fc.temp.ts
			#sigma.fc.temp <- timeSeries(sigma.fc.roll[k.count,i],dates.fc[k.count,i])
			#sigma.fc.temp.ts <- rbind(sigma.fc.ts.list[[i]],sigma.fc.temp)
			#sigma.fc.ts.list[[i]] <- sigma.fc.temp.ts
			k.count <- k.count + 1}
	} else if (adf.test(as.numeric(temp))$p.value<=0.1) 
	{
		lm.temp <- lm(temp ~ lag(temp,k=1))
		mu.fc.temp <- exp(coefficients(lm.temp)[1]/(1-coefficients(lm.temp)[2]))
		lambda.fc.temp <- -log(coefficients(lm.temp)[2])/(1/12)
		sigma.fc.temp <- sigma_0.hh.real*sqrt(-2*log(coefficients(lm.temp)[2])/((1-coefficients(lm.temp)[2]^2)*(1/12)))
		dates.fc.temp <- as.character(as.Date(date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2),format="%Y-%m-%d"))
		if (mu.fc.temp>7.5){pc <- pc1} else {pc <- pc2}
		if (as.numeric(mu.fc.temp) < moving.avg.fc[h]*(1+pc) & as.numeric(mu.fc.temp) > moving.avg.fc[h]*(1-pc)) {
			mu.fc.roll[k.count,i] <- mu.fc.temp
			lambda.fc.roll[k.count,i] <- lambda.fc.temp
			sigma.fc.roll[k.count,i] <- sigma.fc.temp
			dates.fc[k.count,i] <- dates.fc.temp
			mu.fc.temp.ts <- timeSeries(mu.fc.roll[k.count,i],dates.fc[k.count,i])
			mu.fc.hist.temp.ts <- rbind(mu.fc.ts.list[[i]],mu.fc.temp.ts)
			colnames(mu.fc.hist.temp.ts) <- c("FC Price")
			mu.fc.ts.list[[i]] <- na.omit(mu.fc.hist.temp.ts)
			#lambda.fc.temp <- timeSeries(lambda.fc.roll[k.count,i],dates.fc[k.count,i])
			#lambda.fc.temp.ts <- rbind(lambda.fc.ts.list,lambda.fc.temp)
			#lambda.fc.ts.list[[i]] <- lambda.fc.temp.ts
			#sigma.fc.temp <- timeSeries(sigma.fc.roll[k.count,i],dates.fc[k.count,i])
			#sigma.fc.temp.ts <- rbind(sigma.fc.ts.list[[i]],sigma.fc.temp)
			#sigma.fc.ts.list[[i]] <- sigma.fc.temp.ts
			k.count <- k.count + 1}
	}
	b.motion.ts[j,i] <- rnorm(1, mean=0, sd=1)
	price.fc.ts.temp <- price.fc.plus1(as.numeric(price.hist.fc.list[[i]][1]),as.numeric(mu.fc.ts.list[[i]][end(mu.fc.ts.list[[i]])]), as.numeric(lambda.hh.real),as.numeric(sigma.hh.real),b.motion.ts[j,i],delta.t)
	ln.price.fc.ts.temp <- log(price.fc.ts.temp)
	price.hist.fc.list[[i]] <- rbind(price.fc.ts.temp, price.hist.fc.list[[i]])
	colnames(price.hist.fc.list[[i]]) <- paste("HH FC ",i,sep="")
	ln.price.hist.fc.list[[i]] <- rbind(price.fc.ts.temp, ln.price.hist.fc.list[[i]])
	colnames(ln.price.hist.fc.list[[i]]) <- paste("ln(HH FC ",i,")",sep="")
	}
}

color.palate <- c("blue","green","orange","pink","red","purple","thistle","turquoise","grey","brown")
dev.new()
plot(price.hist.fc.list[[1]],xlim = c(start(price.hist.fc.list[[1]]),end(price.hist.fc.list[[1]])),ylim=c(0,15))
points(mu.ts,col="blue",pch=1)
for (m in 2:n-1) {lines(price.hist.fc.list[[m]],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(price.hist.fc.list[[n]],col="black")

print(window(price.hist.fc.list[[1]],start="2013-01-01",end="2015-01-01"))

summary(hh.m.real.returns.fit.norm)
dev.new(); plot(hh.m.real.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
title("Gaussian")

c1 <-1
#### Test Histogram and Density functions of the HH Forecast Prices
while (c1 < n) 
{
	price.fc.temp <- window(price.hist.fc.list[[c1]],start=end(hh.spot.m.real),end=end(price.hist.fc.list[[c1]]))
	hh.fc.returns <- returns(price.fc.temp)
	### Fitting HH Monthly Returns Distribution
	hh.fc.returns.num <- as.numeric(hh.fc.returns)
	hh.fc.returns.fit.norm <- fitdist(hh.fc.returns.num, "norm") #Fits HH returns with Gaussian (Normal) Distribution
	print(summary(hh.fc.returns.fit.norm))
	dev.new(); plot(hh.fc.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
	title(paste("Gaussian ",c1,sep=""))
	c1 <- c1 + trunc(n/10)
}

end.run <- Sys.timeDate()
print(start.run)
print(end.run)

#######################################################################################################################################################
setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/CSV Files/50 Years")
write.csv(as.data.frame(as.list(price.hist.fc.list)),file=paste("HH Monte Carlo - ",n," Price Series (",as.character(hh.spot.d[1,1]),").csv",sep=""))
write.csv(as.data.frame(as.list(price.hist.fc.list)),file=paste("HH Monte Carlo - ",n," Price Series (most recent).csv",sep=""))

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/PDF R-Reports/50 Years")
pdf(file="HH Volatility Report.pdf")
#Plot of Nominal Historical HH
plot(hh.spot.m.nom,type="l",format="%Y",col="red",main="Historical HH Nominal Prices ($US)",xlab="Time",ylab="$US")
#Plot US CPI
plot(cpi.us.short,type="l",format="%Y",main="US CPI, 2002 Base")
#Plot of Real Historical HH with Mean Reversion
plot(hh.spot.m.real,ylim=c(0,15))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)
#Plot of Real Historical HH with Mean Reversion > 2008
plot(window(hh.spot.m.real,start="2008-01-01",end=end(hh.spot.m.real)),ylim=c(0,15))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)
#Distribution of HH Returns
plot(hh.m.real.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
title("Gaussian")
#Report Average Historical Variance etc.
#plot.new()
#mtext(paste("Historical Mean = ",mu.hh.real,sep=""))
#mtext(paste("Historical Variance = ",sigma.hh.real,sep=""))
#mtext(paste("Historical Half-Life = ",lambda.hh.real,sep=""))
#Plot of Future Price Curves
plot(price.hist.fc.list[[1]],xlim = c(start(price.hist.fc.list[[1]]),end(price.hist.fc.list[[1]])),ylim=c(0,15))
points(mu.ts,col="blue",pch=1)
for (m in 2:n-1) {lines(price.hist.fc.list[[m]],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(price.hist.fc.list[[n]],col="black")
dev.off()


