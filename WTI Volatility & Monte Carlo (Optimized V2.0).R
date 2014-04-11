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
library(Quandl)
require(compiler)
enableJIT(3)


Quandl.auth("zVhoLfyQE2Zss2d73snH")

start.run <- Sys.timeDate()

# Set CPI Index Year
if (month(start.run)!=1){
cpi.base.year <- year(Sys.Date()) ##### This needs to be updated every evaluation #####
} else
{cpi.base.year <- year(Sys.Date())-1
}
cpi.base.year.start <- as.Date(paste(cpi.base.year,"-01-01",sep=""),"%Y-%m-%d")
cpi.base.year.end <- as.Date(paste(cpi.base.year,"-12-31",sep=""),"%Y-%m-%d")

# Historical data limit of WTI prices
#WTI_start_date <- "1986-01-30"
WTI_start_date <- "1960-01-01"

# Set working directory path for WTI import
pathdir.wti = "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/CSV Files/50 Years/"
setwd(pathdir.wti)
# Import WTI Historical Prices
###wti.spot <- read.csv("cl01.csv")
###wti.spot <- as.timeSeries(wti.spot[,-1])
oil.price.fred <- rev(fredSeries("OILPRICE",from=WTI_start_date,to="1999-12-31"))
wti.spot.m.dl <- Quandl("FRED/MCOILWTICO",start_date="2000-01-01",end_date=Sys.Date())
mcoilwtico <- as.timeSeries(wti.spot.m.dl[,2],wti.spot.m.dl[,1])
wti.spot.m <- rbind(mcoilwtico, oil.price.fred)

# Import CPI Historical Prices (All Items)
cpi.us <- rev(fredSeries("CPILEGSL",from="1957-01-01",to=Sys.Date())) #CPI for All Urban Consumers: All Items less Energy
cpi.us.short <- window(cpi.us,start=WTI_start_date,end=end(cpi.us))

if (month(end(wti.spot.m))==12) {
	daily.start <- as.Date(paste(year(end(wti.spot.m))+1,"-01-01",sep=""),"%Y-%m-%d")
} else {
	daily.start <- as.Date(paste(year(end(wti.spot.m)),"-",month(end(wti.spot.m))+1,"-01",sep=""),"%Y-%m-%d")
}

wti.spot.d.dl <- try(Quandl("FRED/DCOILWTICO",start_date=daily.start,end_date=Sys.Date()))
daily.end <- as.Date(paste(year(wti.spot.d.dl[1,1]),"-",month(wti.spot.d.dl[1,1]),"-01",sep=""),"%Y-%m-%d")
if (class(wti.spot.d.dl)!="try-error"){
	wti.spot.d <- as.timeSeries(wti.spot.d.dl[,2],wti.spot.d.dl[,1])
	by.d <- timeSequence(from=daily.start,to=daily.end,by="month")
	df <- as.data.frame(matrix(nrow=length(by.d),ncol=2))
	for (i in 1:length(by.d)) {
		if (month(end(by.d[i]))==12) {
			by.end <- as.Date(paste(year(by.d[i])+1,"-01-01",sep=""),"%Y-%m-%d")
		} else {
			by.end <- as.Date(paste(year(by.d[i]),"-",month(by.d[i])+1,"-01",sep=""),"%Y-%m-%d")
		}
		df[i,1] <- as.character(by.d[i])
		month.win <- window(wti.spot.d,start=by.d[i],end=by.end)
		df[i,2] <- mean(as.numeric(month.win))}
	wti.spot.d.m <- rev(as.timeSeries(df))
	wti.spot.m <- rbind(wti.spot.d.m,wti.spot.m)
}
colnames(wti.spot.m) <- c("Price")

# Renormalize the CPI Indices to CPI Index Year (cpi.base.year)
cpi.us.base.year.mean <- mean(window(cpi.us.short,start=cpi.base.year.start,end=cpi.base.year.end))
cpi.us.real <- cpi.us.short/cpi.us.base.year.mean*100
dev.new();par(mfrow=c(2,1))
plot(cpi.us.short,type="l",format="%Y",main="US CPI, 2002 Base")
plot(cpi.us.real,type="l",format="%Y",main=paste("CDN US, ",cpi.base.year," Base",sep=""))

# Calculate Real prices of the WTI prices beyond the CPI index (estimate with most recent annual inflation)
annual.infl <- (as.numeric(cpi.us.real[end(cpi.us.real)])-as.numeric(cpi.us.real[as.Date(paste(year(end(cpi.us.real))-1,"-",month(end(cpi.us.real)),"-",day(end(cpi.us.real)),sep=""),,"%Y-%m-%d")]))/100
if (class(wti.spot.d.m)!="try-error"){
	wti.spot.d.m.real <- rev(wti.spot.d.m)
	wti.spot.d.m.rev <- rev(wti.spot.d.m)
	for (i in 1:length(wti.spot.d.m[,1])) {
		wti.spot.d.m.real[i,1] <- wti.spot.d.m.rev[i,1]/(as.numeric(cpi.us.real[end(cpi.us.real)])*((1+((1+annual.infl)^(1/12)-1))^i))*100}
	wti.spot.d.m.real <- rev(wti.spot.d.m.real)
}

# Resize data to only consider actual historical real prices
wti.spot.m.nom <- window(wti.spot.m,start=WTI_start_date,end=end(cpi.us.real)) # matches the time window to CPI
dev.new(); plot(wti.spot.m.nom,type="l",format="%Y",col="red",main="Historical WTI Nominal Prices ($US)",xlab="Time",ylab="$US")

# Calculate Real Prices
wti.spot.m.real <- wti.spot.m.nom
cpi.us.real.m <- window(cpi.us.real,start=start(wti.spot.m.nom),end=end(wti.spot.m.nom))
wti.spot.m.real <- wti.spot.m.nom/cpi.us.real.m*100
if (class(wti.spot.d.m)!="try-error"){wti.spot.m.real <- rbind(wti.spot.d.m.real,wti.spot.m.real)}
dev.new(); plot(wti.spot.m.real,type="l",format="%Y",col="red",main="Historical WTI Real Prices ($US)",xlab="Time",ylab="$US")

#### Plot Histogram and Density functions of the WTI Real Prices
wti.m.real.returns <- returns(wti.spot.m.real)
### Fitting WTI Monthly Returns Distribution
wti.m.real.returns.num <- as.numeric(wti.m.real.returns)
wti.m.real.returns.fit.norm <- fitdist(wti.m.real.returns.num, "norm") #Fits WTI returns with Gaussian (Normal) Distribution
wti.m.real.returns.fit.logis <- fitdist(wti.m.real.returns.num, "logis") #Fits WTI returns with Logistic Distribution
#wti.m.real.returns.fit.cauchy <- fitdist(wti.m.real.returns.num, "cauchy") #Fits WTI returns with Cauchy Distribution
summary(wti.m.real.returns.fit.norm)
#summary(wti.m.real.returns.fit.logis)
#summary(wti.m.real.returns.fit.cauchy)
#dev.new(); plot(wti.m.real.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
#title("Gaussian")
#dev.new(); plot(wti.m.real.returns.fit.logis,breaks=30) ### Model with Logistic Distribution (better than Gaussian)
#title("Logistic")
#dev.new(); #plot(wti.m.real.returns.fit.cauchy,breaks=30) ### Model with Cauchy Distribution (not a good model)
#title("Cauchy")

#### Variance Ratio Test
wti.real.vrtest <- VR.minus.1(returns(wti.spot.m.real), seq(3,40*12,3))
n <- which.min((wti.real.vrtest$VR.kvec+1)[2:length(wti.real.vrtest$VR.kvec)])
wti.real.vrtest2 <- -wti.real.vrtest$VR.kvec+1
if (n > 2) {wti.real.vrtest2[1:n] <- wti.real.vrtest$VR.kvec[1:n]+1}
dev.new()
plot(wti.real.vrtest$Holding.Periods, wti.real.vrtest2, ylim=c(0,1.5), xlab="Time Difference (Months)", ylab="Variance Ratio", type="l")
for (i in 1:length(wti.real.vrtest2)) {
	if (wti.real.vrtest2[i]<=0.05) { ##### Assumes Variance Ratio needs confidence interval of 95%
		variance.ratio.loc <- i
		break
	}
}

variance.ratio.months <- wti.real.vrtest$Holding.Periods[variance.ratio.loc]

#### Calculate an OU Model using the Calculated Mean Reversion Time
ln.wti.spot.m.real <- log(removeNA(wti.spot.m.real))
print(adf.test(as.numeric(ln.wti.spot.m.real))$statistic < -1.931)
if (adf.test(as.numeric(ln.wti.spot.m.real))$statistic < -1.931) {
   lm.wti.m.real <- lm(ln.wti.spot.m.real ~ lag(ln.wti.spot.m.real,k=1))
   mu.wti.real <- exp(coefficients(lm.wti.m.real)[1]/(1-coefficients(lm.wti.m.real)[2]))
   lambda.wti.real <- -log(coefficients(lm.wti.m.real)[2])/(1/12)
   sigma_0.wti.real <- sqrt(anova(lm.wti.m.real)$"Mean Sq"[2])
   sigma.wti.real <- sigma_0.wti.real*sqrt(-2*log(coefficients(lm.wti.m.real)[2])/((1-coefficients(lm.wti.m.real)[2]^2)*(1/12)))
} else if (adf.test(as.numeric(ln.wti.spot.m.real))$p.value<=0.1) {
   lm.wti.m.real <- lm(ln.wti.spot.m.real ~ lag(ln.wti.spot.m.real,k=1))
   mu.wti.real <- exp(coefficients(lm.wti.m.real)[1]/(1-coefficients(lm.wti.m.real)[2]))
   lambda.wti.real <- -log(coefficients(lm.wti.m.real)[2])/(1/12)
   sigma_0.wti.real <- sqrt(anova(lm.wti.m.real)$"Mean Sq"[2])
   sigma.wti.real <- sigma_0.wti.real*sqrt(-2*log(coefficients(lm.wti.m.real)[2])/((1-coefficients(lm.wti.m.real)[2]^2)*(1/12)))
} else {
   sigma.wti.real <- NA
}

#### Construct an Ornstein-Uhlenbeck (OU) Mean Reversion Time Series
date1 <- start(ln.wti.spot.m.real)
########## Mean Reversion Time for WTI is 4.5 Years
date2 <- date1 + 4.5*365.242198781*24*60*60
##########
mu.roll <- 0
lambda.roll <- 0
sigma.roll <- 0
moving.avg <- 0
date.parameters <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
date.ma <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
j <- 1
h <- 1
pc <- .20 #Band within Mean Reversion Values are Accepted (+/-20% of Moving Average)
# Calculate mu, lambda, sigma on a monthly 4 year average
while(as.timeDate(as.Date(paste(year(date2),"-",month(date2),"-",day(date2),sep=""))) <= end(ln.wti.spot.m.real)) {
	temp <- window(ln.wti.spot.m.real, date1, date2)
	moving.avg[h] <- mean(window(wti.spot.m.real, date1, date2))
	date.ma[h] <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
	if (adf.test(as.numeric(temp))$statistic < -1.931)
	{
		lm.wti.m.real.temp <- lm(temp ~ lag(temp,k=1))
		mu.temp <- exp(coefficients(lm.wti.m.real.temp)[1]/(1-coefficients(lm.wti.m.real.temp)[2]))
		lambda.temp <- -log(coefficients(lm.wti.m.real.temp)[2])/(1/12)
		sigma.temp <- sigma_0.wti.real*sqrt(-2*log(coefficients(lm.wti.m.real.temp)[2])/((1-coefficients(lm.wti.m.real.temp)[2]^2)*(1/12)))
		date.temp <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
		if (as.numeric(mu.temp) < moving.avg[h]*(1+pc) & as.numeric(mu.temp) > moving.avg[h]*(1-pc)) {
			mu.roll[j] <- mu.temp; lambda.roll[j] <- lambda.temp; sigma.roll[j] <- sigma.temp
			date.parameters[j] <- date.temp; j <- j+1}
	} else if (adf.test(as.numeric(temp))$p.value<=0.1) 
	{
		lm.wti.m.real.temp <- lm(temp ~ lag(temp,k=1))
		mu.temp <- exp(coefficients(lm.wti.m.real.temp)[1]/(1-coefficients(lm.wti.m.real.temp)[2]))
		lambda.temp <- -log(coefficients(lm.wti.m.real.temp)[2])/(1/12)
		sigma.temp <- sigma_0.wti.real*sqrt(-2*log(coefficients(lm.wti.m.real.temp)[2])/((1-coefficients(lm.wti.m.real.temp)[2]^2)*(1/12)))
		date.temp <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
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
plot(wti.spot.m.real,ylim=c(0,150))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)
dev.new()
plot(window(wti.spot.m.real,start="2008-01-01",end=end(wti.spot.m.real)),ylim=c(0,150))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)

#### Forecast out 1 time period -> 1 Month
delta.t <- 1
price.t <- wti.spot.m$Price[1]
last.price.date <-  end(wti.spot.m)

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
	price.plus1.step <- (1-exp(-lambda*dt))*mu + exp(-lambda*dt)*price + sigma*mu/3.55*sqrt((1-exp(-2*lambda*dt))/(2*lambda))*b.motion
	return(price.plus1.step)
} ##################################

#### Calculate the 1st Forecast Month Price based on historical data
price.fc.matrix <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
for (i in 1:n) {
	price.fc.matrix[1,i] <- price.fc.plus1(price.t,as.numeric(mu.ts[end(mu.ts)]),as.numeric(lambda.wti.real),
		as.numeric(sigma.wti.real),brownian.motion[1,i], delta.t)
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
	price.hist.fc.list[[i]] <- rbind(rev(price.fc.ts[1,i]),wti.spot.m.real)
	ln.price.hist.fc.list[[i]] <- as.data.frame(matrix(nrow=1,ncol=1))
	ln.price.hist.fc.list[[i]] <- rbind(rev(ln.price.fc.ts[1,i]),ln.wti.spot.m.real)
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
	# Combine log prices of the Real WTI prices, prices between the end of the CPI series and the end of the WTI series
	# (prices cut off by CPI data are adjusted to the most recent CPI value)
	# and the newly forecasted prices
	temp <- window(ln.price.hist.fc.list[[i]], date1, date2)
	moving.avg.fc[h] <- mean(window(price.hist.fc.list[[i]], date1, date2))
	if (adf.test(as.numeric(temp))$statistic < -1.931)
	{
		lm.temp <- lm(temp ~ lag(temp,k=1))
		mu.fc.temp <- exp(coefficients(lm.temp)[1]/(1-coefficients(lm.temp)[2]))
		lambda.fc.temp <- -log(coefficients(lm.temp)[2])/(1/12)
		sigma.fc.temp <- sigma_0.wti.real*sqrt(-2*log(coefficients(lm.temp)[2])/((1-coefficients(lm.temp)[2]^2)*(1/12)))
		dates.fc.temp <- as.character(as.Date(date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2),format="%Y-%m-%d"))
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
		sigma.fc.temp <- sigma_0.wti.real*sqrt(-2*log(coefficients(lm.temp)[2])/((1-coefficients(lm.temp)[2]^2)*(1/12)))
		dates.fc.temp <- as.character(as.Date(date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2),format="%Y-%m-%d"))
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
	price.fc.ts.temp <- price.fc.plus1(as.numeric(price.hist.fc.list[[i]][1]),as.numeric(mu.fc.ts.list[[i]][end(mu.fc.ts.list[[i]])]), as.numeric(lambda.wti.real),as.numeric(sigma.wti.real),b.motion.ts[j,i],delta.t)
	ln.price.fc.ts.temp <- log(price.fc.ts.temp)
	price.hist.fc.list[[i]] <- rbind(price.fc.ts.temp, price.hist.fc.list[[i]])
	colnames(price.hist.fc.list[[i]]) <- paste("WTI FC ",i,sep="")
	ln.price.hist.fc.list[[i]] <- rbind(price.fc.ts.temp, ln.price.hist.fc.list[[i]])
	colnames(ln.price.hist.fc.list[[i]]) <- paste("ln(WTI FC ",i,")",sep="")
	}
}

color.palate <- c("blue","green","orange","pink","red","purple","thistle","turquoise","grey","brown")
dev.new()
plot(price.hist.fc.list[[1]],xlim = c(start(price.hist.fc.list[[1]]),end(price.hist.fc.list[[1]])),ylim=c(0,150))
points(mu.ts,col="blue",pch=1)
for (m in 2:n-1) {lines(price.hist.fc.list[[m]],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(price.hist.fc.list[[n]],col="black")

summary(wti.m.real.returns.fit.norm)
dev.new(); plot(wti.m.real.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
title("Gaussian")

c1 <-1
#### Test Histogram and Density functions of the WTI Forecast Prices
while (c1 < n) 
{
	price.fc.temp <- window(price.hist.fc.list[[c1]],start=end(wti.spot.m.real),end=end(price.hist.fc.list[[c1]]))
	wti.fc.returns <- returns(price.fc.temp)
	### Fitting WTI Monthly Returns Distribution
	wti.fc.returns.num <- as.numeric(wti.fc.returns)
	wti.fc.returns.fit.norm <- fitdist(wti.fc.returns.num, "norm") #Fits WTI returns with Gaussian (Normal) Distribution
	print(summary(wti.fc.returns.fit.norm))
	dev.new(); plot(wti.fc.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
	title(paste("Gaussian ",c1,sep=""))
	c1 <- c1 + trunc(n/10)
}

end.run <- Sys.timeDate()
print(start.run)
print(end.run)


setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/CSV Files/50 Years")
write.csv(as.data.frame(as.list(price.hist.fc.list)),file=paste("WTI Monte Carlo - ",n," Price Series (",as.character(end(wti.spot.d)),").csv",sep=""))
write.csv(as.data.frame(as.list(price.hist.fc.list)),file=paste("WTI Monte Carlo - ",n," Price Series (most recent).csv",sep=""))

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/PDF R-Reports/50 Years")
pdf(file="WTI Volatility Report.pdf")
#Plot of Nominal Historical WTI
plot(wti.spot.m.nom,type="l",format="%Y",col="red",main="Historical WTI Nominal Prices ($US)",xlab="Time",ylab="$US")
#Plot US CPI
plot(cpi.us.short,type="l",format="%Y",main="US CPI, 2002 Base")
#Plot of Real Historical WTI with Mean Reversion
plot(wti.spot.m.real,ylim=c(0,150))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)
#Plot of Real Historical WTI with Mean Reversion > 2008
plot(window(wti.spot.m.real,start="2008-01-01",end=end(wti.spot.m.real)),ylim=c(0,150))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)
#Distribution of WTI Returns
plot(wti.m.real.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
title("Gaussian")
#Report Average Historical Variance etc.
#plot.new()
#mtext(paste("Historical Mean = ",mu.wti.real,sep=""))
#mtext(paste("Historical Variance = ",sigma.wti.real,sep=""))
#mtext(paste("Historical Half-Life = ",lambda.wti.real,sep=""))
#Plot of Future Price Curves
plot(price.hist.fc.list[[1]],xlim = c(start(price.hist.fc.list[[1]]),end(price.hist.fc.list[[1]])),ylim=c(0,150))
points(mu.ts,col="blue",pch=1)
for (m in 2:n-1) {lines(price.hist.fc.list[[m]],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(price.hist.fc.list[[n]],col="black")
dev.off()


