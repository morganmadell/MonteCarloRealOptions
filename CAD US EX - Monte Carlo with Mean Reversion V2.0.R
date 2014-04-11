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
require(compiler)
enableJIT(3)

start.run <- Sys.timeDate()

# Historical data limit of US-Canada Exchange rates
USCADEX_start_date <- "1971-01-01"

# Import Historical US-Canada Exchange rates
cad.usd.ex.m <- rev(fredSeries("EXCAUS", from=USCADEX_start_date,to=Sys.Date()))
if (month(end(cad.usd.ex.m))==12) {
	daily.start <- as.Date(paste(year(end(cad.usd.ex.m))+1,"-01-01",sep=""),"%Y-%m-%d")
} else {
	daily.start <- as.Date(paste(year(end(cad.usd.ex.m)),"-",month(end(cad.usd.ex.m))+1,"-01",sep=""),"%Y-%m-%d")
}
if (daily.start<Sys.Date()) {cad.usd.ex.d <- rev(fredSeries("DEXCAUS", from=daily.start,to=Sys.Date()))}
if (exists("cad.usd.ex.d")) {
	by.d <- timeSequence(from=daily.start,to=daily.start,by="month")
	df <- as.data.frame(matrix(nrow=1,ncol=2))
	df[1,1] <- as.character(by.d); df[1,2] <- mean(cad.usd.ex.d)
	cad.usd.ex.d.m <- as.timeSeries(df)
	#cad.usd.ex.d.m <- timeSeries(mean(cad.usd.ex.d),by.d)
	cad.usd.ex.m <- rbind(cad.usd.ex.d.m,cad.usd.ex.m)
}
colnames(cad.usd.ex.m) <- c("CAD-USD EX")
dev.new(); plot(cad.usd.ex.m,type="l",format="%Y",col="red",main="Historical Canada-US Exchange Rates (CAD:USD)",xlab="Time",ylab="CAD:USD")

#### Plot Histogram and Density functions of the WTI Real Prices
cad.usd.ex.m.returns <- returns(cad.usd.ex.m)
### Fitting WTI Monthly Returns Distribution
cad.usd.ex.m.returns.num <- as.numeric(cad.usd.ex.m.returns)
cad.usd.ex.m.returns.fit.norm <- fitdist(cad.usd.ex.m.returns.num, "norm") #Fits USCADEXreturns with Gaussian (Normal) Distribution
cad.usd.ex.m.returns.fit.logis <- fitdist(cad.usd.ex.m.returns.num, "logis") #Fits USCADEXreturns with Logistic Distribution
#cad.usd.ex.m.returns.fit.cauchy <- fitdist(cad.usd.ex.m.returns.num, "cauchy") #Fits USCADEXreturns with Cauchy Distribution
summary(cad.usd.ex.m.returns.fit.norm)
#summary(cad.usd.ex.m.returns.fit.logis)
#summary(cad.usd.ex.m.returns.fit.cauchy)
dev.new(); plot(cad.usd.ex.m.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
title("Gaussian")
#dev.new(); plot(cad.usd.ex.m.returns.fit.logis,breaks=30) ### Model with Logistic Distribution (better than Gaussian)
#title("Logistic")
#dev.new(); #plot(cad.usd.ex.m.returns.fit.cauchy,breaks=30) ### Model with Cauchy Distribution (not a good model)
#title("Cauchy")

#### Variance Ratio Test
us.cad.ex.real.vrtest <- VR.minus.1(returns(cad.usd.ex.m), seq(3,40*12,3))
n <- which.min((us.cad.ex.real.vrtest$VR.kvec+1)[2:length(us.cad.ex.real.vrtest$VR.kvec)])
us.cad.ex.real.vrtest2 <- -us.cad.ex.real.vrtest$VR.kvec+1
if (n > 2) {us.cad.ex.real.vrtest2[1:n] <- us.cad.ex.real.vrtest$VR.kvec[1:n]+1}
dev.new()
plot(us.cad.ex.real.vrtest$Holding.Periods, us.cad.ex.real.vrtest2, ylim=c(0,2), xlab="Time Difference (Months)", ylab="Variance Ratio", type="l")
for (i in 1:length(us.cad.ex.real.vrtest2)) {
	if (us.cad.ex.real.vrtest2[i]<=0.05) { ##### Assumes Variance Ratio needs confidence interval of 95%
		variance.ratio.loc <- i
		break
	}
}

variance.ratio.months <- us.cad.ex.real.vrtest$Holding.Periods[variance.ratio.loc]

#### Calculate an OU Model using the Calculated Mean Reversion Time
ln.cad.usd.ex.m <- log(removeNA(cad.usd.ex.m))
if (adf.test(as.numeric(ln.cad.usd.ex.m))$statistic < -1.6164) {
   lm.cad.usd.ex <- lm(ln.cad.usd.ex.m ~ lag(ln.cad.usd.ex.m,k=1))
   mu.cad.usd.ex <- exp(coefficients(lm.cad.usd.ex)[1]/(1-coefficients(lm.cad.usd.ex)[2]))
   lambda.cad.usd.ex <- -log(coefficients(lm.cad.usd.ex)[2])/(1/12)
   sigma_0.cad.usd.ex <- sqrt(anova(lm.cad.usd.ex)$"Mean Sq"[2])
   sigma.cad.usd.ex <- sigma_0.cad.usd.ex*sqrt(-2*log(coefficients(lm.cad.usd.ex)[2])/((1-coefficients(lm.cad.usd.ex)[2]^2)*(1/12)))
} else if (adf.test(as.numeric(ln.cad.usd.ex.m))$p.value<=0.1) {
   lm.cad.usd.ex <- lm(ln.cad.usd.ex.m ~ lag(ln.cad.usd.ex.m,k=1))
   mu.cad.usd.ex <- exp(coefficients(lm.cad.usd.ex)[1]/(1-coefficients(lm.cad.usd.ex)[2]))
   lambda.cad.usd.ex <- -log(coefficients(lm.cad.usd.ex)[2])/(1/12)
   sigma_0.cad.usd.ex <- sqrt(anova(lm.cad.usd.ex)$"Mean Sq"[2])
   sigma.cad.usd.ex <- sigma_0.cad.usd.ex*sqrt(-2*log(coefficients(lm.cad.usd.ex)[2])/((1-coefficients(lm.cad.usd.ex)[2]^2)*(1/12)))
} else {
   sigma.cad.usd.ex <- NA
}

#### Construct an Ornstein-Uhlenbeck (OU) Mean Reversion Time Series
date1 <- start(ln.cad.usd.ex.m)
########## Mean Reversion Time for USD-CAD Exchange is 3 Years
date2 <- date1 + 3*365.242198781*24*60*60
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
while(as.timeDate(as.Date(paste(year(date2),"-",month(date2),"-",day(date2),sep=""))) <= end(ln.cad.usd.ex.m)) {
	temp <- window(ln.cad.usd.ex.m, date1, date2)
	moving.avg[h] <- mean(window(cad.usd.ex.m, date1, date2))
	date.ma[h] <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
	if (adf.test(as.numeric(temp))$statistic < -1.931)
	{
		lm.cad.usd.ex.temp <- lm(temp ~ lag(temp,k=1))
		mu.temp <-  exp(coefficients(lm.cad.usd.ex.temp)[1]/(1-coefficients(lm.cad.usd.ex.temp)[2]))
		lambda.temp <- -log(coefficients(lm.cad.usd.ex.temp)[2])/(1/12)
		sigma.temp <- sigma_0.cad.usd.ex*sqrt(-2*log(coefficients(lm.cad.usd.ex.temp)[2])/((1-coefficients(lm.cad.usd.ex.temp)[2]^2)*(1/12)))
		date.temp <- date1 + as.numeric(difftimeDate(date2, date1, units="secs")/2)
		if (as.numeric(mu.temp) < moving.avg[h]*(1+pc) & as.numeric(mu.temp) > moving.avg[h]*(1-pc)) {
			mu.roll[j] <- mu.temp; lambda.roll[j] <- lambda.temp; sigma.roll[j] <- sigma.temp
			date.parameters[j] <- date.temp; j <- j+1} 
	} else if (adf.test(as.numeric(temp))$p.value<=0.1) 
	{
		lm.cad.usd.ex.temp <- lm(temp ~ lag(temp,k=1))
		mu.temp <-  exp(coefficients(lm.cad.usd.ex.temp)[1]/(1-coefficients(lm.cad.usd.ex.temp)[2]))
		lambda.temp <- -log(coefficients(lm.cad.usd.ex.temp)[2])/(1/12)
		sigma.temp <- sigma_0.cad.usd.ex*sqrt(-2*log(coefficients(lm.cad.usd.ex.temp)[2])/((1-coefficients(lm.cad.usd.ex.temp)[2]^2)*(1/12)))
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
dev.new(); plot(cad.usd.ex.m,ylim=c(0,2))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)
dev.new(); plot(window(cad.usd.ex.m,start="2008-01-01",end=end(cad.usd.ex.m)),ylim=c(0,2))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)

#### Forecast out 1 time period -> 1 Month
delta.t <- 1
price.t <- cad.usd.ex.m$"CAD-USD EX"[1]
last.price.date <-  end(cad.usd.ex.m)

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
	#### Sigma is modified by mu/3.45 to fit historical standard deviation
	mean.rev.scaler <- 1.15 ### Attempt to reduce the effect of the mean reversion term to induce more "walk"
	price.plus1.step <- mean.rev.scaler*(1-exp(-lambda*dt))*mu + exp(-lambda*dt)*price + sigma*mu/3*sqrt((1-exp(-2*lambda*dt))/(2*lambda))*b.motion
	return(price.plus1.step)
} ##################################

#### Calculate the 1st Forecast Month Price based on historical data
price.fc.matrix <- as.data.frame(matrix(nrow=fc.periods,ncol=n))
for (i in 1:n) {
	price.fc.matrix[1,i] <- price.fc.plus1(price.t,as.numeric(mu.ts[end(mu.ts)]),as.numeric(lambda.cad.usd.ex),
		as.numeric(sigma.cad.usd.ex),brownian.motion[1,i], delta.t)
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
	if (exists("cad.usd.ex.d.m")) {
		price.hist.fc.list[[i]] <- rbind(rev(price.fc.ts[1,i]),cad.usd.ex.d.m,cad.usd.ex.m)
	} else {price.hist.fc.list[[i]] <- rbind(rev(price.fc.ts[1,i]),cad.usd.ex.m)}
	ln.price.hist.fc.list[[i]] <- as.data.frame(matrix(nrow=1,ncol=1))
	if (exists("cad.usd.ex.d.m")) {
		ln.price.hist.fc.list[[i]] <- rbind(rev(ln.price.fc.ts[1,i]),log(cad.usd.ex.d.m),ln.cad.usd.ex.m)
	} else {ln.price.hist.fc.list[[i]] <- rbind(rev(ln.price.fc.ts[1,i]),ln.cad.usd.ex.m)}
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
		sigma.fc.temp <- sigma_0.cad.usd.ex*sqrt(-2*log(coefficients(lm.temp)[2])/((1-coefficients(lm.temp)[2]^2)*(1/12)))
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
		sigma.fc.temp <- sigma_0.cad.usd.ex*sqrt(-2*log(coefficients(lm.temp)[2])/((1-coefficients(lm.temp)[2]^2)*(1/12)))
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
	price.fc.ts.temp <- price.fc.plus1(as.numeric(price.hist.fc.list[[i]][1]),as.numeric(mu.fc.ts.list[[i]][end(mu.fc.ts.list[[i]])]), as.numeric(lambda.cad.usd.ex),as.numeric(sigma.cad.usd.ex),b.motion.ts[j,i],delta.t)
	ln.price.fc.ts.temp <- log(price.fc.ts.temp)
	price.hist.fc.list[[i]] <- rbind(price.fc.ts.temp, price.hist.fc.list[[i]])
	colnames(price.hist.fc.list[[i]]) <- c("USD CAD Ex")
	ln.price.hist.fc.list[[i]] <- rbind(price.fc.ts.temp, ln.price.hist.fc.list[[i]])
	colnames(ln.price.hist.fc.list[[i]]) <- c("ln(USD CAD Ex)")	
	}
}


color.palate <- c("blue","green","orange","pink","red","purple","thistle","turquoise","grey","brown")
dev.new()
plot(price.hist.fc.list[[1]],xlim = c(start(price.hist.fc.list[[1]]),end(price.hist.fc.list[[1]])),ylim=c(0,2))
points(mu.ts,col="blue",pch=1)
for (m in 2:n-1) {lines(price.hist.fc.list[[m]],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(price.hist.fc.list[[n]],col="black")

summary(cad.usd.ex.m.returns.fit.norm)
dev.new(); plot(cad.usd.ex.m.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
title("Gaussian")

c1 <-1
#### Test Histogram and Density functions of the WTI Forecast Prices
while (c1 < n) 
{
	price.fc.temp <- window(price.hist.fc.list[[c1]],start=end(cad.usd.ex.m),end=end(price.hist.fc.list[[c1]]))
	cad.usd.ex.fc.returns <- returns(price.fc.temp)
	### Fitting WTI Monthly Returns Distribution
	cad.usd.ex.fc.returns.num <- as.numeric(cad.usd.ex.fc.returns)
	cad.usd.ex.fc.returns.fit.norm <- fitdist(cad.usd.ex.fc.returns.num, "norm") #Fits WTI returns with Gaussian (Normal) Distribution
	print(summary(cad.usd.ex.fc.returns.fit.norm))
	dev.new(); plot(cad.usd.ex.fc.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
	title(paste("Gaussian ",c1,sep=""))
	c1 <- c1 + trunc(n/10)
}

end.run <- Sys.timeDate()
print(start.run)
print(end.run)

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/CAD US EX/CSV Files")
write.csv(as.data.frame(as.list(price.hist.fc.list)),file=paste("USD-CAD Monte Carlo - ",n," Price Series (",as.character(Sys.Date()),").csv",sep=""))
write.csv(as.data.frame(as.list(price.hist.fc.list)),file=paste("USD-CAD Monte Carlo - ",n," Price Series (most recent).csv",sep=""))

setwd("C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/PDF R-Reports/50 Years")
pdf(file="USD-CAD Exchange Volatility Report.pdf")
#Plot of Historical CAD-USD Exchange with Mean Reversion
plot(cad.usd.ex.m,ylim=c(0,2))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)
#Plot of Historical CAD-USD Exchange with Mean Reversion > 2008
plot(window(cad.usd.ex.m,start="2008-01-01",end=end(cad.usd.ex.m)),ylim=c(0,2))
points(moving.average.ts,col="green",pch=1);points(mu.ts,col="blue",pch=1)
#Distribution of WTI Returns
plot(cad.usd.ex.m.returns.fit.norm,breaks=30) ### Model with Gaussian (Normal) Distribution
title("Gaussian")
#Report Average Historical Variance etc.
#plot.new()
#mtext(paste("Historical Mean = ",mu.wti.real,sep=""))
#mtext(paste("Historical Variance = ",sigma.wti.real,sep=""))
#mtext(paste("Historical Half-Life = ",lambda.wti.real,sep=""))
#Plot of Future Price Curves
plot(price.hist.fc.list[[1]],xlim = c(start(price.hist.fc.list[[1]]),end(price.hist.fc.list[[1]])),ylim=c(0,2))
points(mu.ts,col="blue",pch=1)
for (m in 2:n-1) {lines(price.hist.fc.list[[m]],col=color.palate[as.numeric(str_sub(m,start=-1))])}
lines(price.hist.fc.list[[n]],col="black")
dev.off()


