
################### Source West Texas Intermediate from "\Dropbox\Pricing Volatility\Pricing Volatility CEC\WTI Pricing"
working.dir.wti <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/" # Work Computer
#working.dir.wti <- "" # Home Computer
setwd(working.dir.wti)
wti.file.path <- paste(working.dir.wti ,"WTI Volatility & Monte Carlo (Optimized V2.0).R",sep="")
source(wti.file.path)

rm(list=ls(all=TRUE)) #Remove All Objects

################### Source Henry Hub from "\Dropbox\Pricing Volatility\Pricing Volatility CEC\WTI Pricing"
working.dir.hh <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/WTI Pricing/" # Work Computer
#working.dir.hh <- "" # Home Computer
setwd(working.dir.hh)
hh.file.path <- paste(working.dir.hh,"HH Volatility & Monte Carlo (Optimized V2.0).R",sep="")
source(hh.file.path)

rm(list=ls(all=TRUE)) #Remove All Objects

################### Source US Consumer Price Index from "\Dropbox\Pricing Volatility\Pricing Volatility CEC\CAD US CPI"
working.dir.us.cpi <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/CAD US CPI/" # Work Computer
#working.dir.us.cpi <- "" # Home Computer
setwd(working.dir.us.cpi)
us.cpi.file.path <- paste(working.dir.us.cpi,"US CPI - Monte Carlo.R",sep="")
source(us.cpi.file.path)

rm(list=ls(all=TRUE)) #Remove All Objects

################### Source Canada Consumer Price Index from "\Dropbox\Pricing Volatility\Pricing Volatility CEC\CAD US CPI"
working.dir.cad.cpi <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/CAD US CPI/" # Work Computer
#working.dir.cad.cpi <- "" # Home Computer
setwd(working.dir.cad.cpi)
cad.cpi.file.path <- paste(working.dir.cad.cpi,"CAD CPI - Monte Carlo.R",sep="")
source(cad.cpi.file.path)

rm(list=ls(all=TRUE)) #Remove All Objects

################### Source Canada-US Exchange Rates from "\Dropbox\Pricing Volatility\Pricing Volatility CEC\CAD US EX"
working.dir.cad.usd.ex <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/CAD US EX/" # Work Computer
#working.dir.cad.usd.ex <- "" # Home Computer
setwd(working.dir.cad.usd.ex)
cad.usd.ex.file.path <- paste(working.dir.cad.usd.ex,"CAD US EX - Monte Carlo with Mean Reversion V2.0.R",sep="")
source(cad.usd.ex.file.path)

rm(list=ls(all=TRUE)) #Remove All Objects

################### Source Canadian Oil Differentials from "\Dropbox\Pricing Volatility\Pricing Volatility CEC\Canadian Price Differentials"
working.dir.cad.diff.oil <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Price Differentials/" # Work Computer
#working.dir.cad.diff.oil <- "" # Home Computer
setwd(working.dir.cad.diff.oil)
cad.diff.oil.file.path <- paste(working.dir.cad.diff.oil,"Canadian (Oil) Differentials w Sproule Differentials Monte Carlo.R",sep="")
source(cad.diff.oil.file.path)

rm(list=ls(all=TRUE)) #Remove All Objects

################### Source Canadian Gas Differentials from "\Dropbox\Pricing Volatility\Pricing Volatility CEC\Canadian Price Differentials"
working.dir.cad.diff.gas <- "C:/Users/don.mikalson/Dropbox/Pricing Volatility/Pricing Volatility CEC/Canadian Price Differentials/" # Work Computer
#working.dir.cad.diff.gas <- "" # Home Computer
setwd(working.dir.cad.diff.gas)
cad.diff.gas.file.path <- paste(working.dir.cad.diff.gas,"Canadian (Gas) Differentials w Sproule Differentials Monte Carlo.R",sep="")
source(cad.diff.gas.file.path)

rm(list=ls(all=TRUE)) #Remove All Objects