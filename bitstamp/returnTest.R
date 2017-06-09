rm(list = ls());setwd('~/Code/R/COIN/bitstamp/');source('~/rhead')
require(Hmisc)
require(quantmod)
require(TTR)

###### MAIN ######
# getSymbols("AAPL",from="2017-01-01",to=Sys.Date())
load(file.path(dir_data,'bitstamp_xts.Rda'))
DT.1d <- get_xts_period(DT,'days',1)

chartSeries(DT.1d["2013-03::2013-06"],theme="white")
ma_20<-runMean(DT.1d[,4],n=20)   
ma_60<-runMean(DT.1d[,4],n=60)
addTA(ma_20,on=1,col="blue")
addTA(ma_60,on=1,col="red")

position<-Lag(ifelse(ma_20>ma_60, 1,0))
return<-ROC(Cl(DT.1d))*position
return<-return['2014-12-31/2016-12-31']
return<-exp(cumsum(return))
plot(return)

addBBands()
addBBands(draw="p")