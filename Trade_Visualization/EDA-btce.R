rm(list = ls())
setwd('~/Code/R/COIN/Trade_Visualization/')
source('~/rhead')

DT <- read.csv(file.path(dir_data,'btce.csv'))
names(DT) <- c('index','pair','type','price','tid','value','time')

###### MAIN ######
DT <- subsetX(DT,!is.na(index))
DT$time <- as.POSIXct.numeric(DT$time,origin = '1970-01-01')

table_pair_type <- melt(table(DT$type,DT$pair))
names(table_pair_type) <- c('type','pair','count')
ggplot(table_pair_type,aes(x = pair,y = count,fill = type)) + geom_bar(stat = 'identity')

getCutInterval <- function(t,interval){
  mint <- round.POSIXt(min(t),'mins') - 60
  maxt <- round.POSIXt(max(t),'mins') + 60
  seqt <- seq.POSIXt(mint,maxt,interval)
  cut.POSIXt(t,seqt,seqt[-length(seqt)])
}

getCandleStick <- function(interval = 300,gcsPair = 'btc_usd'){
  gcsDT <- subset(DT,pair == gcsPair)
  gcsDT$unitT <- as.p(getCutInterval(gcsDT$timestamp,15*60))
  sta <- by(gcsDT,gcsDT$unitT,function(df){
    list(df$unitT[1],
    roundX(mean(df$price)),
    df$price[1],
    df$price[nrow(df)],
    max(df$price),
    min(df$price),
    sum(df$amount))
  })
  sta <- data.frame(matrix(unlist(sta),byrow = T,nrow = length(sta)))
  names(sta) <- c('ts','mean','open','close','max','min','amount')
  sta$ts <- as.POSIXct.numeric(sta$ts,origin = '1970-01-01')

  ggplot(sta,aes(x = ts,group = 1)) + 
    geom_boxplot(aes(ymin = min, ymax = max, middle = mean, lower = pmin(open,close), upper = pmax(open,close)), stat = 'identity')
}
p <- getCandleStick()
