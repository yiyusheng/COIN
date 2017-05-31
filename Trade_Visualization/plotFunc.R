#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: plotFunc.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-19 14:51:50
#
# Last   modified: 2017-05-19 14:51:51
#
#
#

# P1. plot trend of price
plot_price <- function(pa,from = NULL,to = NULL,DTP = DT){
  if(!is.null(from) & !is.null(to)){
    DTP <- subset(DTP,time > from & time < to)
  }else{
    DTP <- DTP
  }
  p <- ggplot(subset(DTP,pair == pa),aes(x = time)) + geom_line(aes(y = price),color = 'red') + ggtitle(pa)
}

# P2. plot trend of number of trade
plot_num_trade <- function(pa,bw = 3600,from = NULL,to = NULL,DTP = DT){
  if(!is.null(from) & !is.null(to)){
    DTP <- subset(DTP,time > from & time < to)
  }else{
    DTP <- DTP
  }
  p <- ggplot(subset(DTP,pair == pa),aes(x = time)) + geom_histogram(binwidth = bw) + ggtitle(pa)
}

# P3. plot trend of number of value
plot_value <- function(){
  aggTPI <- setNames(with(DT,aggregate(value,by = list(pair,type,timeH),length)),
                     c('pair','type','time','numTrade'))
  aggVPI <- setNames(with(DT,aggregate(value,by = list(pair,type,timeH),sum)),
                     c('pair','type','time','numValue'))
  p1 <- ggplot(aggTPI,aes(x = time,y = numTrade,fill = type)) + geom_bar(stat = 'identity') + ggtitle('Trade per hour')
  p2 <- ggplot(aggVPI,aes(x = time,y = numValue,fill = type)) + geom_bar(stat = 'identity') + ggtitle('Value per hour')
  return(list(p1,p2))
}

# P3. plot CandleStick
plot_CandleStick <- function(interval = 300,gcsPair = 'btc_usd'){
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