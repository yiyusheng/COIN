#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: region_cut.R
#
# Description: cut time series data into regions representing a continutes increment/reduction
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-12-01 12:02:01
#
# Last   modified: 2017-12-01 12:02:02
#
#
#
rm(list = ls());setwd('~/Code/R/COIN/Trade_Visualization/');source('~/rhead')
load(file.path(dir_data,'sqlData.Rda'))
pair_set <- c('btc_usd', 'eth_btc', 'eth_usd', 'ltc_btc', 'ltc_usd')

# S1. data prepare
pair_select <- pair_set[1]
DT_pair <- subset(DT,pair == pair_select)

gen_ohlc <- function(df,uni='hours'){
  df <- df[order(df$time),]
  df$unit_time <- as.numeric(base::round.POSIXt(df$time,uni))
  r <- by(df[,c('unit_time','price')],df$unit_time,function(dff){
    arr <- dff$price
    list(as.numeric(dff$unit_time[1]),arr[1],max(arr),min(arr),arr[length(arr)],mean(arr))
  })
  rr <- setNames(data.frame(matrix(unlist(r),nrow=length(r),byrow=T)),nm=c('unitime','open','high','low','close','mean'))
  rr$rise <- as.numeric(rr$close>rr$open)
  return(rr)
}

DT_ohlc_hours <- gen_ohlc(DT_pair,'hours')

ggplot(DT_ohlc_hours,aes(x = unitime)) + 
  geom_boxplot(aes(ymin = low, ymax = high, middle = (open+close)/2, 
                   color = (open-close) > 0,lower = pmin(open,close), upper = pmax(open,close)), stat = 'identity')+
  geom_line(aes(y=mean))
  