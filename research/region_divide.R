#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: region_divide.R
#
# Description: divide the increment and reduction regions
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-12-13 17:56:50
#
# Last   modified: 2017-12-13 17:56:51
#
#
#

rm(list = ls());setwd('~/Code/R/COIN/Trade_Visualization/');source('~/rhead')
load(file.path(dir_data,'Bitfinex_btcusd.Rda'))

gen_ma <- function(df,attr,para){
  arr <- df[[attr]]
  sum_ma <- rep(0,nrow(df))
  for(i in seq_len(para)){
    sum_ma <- sum_ma + c(rep(0,i-1),arr[1:(length(arr)-i+1)])
  }
  ma <- c(rep(0,para-1),(sum_ma[para:length(arr)])/para)
}

ma_week <- gen_ma(DT,'Mid',7)
DT <- cbind(DT,ma_week)
map1 <- 12
map2 <- 26
ma1 <- gen_ma(DT,'ma_week',map1)
ma2 <- gen_ma(DT,'ma_week',map2)


DT.region <- cbind(DT[,c('Date','Mid','ma_week')],ma1,ma2)
DT.region$madiff <- with(DT.region,ma1-ma2)
DT.region$madiff[1:map2] <- 0
DT.region$inflexion <- with(DT.region,as.numeric(c(0,madiff[-length(madiff)]*madiff[-1])<0))



p1 <- ggplot(DT.region[1:800,],aes(x=as.numeric(Date)))+
  geom_point(aes(y=ma_week),size=0.05)+geom_line(aes(y=ma_week))+
  geom_bar(aes(y=-abs(madiff),fill=madiff>0),stat = 'identity')+
  guides(fill = guide_legend(title='region'))+scale_fill_discrete(labels = c("Increment", "Reduction"), breaks = c(TRUE,FALSE))+
  geom_vline(aes(xintercept=as.numeric(Date)),data=subset(DT.region[1:800,],inflexion==1))
p1

#------
a <- DT
a$diff <- c(0,diff(a$Last))
a$flag <- as.numeric(a$diff>0)
cur <- 1
count <- 0
idx <- rep(0,nrow(a))
for(i in 1:nrow(a)){
  if(cur == a$flag[i]){
    count <- count+1
    idx[i] <- count
  }else{
    count <- 1
    cur <- a$flag[i]
    idx[i] <- count
  }
}
a$idx <- idx
