#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: gen_reward.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-30 18:21:17
#
# Last   modified: 2017-04-30 18:21:19
#
#
#

rm(list = ls());setwd('~/Code/R/COIN/');source('~/rhead')

get_diurnal_price <- function(DT,mtd = 'mean'){
  f <- get(mtd)
  v <- melt(tapply(DT$price,DT$date,f))
  names(v) <- c('time','price')
  v$price <- round(v$price,digits = 4)
  v$time <- as.Date(v$time)
  return(v)
}

patch_missing_price <- function(DP,missing_date){
  left_value <- DP$price[max(which((missing_date - DP$time) > 0))]
  right_value <- DP$price[min(which((DP$time - missing_date) > 0))]
  return(data.frame(time = as.Date(missing_date,origin = '1970-01-01'),
                    price = (left_value + right_value)/2))
}

get_diurnal_reward <- function(DP){
  DP$num_cash <- 0;DP$num_coin <- 0;DP$total_revenue <- 0
  DP$num_cash[1] <- 100;DP$total_revenue[1] <- DP$num_cash[1] + DP$num_coin[1]*DP$price[1]
  for(i in seq_len((nrow(DP)-1))){
    if(DP$act[i] > 0){
      val <- DP$num_cash[i]*DP$act[i]
      DP$num_cash[i+1] <- DP$num_cash[i] - val
      DP$num_coin[i+1] <- DP$num_coin[i] + val/DP$price[i]
      DP$total_revenue[i+1] <- DP$num_cash[i+1] + DP$num_coin[i+1]*DP$price[i+1]
    }else if(DP$act[i] < 0){
      val <- DP$price[i]*DP$num_coin[i]*(-DP$act[i])
      DP$num_cash[i+1] <- DP$num_cash[i] + val
      DP$num_coin[i+1] <- DP$num_coin[i]*(1 + DP$act[i])
      DP$total_revenue[i+1] <- DP$num_cash[i+1] + DP$num_coin[i+1]*DP$price[i+1]
    }else{
      DP[i+1,c('num_cash','num_coin','total_revenue')] <- DP[i,c('num_cash','num_coin','total_revenue')]
    }
  }
  DP$num_cash <- roundX(DP$num_cash);DP$num_coin <- roundX(DP$num_coin);DP$total_revenue <- roundX(DP$total_revenue)
  return(DP)
}
###### MAIN ######
dayS <- as.Date('2011-11-01')
dayE <- as.Date('2017-04-01')
seq_days <- seq.Date(dayS,dayE,by = 'days')
actions <- data.frame(time = seq_days,
                      act = round(runif(length(seq_days),-1,1),digits = 2))

load(file.path(dir_data,'bitstampUSD.Rda'))
DT$date <- as.Date(DT$time)
DT <- subset(DT,date >= dayS & date <= dayE)
DP <- get_diurnal_price(DT,'mean')

missing_time <- as.Date(setdiff(actions$time,DP$time),origin = '1970-01-01')
DP_missing <- lapplyX(missing_time,function(t)patch_missing_price(DP,t))
DP <- rbind(DP,DP_missing)
DP <- DP[order(DP$time),]
DP$act <- actions$act[match(DP$time,actions$time)]
DP <- get_diurnal_reward(DP)

ggplot(DP,aes(x = time)) + geom_line(aes(y = price)) + geom_line((aes(y = total_revenue)))


