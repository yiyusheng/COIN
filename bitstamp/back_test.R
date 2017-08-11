#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: back_test.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-06-30 11:49:44
#
# Last   modified: 2017-06-30 11:49:46
#
#
#

rm(list = ls());setwd('~/Code/R/COIN/bitstamp/');source('~/rhead')
require(Hmisc)
require(quantmod)
require(TTR)

###### MAIN ######
# getSymbols("AAPL",from="2017-01-01",to=Sys.Date())
load(file.path(dir_data,'bitstamp_xts.Rda'))
DT.1d <- get_xts_period(DT,'days',1)

start <- '2017-01-01'
end <- '2017-04-01'
money_base <- 10000
btc_base <- 0

account <- list(start = start,
                end = end,
                money_base = money_base,
                btc_base = btc_base)

handle_data <- function(account){
  DT <- DT.1d[paste(account$start,'/',account$end,sep='')]
  DT_price <- as.numeric(Cl(DT))
  ma5 <- runMean(DT[,4],n = 5)
  ma10 <- runMean(DT[,4],n = 10)
  policy <- rep(0,nrow(DT))
  
  dif_5_10 <- ma5 - ma10
  policy[dif_5_10 > 0] <- 1
  policy[dif_5_10 < 0] <- -1
  rate_money <- 0.5
  rate_btc <- 0.5
  fee <- 0.002

  for(i in seq_len(length(policy))){
    if(policy[i] == 1 & account$money_base > 0){
      account$btc_base <- account$btc_base + account$money_base*rate_money/DT_price[i]*(1-fee)
      account$money_base <- account$money_base*(1-rate_money)
    }else if(policy[i] == -1 & account$btc_base > 0){
      account$money_base <- account$money_base + account$btc_base*rate_btc*DT_price[i]*(1-fee)
      account$btc_base <- account$btc_base*(1-rate_btc)
    }
  }
  account$asset <- account$money_base + account$btc_base*DT_price[length(DT_price)]
}