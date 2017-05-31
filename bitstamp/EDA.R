#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: EDA.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-27 09:25:42
#
# Last   modified: 2017-05-27 09:25:43
#
#
#

rm(list = ls());setwd('~/Code/R/COIN/bitstamp/');source('~/rhead')
load(file.path(dir_data,'bitstampUSD.Rda'))
require(Hmisc)
require(quantmod)
require(TTR)

# S1. data prepare
DT1 <- DT
DT <- DT1
names(DT) <- c('time','price','volume')
DT <- xts(DT[,c('price','volume')],DT$time)
save(DT,file = file.path(dir_data,'bitstamp_xts.Rda'))

# S2. generate OLHC with different period
DT.1m <- get_xts_period(DT,'minutes',1)
DT.5m <- get_xts_period(DT,'minutes',5)
DT.15m <- get_xts_period(DT,'minutes',15)
DT.1h <- get_xts_period(DT,'hours',1)
DT.4h <- get_xts_period(DT,'hours',4)
DT.1d <- get_xts_period(DT,'days',1)
DT.1w <- get_xts_period(DT,'weeks',1)

# S3. plot chart
chartSeries(DT.1h['201703'],up.col='blue',down.col='red',
            TA = "addMACD();addBBands();addVo();addADX();addSMA(n=10)")

# S4. technical metrics
ma_20 <- runMean(DT.1h[,4],n = 20)
