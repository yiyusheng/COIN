#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: load_sqlite.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-19 15:24:55
#
# Last   modified: 2017-05-19 15:24:56
#
#
#
rm(list = ls());setwd('~/Code/R/COIN/Trade_Visualization/');source('~/rhead')
source('plotFunc.R')
# DT <- load_sqlite()
load(file.path(dir_data,'sqlData.Rda'))
DT <- subset(DT,time > as.POSIXct('2017-05-12') & time < as.POSIXct(as.character(as.Date(max(DT$time)))))
DT <- subset(DT,pair == 'ltc_usd')

###### MAIN:STATISTIC ######\
DT$timeD <- getCutInterval(DT$time,interval = 'days')
DT$timeH <- getCutInterval(DT$time,interval = 'hours')
DT$timeM <- getCutInterval(DT$time,interval = 'mins')
# S1. number of trade per interval
tableTPI <- melt(table(DT$type,DT$pair,DT$timeD))
names(tableTPI) <- c('type','pair','time','count')
tableTPI <- subset(tableTPI,count != 0)
tableTPI$time <- as.Date(tableTPI$time)

# S2. number of value per interval
tableVPI <- list2df(tapply(DT$value,DT$timeD,sum),n = c('value','time'))

###### MAIN:PLOT ######
# P1. plot trend of price and bid/ask count
p1 <- plot_price('ltc_usd')
p2 <- plot_value('ltc_usd',bw = 3600)
multiplot(p1,p2,cols = 1)

# P2. plot the candleStick plot
p <- getCandleStick()
