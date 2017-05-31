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
library(Hmisc)
source('plotFunc.R')
source('edaFunc.R')
source('logFunc.R')
# DT <- load_DT()
load(file.path(dir_data,'sqlData.Rda'))


###### MAIN:STATISTIC ######\
# S0. add interval
DT <- gen_interval(DT)

# S1. number of value on certain day/hour/minutes
aggTOW <- setNames(with(DT,aggregate(value,by = list(pair,type,timePW),sum)),
                   c('pair','type','time','numValue'))
aggTOH <- setNames(with(DT,aggregate(value,by = list(pair,type,timePH),sum)),
                   c('pair','type','time','numValue'))
aggTOM <- setNames(with(DT,aggregate(value,by = list(pair,type,timePM),sum)),
                   c('pair','type','time','numValue'))

###### MAIN:PLOT ######
# P1. plot trend of price and bid/ask count
p1 <- plot_price('ltc_usd')
list[p_trade,p_value] <- plot_value()
multiplot(p1,p_trade,p_value,cols = 1)

# P2. plot the candleStick plot
p <- getCandleStick()

# P3. number of value on certain day/hour/minutes
ggplot(aggTOW,aes(x = time,y = numValue,fill = type)) + geom_bar(stat = 'identity')
ggplot(aggTOH,aes(x = time,y = numValue,fill = type)) + geom_bar(stat = 'identity')
ggplot(aggTOM,aes(x = time,y = numValue,fill = type)) + geom_bar(stat = 'identity')
