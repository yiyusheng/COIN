#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: load_bitstampUSD.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-27 21:51:37
#
# Last   modified: 2017-04-27 21:52:06
#
#
#

rm(list = ls());setwd('~/Code/R/COIN/');source('~/rhead')
DT <- read.csv(file.path(dir_data,'bitstampUSD.csv'),header = F)
names(DT) <- c('time','price','value')
DT$time <- as.POSIXct.numeric(DT$time,origin = '1970-01-01')
save(DT,file = file.path(dir_data,'bitstampUSD.Rda'))


###### ANALYSIS ######
DT$date <- as.Date(DT$time)
sta_date <- melt(table(DT$date))
