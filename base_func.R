#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: base_func.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-19 16:00:57
#
# Last   modified: 2017-05-19 16:00:59
#
#
#

# F1. Load data from sqlite collected by python
load_sqlite <- function(){
  require(rPython)
  python.load('~/Code/Python/COIN/Trade_Visualization/btce_export.py')
  
  DT <- python.get('DT')
  DT <- data.frame(matrix(unlist(DT),byrow = T,nrow = length(DT)))
  names(DT) = c('pair','type','price','tid','value','time','create_time')
  DT$price <- fct2num(DT$price)
  DT$tid <- as.integer(fct2ori(DT$tid))
  DT$value <- fct2num(DT$value)
  DT$time <- as.integer(fct2ori(DT$time))
  DT$time <- as.POSIXct.numeric(DT$time,origin = '1970-01-01')
  DT$create_time <- as.POSIXct(DT$create_time)
  
  # turn on commit from btce_export.py to get DT_old
  # DT0522 <- python.get('DT_old')
  # DT0522 <- data.frame(matrix(unlist(DT0522),byrow = T,nrow = length(DT0522)))
  # names(DT0522) = c('pair','type','price','tid','value','time')
  # DT0522$price <- fct2num(DT0522$price)
  # DT0522$tid <- as.integer(fct2ori(DT0522$tid))
  # DT0522$value <- fct2num(DT0522$value)
  # DT0522$time <- as.integer(fct2ori(DT0522$time))
  # DT0522$time <- as.POSIXct.numeric(DT0522$time,origin = '1970-01-01')
  # DT0522$create_time <- as.POSIXct('2017-05-22 00:00:00')
  # save(DT0522,file = file.path(dir_data,'DT0522.Rda'))
  
  return(DT)
}

# F2. convert OLHC to special period. prd is period and k is the times of period.
get_xts_period <- function(DT.xts,prd,k){
  time_base <- data.frame(prd = c('minutes','hours','days','weeks'),
                          base = c(60,60*60,60*60*24,60*60*24*7))
  DT.xts <- to.period(DT.xts,period = prd,k = k,indexAt = 'endof')
  cur_tbase <- time_base$base[time_base$prd == prd]*k
  index(DT.xts) <- align.time(index(DT.xts),cur_tbase) - cur_tbase
  return(DT.xts)
}
