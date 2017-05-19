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
load_sqlite <- function(path = NULL){
  require(rPython)
  if(is.null(path))python.load('~/Code/Python/COIN/Trade_Visualization/btce_export.py')
  DT <- python.get('DT')
  DT <- data.frame(matrix(unlist(DT),byrow = T,nrow = length(DT)))
  names(DT) = c('pair','type','price','tid','value','time')
  DT$price <- fct2num(DT$price)
  DT$tid <- as.integer(fct2ori(DT$tid))
  DT$value <- fct2num(DT$value)
  DT$time <- as.integer(fct2ori(DT$time))
  DT$time <- as.POSIXct.numeric(DT$time,origin = '1970-01-01')
  save(DT,file = file.path(dir_data,'sqlData.Rda'))
  return(DT)
}

# F2. cut time by interval
getCutInterval <- function(t,interval){
  mint <- round.POSIXt(min(t),interval)
  maxt <- round.POSIXt(max(t),interval)
  seqt <- seq.POSIXt(mint,maxt,interval)
  # seqt <- seqt[-c(1,length(seqt))]
  cut.POSIXt(t,seqt,seqt[-length(seqt)])
}