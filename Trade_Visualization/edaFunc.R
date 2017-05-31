#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: edaFunc.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-24 15:13:45
#
# Last   modified: 2017-05-24 15:13:47
#
#
#

getCutInterval <- function(t,interval){
  mint <- trunc.POSIXt(min(t),interval)
  maxt <- ceil.POSIXt(max(t),interval)
  seqt <- seq.POSIXt(mint,maxt,interval)
  as.p1(cut.POSIXt(t,seqt,seqt[-length(seqt)]))
}

gen_interval <- function(DT){
  DT$timeD <- getCutInterval(DT$time,interval = 'days')
  DT$timeH <- getCutInterval(DT$time,interval = 'hours')
  DT$timeM <- getCutInterval(DT$time,interval = 'mins')
  DT$timePW <- as.numeric(format(DT$time,'%w'))
  DT$timePH <- as.numeric(format(DT$time,'%H'))
  DT$timePM <- as.numeric(format(DT$time,'%M'))
  return(DT)
}

load_DT <- function(){
  DT <- load_sqlite()
  load(file.path(dir_data,'DT0522.Rda'))
  DT <- rbind(DT,DT0522);DT <- DT[!duplicated(DT$tid),]
  DT$time <- as.POSIXct.numeric(as.numeric(DT$time),tz = 'UTC',origin='1970-01-01') #CST to UTC
  DT$create_time <- as.POSIXct.numeric(as.numeric(DT$create_time),tz = 'UTC',origin='1970-01-01') #CST to UTC
  DT$create_time <- round_time(DT$create_time)
  # DT <- subset(DT,time > as.POSIXct('2017-05-22 08:00:00',tz = 'UTC') & time < max(round.POSIXt(DT$time,units = 'days')) + 8*3600)
  DT <- DT[order(DT$time),]
  splitDT <- split(DT,DT$pair)
  save(DT,splitDT,file = file.path(dir_data,'sqlData.Rda'))
  return(DT)
}