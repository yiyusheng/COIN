#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: logFunc.R
#
# Description: 
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-24 14:57:08
#
# Last   modified: 2017-05-24 14:57:10
#
#
#

get_info <- function(DTname,difft = 0){
  DT <- read.csv(file.path(dir_d,'Trade_Visualization',DTname),header = F,sep='\t')
  DT <- subset(DT,V7 != '')
  
  DT$V1 <- as.p(gsub('\\[|\\]','',extract_reg('\\[.*\\]',DT$V1)))
  DT[,2:7] <- apply(DT[,2:7],2,function(x)as.numeric(gsub('.*:','',x)))
  names(DT) <- c('time','bu','lu','lb','eu','eb','total')
  
  DT$timeSync <- round_time(DT$time) + difft
  DT <- DT[!duplicated(DT$timeSync),]
  return(DT)
}


round_time <- function(t){
  t1 <- round.POSIXt(t,digits = 'hours')
  rest <- floor(as.numeric(t-t1)/60/20)*20
  t1 + rest*60
}

gen_countDiff <- function(DT,col_value){
  for(cv in col_value){
    DT[[cv]] <- c(0,diff(DT[[cv]]))
  }
  return(DT)
}
