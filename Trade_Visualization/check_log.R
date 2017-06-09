#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: check_log.R
#
# Description: check log files to see difference between two collection
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-05-19 15:24:55
#
# Last   modified: 2017-05-23 09:26:48
#
#
#
rm(list = ls());setwd('~/Code/R/COIN/Trade_Visualization/');source('~/rhead')
require(Hmisc)
source('logFunc.R')

merge_diff <- function(DT1,DT2,attr){
  merge_DT <- merge(DT1[,c('timeSync',attr)],DT2[,c('timeSync',attr)],by = 'timeSync')
  names(merge_DT) <- c('timeSync','attr1','attr2')
  merge_DT$diffT <- merge_DT$attr1 - merge_DT$attr2
  p <- ggplot(merge_DT,aes(x = timeSync, y = diffT)) + geom_line() + geom_point() + ylab(paste('diff',attr,sep=''))
  return(list(merge_DT,p))
}


# S1. read logs
DT_woot <- get_info('btcewoot.log',12*3600)
DT_cu02 <- get_info('btce.log')

# S2. check difference of total number of items between two logs
list[merge_total,p_total] <- merge_diff(DT_cu02,DT_woot,'total')
list[merge_bu,p_bu] <- merge_diff(DT_cu02,DT_woot,'bu')
list[merge_lu,p_lu] <- merge_diff(DT_cu02,DT_woot,'lu')
list[merge_lb,p_lb] <- merge_diff(DT_cu02,DT_woot,'lb')
list[merge_eu,p_eu] <- merge_diff(DT_cu02,DT_woot,'eu')
list[merge_eb,p_eb] <- merge_diff(DT_cu02,DT_woot,'eb')
multiplot(p_total,p_bu,p_lu,p_lb,p_eu,p_eb,cols = 2)

merge_DT <- merge(DT_woot[,c('timeSync','total')],DT_cu02[,c('timeSync','total')],by = 'timeSync')
merge_DT$diffT <- merge_DT$total.y - merge_DT$total.x
ggplot(merge_DT,aes(x = timeSync, y = diffT)) + geom_line() + geom_point()

# S3. generate number of items collected each episode
col_value <- c('bu','lu','lb','eu','eb','total')
DTc_diff <- gen_countDiff(DT_cu02,col_value)
DTc_diff_melt <- melt(DTc_diff[,c(col_value[-length(col_value)],'timeSync')],id.vars = 'timeSync')
ggplot(DTc_diff_melt,aes(x = timeSync,y = value,group = variable,color = variable)) + geom_line() + geom_point()

DTw_diff <- gen_countDiff(DT_woot,col_value)
DTw_diff_melt <- melt(DTw_diff[,c(col_value[-length(col_value)],'timeSync')],id.vars = 'timeSync')
