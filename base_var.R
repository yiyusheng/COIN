###### VARIABLES ######
dirName <- 'COIN'
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99","#000000")
dir_code <- file.path(dir_c,dirName)
dir_data <- file.path(dir_d,dirName)

if (osFlag == 'Windows'){
  source('D:/Git/R_libs_user/R_custom_lib.R')
}else{
  source('~/Code/R/R_libs_user/R_custom_lib.R')
  # options('width' = 150)
}

###### PACKAGES ######
require('scales')
require('grid')
require('ggplot2')
require('reshape2')
require('plyr')


