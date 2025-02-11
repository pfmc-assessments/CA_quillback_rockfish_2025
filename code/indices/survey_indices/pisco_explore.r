# PISCO data for the 2025 CA quillback assessment

##### Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

##### Load packages
library(tidyverse)  


dir <- file.path("C:/users/melissa.monk/documents/stock_assessments/CA_quillback_2025")
setwd(dir)
##### Load data
# quillback observations and bio data
load("pisco_pulls.RData")

# get the number of counts
dim(pisco_catch) #20 observations of quillback
sum(pisco_catch$sum_cnt) #totalling 26 fish

#get the number of total lengths
dim(pisco_lengths)
sum(pisco_lengths$count) #26 lengths

summary(pisco_lengths$fish_tl)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  14.00   18.00   24.00   24.08   27.00   40.00


#how many sightings per year
with(pisco_catch, table(year, as.factor(site_side)))


#data too sparse to do anything with