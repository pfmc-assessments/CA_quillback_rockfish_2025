##########################################################################################-
#
# Sensitivity runs for 2025 California Quillback Rockfish 
#   By: Brian Langseth, Melissa Monk, Julia Coates
#
##########################################################################################-

#Load packages
library(r4ss)
library(here)
library(dplyr)
library(ggplot2)
source(here('code/selexComp.R'))

#Enter in base model from which to base sensitivities
base_mod_name <- '3_2_2_SetUpExtraSE' 
base_mod <- SS_read(here('models', base_mod_name))

#Create the sensitivities directory
sens_dir <- here('models', '_sensitivities')
dir.create(sens_dir)

#Set up fleet converter
fleet.converter <- base_mod$dat$fleetinfo %>%
  dplyr::mutate(fleet_no_num = 1:5,
                fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::select(fleetname, fleet_no_num, fleet)


####------------------------------------------------#
# Leave one out explorations ----
####------------------------------------------------#


## Drop length data by fleet --------------------------------------------------------

## Drop age data by fleet --------------------------------------------------------

## Drop indices (and index comps) by fleet --------------------------------------------------------



####------------------------------------------------#
# Other sensitivities ----
####------------------------------------------------#


## Reduce catches --------------------------------------------------------



## Continue on here