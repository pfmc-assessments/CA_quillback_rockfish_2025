###############################################################
# Script clean up the available age at length data for quillback
# rockfish for the 2025 California stock assessment
#
#Author: Melissa Monk SWFSC
#      2/28/2025
###############################################################
rm(list = ls(all = TRUE))
graphics.off()

library(ggplot2)
library(dplyr)
library(here)
library(nwfscSurvey)

setwd(here())
#fig.dir <- file.path(here(),"data_explore_figs")
#read in the data
#This is a data dump from Patrick McDonald (NWFSC CAP lab) of all the 
#quillback his lab has aged

qlbk <- read.csv(file.path(here(), "data-raw", "QLBK_Data_Dump_updated.csv"))
qlbk <- qlbk %>% dplyr::select(-c(date_aged, date_sent)) %>% unique()#drops duplicates
str(qlbk)
dim(qlbk)
#starting count 2358

#check how many by source
summary(as.factor(qlbk$source))
#  CA NWFSC    OR 
# 1083   199  1076

#remove Oregon
qlbk <- qlbk %>%
filter(source != 'OR')


#pull survey bio
bio = pull_bio(
  common_name = "quillback rockfish", 
  survey = "NWFSC.Combo")

ca_bottomtrawl <- bio %>%
          filter(Latitude_dd < 42, 
          !is.na(Age)) %>%

          unique()
with(ca_bottomtrawl, table(Year, Age))


#remove Oregon and northern data from the survey
ca <- qlbk %>%
filter(case_when (
       (source == "NWFSC" & specimen_id %in% ca_bottomtrawl$Otosag_id) ~ T,
       source == "CA" ~ T,
       T ~ F))

with(ca, table(source))


#remove the carcass sampling
ca <- ca %>%
   filter(sample_type != "Carcass Sampling")


#Recategorize CCFRP to all the same
ca <- ca %>%
filter(!is.na(length_cm)) %>%
mutate(project = sample_type) 

ca$project[grepl("CCFRP", ca$project)] <- "CCFRP"
ca$project[grepl("Recreational", ca$project)] <- "CDFW"
ca$project[grepl("Rec.", ca$project)] <- "CDFW"
ca$project[grepl("Research", ca$project) & ca$year %in% c(2010,2011)] <- "Abrams"
ca$project[grepl("Research", ca$project)] <- "CDFW" #remaining research go to CDFW
ca$project[grepl("Unknown", ca$project) & ca$year == 2007] <- "SWFSC boxed"
ca$project[grepl("Unknown", ca$project) & ca$year < 2007] <- "CDFW"


#rename columns
ca <- ca %>%
rename(age = age_best, 
       Sex = sex)
ca$Sex <- as.factor(ca$Sex)
#tally by sample type - renamed project to group
#using this to check things
ca %>%
 #filter(project == "Combined Survey") %>%
    group_by(project) %>%
    tally() 
 
#how many by sex
#Sex = 1 is male, Sex = 2 is female, Sex = 3 is unsexed
ca %>%
    group_by(Sex) %>%
    tally() 


save(ca, file =  here("data-raw","all_ages_labeled.RData"))
