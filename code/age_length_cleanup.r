###############################################################
# Script clean up the available age at length data for quillback
# rockfish for the 2025 California stock assessment
# Also cleaned up and produces 2 files for ageing error
# One that includes Oregon data and one that excludes OR double
# reads
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
#  CA   NWFSC    OR 
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

####################################################################################################
# Double reads

dubreadsdir <- here("data-raw", "ageing_error")
#read in double reads
#Patrick McDonald and Jamie Hale 
dubreads <- read.csv(file.path(dubreadsdir,"QLBK_Double_Reads.csv"))
dubreads <- dubreads %>% dplyr::select(-c(date_aged, date_sent)) %>% unique()#drops duplicates
#716

#keep oregon but remove possible double reads
ca_or_bottomtrawl <- bio %>%
          filter(!is.na(Age)) %>%
          unique()

ca_or_dubreads <- dubreads %>%
filter(case_when (
       (source == "NWFSC" & specimen_id %in% ca_or_bottomtrawl$Otosag_id) ~ T,
       source == "CA" ~ T,
       source == "OR" ~ T,
       T ~ F))
#have 713 double reads now

# save the file as reader 1 and reader 1
ca_or_csv <- ca_or_dubreads %>%
rename(Reader1 = age_best, Reader2 = double_read_age) %>%
dplyr::select(c(Reader1, Reader2))

write.csv(ca_or_csv, 
file =  here("data-raw", "ageing_error","QLBK_DoubleReads_CA_OR.csv"), row.names = F)


###################################################################################
#remove oregon and possible duplicates
ca_dubreads <- dubreads %>%
filter(case_when (
       (source == "NWFSC" & specimen_id %in% ca_bottomtrawl$Otosag_id) ~ T,
       source == "CA" ~ T,
       T ~ F))
#418 now

# save the file as reader 1 and reader 1
ca_csv <- ca_dubreads %>%
rename(Reader1 = age_best, Reader2 = double_read_age) %>%
dplyr::select(c(Reader1, Reader2))

write.csv(ca_csv, file =  here("data-raw", "ageing_error","QLBK_DoubleReads_CA.csv"), row.names = F)
