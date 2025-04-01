###############################################################
# Script clean up the available age at length data for quillback
# rockfish for the 2025 California stock assessment
#by data source to look at growth north and south of pt arena
# reads
#Author: Melissa Monk SWFSC
#      3/31/2025
###############################################################
rm(list = ls(all = TRUE))
graphics.off()

library(ggplot2)
library(dplyr)
library(here)
library(nwfscSurvey)

setwd(here())
###################################################################
#Pt Arena at 39 N
#Age data sources
#1. pacfin
#2. Cooperative
#3. CCFRP
#4. CDFW
#5. Abrams
#6. NWFSC Trawl
#7. SMURF data

all_processed_bio <- read.csv(here("data-raw","length_processed_noShare","CAquillback_ALL_bio.csv"))

#columns
#Year, length_cm, weight_kg, age, sex, lat, area, source, tripID

#######################################################
#1. Pacfin
#6. Trawl
#read in data
all_processed_bio <- read.csv(here("data-raw","length_processed_noShare","CAquillback_ALL_bio.csv"))
#get just pacfin and trawl
pacfin_trawl <- all_processed_bio %>% filter(!is.na(age))

#pacfin  trawl 
#   302     34

#######################################################
#2. Cooperative
#3. CCFRP
#5. Abrams
data_dump <- read.csv(file.path(here(), "data-raw", "ages","QLBK_Data_Dump_updated.csv"))

#remove OR and trawl and 
ca <- data_dump %>%
   filter(sample_type != "Carcass Sampling", sample_type != "Commercial") %>%
   filter(source != 'NWFSC', source != 'OR')

ca <- ca %>%
filter(!grepl("Recreational", sample_type), 
       !grepl("Rec.", sample_type), 
       !grepl("IPHC", sample_type),
       !grepl("Unknown", sample_type),
       !grepl("jfield", receiver_id))

ca <- ca %>%
filter(!is.na(length_cm)) %>%
mutate(project = sample_type) 

ca$project[grepl("Research", ca$project) & ca$year %in% c(2010,2011)] <- "Abrams"


#assign area based on ports 
ca <- ca %>%
    mutate(faa_area = case_when(grepl("HSU", project) ~ "north", 
                            grepl("BML", project) ~ "south",
                            grepl("Research", project) ~ "south",
                            grepl("MLML", project) ~ "south",
                            grepl("Abrams", project) ~ "north",
                            grepl("Cooperative", project) ~ "south",
                            .default = "tbd"))



#######################################################
#3. CCFRP
#See #2

#######################################################
#4. CDFW
#not including the 3 historical samples with TL
#not including carcass samples
cdfw <- read.csv(here("data-raw", "ages", "CDFW QB Age Data_compiled_032825_combined_tab.csv"))

#######################################################
#5. Abrams
#See #2

#######################################################
#6. NWFSC trawl
#See 1.

#######################################################
#7. SMURF data
dat <- read.csv(here("data-raw","Baetscher_juvenile_rockfish_NSF_dispersal_proj_genetic_ids.csv"))
str(dat)
dat$year <- sub(".*(\\d+{4}).*$", "\\1", dat$COLLECTION_DATE) #get last 4 digits
mal <- dat %>%
filter(GENETIC_ID == "Smaliger",
       !is.na(LENGTH))
