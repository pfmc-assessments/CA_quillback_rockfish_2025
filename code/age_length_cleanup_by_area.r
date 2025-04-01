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

#columns
#Year, length_cm, weight_kg, age, sex, lat, area, source, tripID
#add faa_area based on Pt Arena

#final columns for each dataset
#year, length_cm, age, sex, source, faa_area

#######################################################
#1. Pacfin
#6. Trawl
#read in data
all_processed_bio <- read.csv(here("data-raw","length_processed_noShare","CAquillback_ALL_bio.csv"))
#get just pacfin and trawl
pacfin_trawl <- all_processed_bio %>% filter(!is.na(age))
summary(as.factor(pacfin_trawl$source))
pacfin_trawl %>% filter(source == "pacfin") %>% group_by(Year) %>% tally()
#pacfin  trawl 
#   302     34

names(pacfin_trawl)
# [1] "Year"       "length_cm"  "weight_kg"  "age"        "sex"       
# [6] "depth_m"    "lat"        "lon"        "area"       "mode"
#[11] "disp"       "wgt_flag"   "lngth_flag" "source"     "tripID"

pacfin_trawl <- pacfin_trawl %>%
mutate(year = Year) %>%
mutate(faa_area = "north")
#all sampelse from north of Pt. Arena

#get the columns
pacfin_trawl_faa <- pacfin_trawl %>%
dplyr::select(year, length_cm, age, sex, source, faa_area)

#######################################################
#2. Cooperative
#3. CCFRP

#assign projects to groups and then north and south
swfsc <- read.csv(here("data-raw", "ages", "SWFSC-QLBK-Otolith-Inventory.csv"))
swfsc <- swfsc %>% mutate(year = substr(Sample_ID, 1, 4)) %>% filter(!is.na(Forked_Length_mm)) %>%
mutate(length_cm = Forked_Length_mm/10) %>%
mutate(age = Final_Age, sex = Sex) %>%
mutate(source = case_when(Project_ID == 1005 ~ "CCFRP",
                         Project_ID == 1012 ~ "Cooperative",
                         Project_ID == 1013 ~ "SWFSCResearch")) %>%
mutate(faa_area = case_when(Port %in% c("Sausalito", "Pillar Point", "Emeryville", "Bodega", "Bodega Bay") ~ "south",
                            Port %in% c("Crescent City", "Eureka", "Fort Bragg") ~ "north"))
swfsc$year <- as.numeric(swfsc$year)

swfsc_faa <- swfsc %>%
dplyr::select(year, length_cm, age, sex, source, faa_area)
#######################################################
#3. CCFRP
#See #2

###################################################################
#4. CDFW
#not including the 3 historical samples with TL and missing length
#not including carcass samples
cdfw <- read.csv(here("data-raw", "ages", "CDFW QB Age Data_compiled_032825_combined_tab.csv"))

cdfw <- cdfw %>%
       mutate(sex = case_when(Sex == 1 ~ "M", Sex == 2 ~ "F", Sex == 9 ~ "U"), 
       length_cm = FLmm/10) %>%
       mutate(faa_area = case_when(PortCode %in% c("BDG", "MNT", "OSF") ~ "south", 
                                   PortCode %in% c("BRG", "CRS", "ERK") ~ "north"))
cdfw_faa <- cdfw %>%
dplyr::select(year, length_cm, age, sex, source, faa_area)
#######################################################
#5. Abrams
#read in using the data dump
data_dump <- read.csv(file.path(here(), "data-raw", "ages","QLBK_Data_Dump_updated.csv"))

#remove OR and trawl and 
ca <- data_dump %>%
    mutate(project = sample_type) 
ca$project[grepl("Research", ca$project) & ca$year %in% c(2010,2011)] <- "Abrams"

abrams <- ca %>% filter(project == "Abrams") %>% mutate(age = age_best, faa_area = "north", source = project, 
                 sex = case_when(sex == 1 ~ "M", sex == 2 ~ "F", sex == 3 ~ "U"))

abrams_faa <- abrams %>%
dplyr::select(year, length_cm, age, sex, source, faa_area)
#######################################################
#6. NWFSC trawl
#See 1.

#######################################################
#7. SMURF data
smurfs <- read.csv(here("data-raw","Baetscher_juvenile_rockfish_NSF_dispersal_proj_genetic_ids.csv"))
smurfs$year <- sub(".*(\\d+{4}).*$", "\\1", smurfs$COLLECTION_DATE) #get last 4 digits
mal <- smurfs %>%
filter(GENETIC_ID == "Smaliger",
       !is.na(LENGTH))
mal$year = as.numeric(mal$year)
mal <- mal %>% mutate(source = "SMURFS", faa_area = "south", length_cm = LENGTH/10, sex = "U", age = 0) 
mal_faa <- mal %>%
dplyr::select(year, length_cm, age, sex, source, faa_area)


#bind them all together
faa_area_age_length <- rbind(pacfin_trawl_faa, 
                            abrams_faa, 
                            swfsc_faa, 
                            mal_faa, 
                            cdfw_faa)

dim(pacfin_trawl_faa)[1] +
dim(abrams_faa)[1] +
dim(swfsc_faa)[1] +
dim(mal_faa)[1] +
dim(cdfw_faa)[1]

dim(faa_area_age_length)[1]
#rows match

#write csv
write.csv(faa_area_age_length, here("data-raw", "ages", "QLBK_faa_age_length.csv"), row.names = F)
