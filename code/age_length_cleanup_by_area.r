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
#The age_length_cleanup file starts with
# 1117 fish: 1083 CA fish plus 34 from the NWFSC trawl caught in California
# 1088 after removing 29 carcass sampled fish 
# 1080 after removing 8 fish without lengths
# From age_length_cleanup.R
 # project            n
# Abrams              116 #matches
# Combined Survey      34 #matches
# Commercial          281 #ok however per issue #47 in our github repo, 6 of 
#these fish (in 2019) were not part of the sampling design and instead purchased, 
#so shouldn't be assigned for pacfin
# SWFSC boxed          27 #These are actually commercial fish 27 + 281 - 6 = 302, 
# total               458

# NMFS-Cooperative   134 #matches
# NMFS-SWFSC         114 #some collected under the CCFRP permit, but not as part of CCFRP
# CCFRP              151 # 114 + 151 + 134 = 399; 
#total               399 
#have 1 more fish now from the SWFSC samples (Rachel found a fish from Humboldt Jamie just aged)


#CDFW    223 # A number of fish from Andre are duplicated that I didn't catch.
#40 fish from 2025 SWFSC wrongly assigned to CDFW

#Still need to find - there were aged for the 2021 assessment and trying to track down...
#5 1985 fish don't show up in any of the new data - where did they come from? 
    #5 commercial fish that cannot be matched to any data, ages not documented anywhere
    #These are assigned as miscellaneous fish in this script. 
#5 2004 fish don't show up in the new data - where did they come from?
    #4 commercial fish unmatched to data, 1 research fish - ages not documented anywhere
    #The 4 comm fish are assigned as miscellaneous fish in this script
    #The other 1 fish is not included, as described in the miscellaneous section
#2 2006 fish don't show up in the new data - where did they come from?
  #2 research fish from the juvenile rockfish cruise; ages not documented anywhere else
  #These fish are not included, as described in the miscellaneous section

#Pt Arena at 39 N
#Age data sources
#1. pacfin
#2. Cooperative
#3. CCFRP
#4. CDFW
#5. Abrams
#6. NWFSC Trawl
#7. SMURF data
#8. Misc. fish aged for the 2021 assessment

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
#same numbers as the prior data accounting for the 6 fish that appropriately now 
#aren't in pacfin but rather labelled as CDFW (see CDFW section)

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

##################################################################
#2. Cooperative

#assign projects to groups and then north and south
#label farallon fish, so as to exclude from comps when the index doesn't include farallon trips
swfsc <- read.csv(here("data-raw", "ages", "SWFSC-QLBK-Otolith-Inventory.csv"))
swfsc <- swfsc %>% mutate(year = substr(Sample_ID, 1, 4)) %>% filter(!is.na(Forked_Length_mm)) %>%
mutate(length_cm = Forked_Length_mm/10) %>%
mutate(age = Final_Age, sex = Sex) %>%
mutate(source = case_when(Project_ID == 1005 & Port %in% c("Pillar Point", "Emeryville") ~ "CCFRPFarallons",
                     Project_ID == 1005 & !Port %in% c("Pillar Point", "Emeryville") ~ "CCFRPNotFarallons",
                         Project_ID == 1012 ~ "Cooperative",
                         Project_ID == 1013 ~ "SWFSCResearch")) %>%
mutate(faa_area = case_when(Port %in% c("Sausalito", "Pillar Point", "Emeryville", "Bodega", "Bodega Bay") ~ "south",
                            Port %in% c("Crescent City", "Eureka", "Fort Bragg") ~ "north"))
swfsc$year <- as.numeric(swfsc$year)

swfsc_faa <- swfsc %>%
dplyr::select(year, length_cm, age, sex, source, faa_area)
###################################################################
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
##################################################################
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
###################################################################
#6. NWFSC trawl
#See 1.

###################################################################
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

#######################################################
#5. Misc data
#These ages only appear in the data dump from Patrick

#Assign them 
misc <- data_dump %>%
         filter(year %in% c(1985, 2004, 2006), source == "CA") %>%
         mutate(project = sample_type)

#assign correct projects
misc$project[grepl("Recreation", misc$project) & misc$year == 2004] <- "calcom" #north fish and now in calcom; and no 
misc$project[grepl("Recreation", misc$project) & misc$year == 1985] <- "comm85" #don't know location - assign north
misc$project[grepl("Research", misc$project)] <- "gfecology" #2004 fish is south, 2006 fish from south of Conception..

 misc <- misc %>%
         mutate(age = age_best, 
         faa_area = case_when(project %in% c("calcom", "comm85") ~ "north", project == "gfecology" ~ "south"), 
         source = project, 
                 sex = case_when(sex == 1 ~ "M", sex == 2 ~ "F", sex == 3 ~ "U")) %>%
                 filter(source != "gfecology")
#remove the three GF ecology fish: 2 are from south of Point Conception


misc_faa <- misc %>%
dplyr::select(year, length_cm, age, sex, source, faa_area)
#######################################################







########################################################################
#bind them all together
faa_area_age_length <- rbind(pacfin_trawl_faa, 
                            abrams_faa, 
                            swfsc_faa, 
                            mal_faa, 
                            cdfw_faa,
                            misc_faa)

dim(pacfin_trawl_faa)[1] +
dim(abrams_faa)[1] +
dim(swfsc_faa)[1] +
dim(mal_faa)[1] +
dim(cdfw_faa)[1] +
dim(misc_faa)[1]

dim(faa_area_age_length)[1]
#rows match

#write csv
write.csv(faa_area_age_length, here("data-raw", "ages", "QLBK_faa_age_length.csv"), row.names = F)
