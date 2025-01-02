##############################################################################################################
#
# 	Purpose: Explore RecFIN/MRFSS quillback rockfish landings/discards 
#            before putting into form for use in SS. Also to explore
#            biological data, including other recreational sources
#
#   Created: Oct 3, 2024
#			  by Brian Langseth 
#
##############################################################################################################

library(here)
library(ggplot2)
library(magrittr)
#devtools::install_github("pfmc-assessments/PacFIN.Utilities")
#pak::pkg_install("pfmc-assessments/PacFIN.Utilities")
library(PacFIN.Utilities)
library(gridExtra)
library(readxl)

#-----------------------------------------------------------------------------#

# Process RecFIN catch data ----

#-----------------------------------------------------------------------------#

##
#Load catch data
##

# RecFIN Recreational - 2005-2023 Landings mtons
ca_rec = read.csv(here("data-raw","RecFIN-CTE001-California-quillback-1990---2023_2.Oct.2024.csv"), header = T)

#Pull 2021 assessment values for comparison. These are recFIN landings + discards
dir = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/output catch"
#dir = "T:/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/output catch"  #Melissa's path
catch_recfin_2021 = read.csv(file.path(dir,"recreational_catch_by_area_model_Feb2021.csv"), header = T) %>%
  dplyr::select(c("Year","CA_mort_mt"))


########################-
## Explore the data ----
########################-

#Looking across fields to see if anything is odd
table(ca_rec$RECFIN_YEAR, useNA = "always")
table(ca_rec$AGENCY, useNA = "always")
table(ca_rec$DISTRICT_NAME, useNA = "always")
table(ca_rec$RECFIN_SUBREGION_NAME, useNA = "always")
table(ca_rec$RECFIN_MODE_NAME, useNA = "always")
table(ca_rec$RECFIN_WATER_AREA_NAME, useNA = "always")
table(ca_rec$RECFIN_TRIP_TYPE_NAME, useNA = "always")
table(ca_rec$RECFIN_TRIP_TYPE_NAME, ca_rec$RECFIN_WATER_AREA_NAME, useNA = "always")
table(ca_rec$SPECIES_NAME, useNA = "always")

#Discard mortality ratios are the same for num and mt
plot(ca_rec$SUM_RELEASED_DEAD_NUM/ca_rec$SUM_RELEASED_ALIVE_NUM - 
       ca_rec$SUM_RELEASED_DEAD_MT/ca_rec$SUM_RELEASED_ALIVE_MT)
#Discard mortality is generally < 5% 
plot(ca_rec$SUM_RELEASED_DEAD_NUM/ca_rec$SUM_RELEASED_ALIVE_NUM)
#...but this isn't an accurate measure of discard mortality RATE.
#Want the amount of releases that are dead over the total releases
plot(ca_rec$SUM_RELEASED_DEAD_NUM/(ca_rec$SUM_RELEASED_ALIVE_NUM + ca_rec$SUM_RELEASED_DEAD_NUM))


########################-
## Process the data ----
########################-

##Simplify variables

#Modes
ca_rec$mode <- dplyr::case_when(ca_rec$RECFIN_MODE_NAME == "Party/Charter Boats" ~ "PC",
                                ca_rec$RECFIN_MODE_NAME == "Private/Rental Boats" ~ "PR")
#Districts
ca_rec$district <- dplyr::case_when(grepl("Bay Area", ca_rec$DISTRICT_NAME) ~ "Bay",
                                    grepl("Central", ca_rec$DISTRICT_NAME) ~ "Central",
                                    grepl("Redwood", ca_rec$DISTRICT_NAME) ~ "Redwood",
                                    grepl("Wine", ca_rec$DISTRICT_NAME) ~ "Wine",
                                    grepl("South", ca_rec$DISTRICT_NAME) ~ "South")


##Aggregate across variables. No checking of number is needed since these are public tables

#Break out by fleet modes
aggFleet <- ca_rec %>%
  dplyr::group_by(mode) %>%
  dplyr::summarize(tot_mt = sum(SUM_TOTAL_MORTALITY_MT),
                   dis_mt = sum(SUM_RELEASED_DEAD_MT),
                   land_mt = sum(SUM_RETAINED_MT))
aggFleetYr <- ca_rec %>%
  dplyr::group_by(mode, RECFIN_YEAR) %>%
  dplyr::summarize(tot_mt = sum(SUM_TOTAL_MORTALITY_MT),
                   dis_mt = sum(SUM_RELEASED_DEAD_MT),
                   land_mt = sum(SUM_RETAINED_MT)) %>%
  data.frame()
aggFleetYr$YEAR <- aggFleetYr$RECFIN_YEAR

#Break out by district
aggDist <- ca_rec %>%
  dplyr::group_by(district) %>%
  dplyr::summarize(tot_mt = sum(SUM_TOTAL_MORTALITY_MT),
                   dis_mt = sum(SUM_RELEASED_DEAD_MT),
                   land_mt = sum(SUM_RETAINED_MT))
aggDistYr <- ca_rec %>%
  dplyr::group_by(district, RECFIN_YEAR) %>%
  dplyr::summarize(tot_mt = sum(SUM_TOTAL_MORTALITY_MT),
                   dis_mt = sum(SUM_RELEASED_DEAD_MT),
                   land_mt = sum(SUM_RETAINED_MT)) %>%
  data.frame()

#District and fleet over time
aggDistFleetYr <- ca_rec %>%
  dplyr::group_by(district, mode, RECFIN_YEAR) %>%
  dplyr::summarize(tot_mt = sum(SUM_TOTAL_MORTALITY_MT),
                   dis_mt = sum(SUM_RELEASED_DEAD_MT),
                   land_mt = sum(SUM_RETAINED_MT)) %>%
  data.frame()


#Break out by water area
aggWater <- ca_rec %>%
  dplyr::group_by(RECFIN_WATER_AREA_NAME) %>%
  dplyr::summarize(tot_mt = sum(SUM_TOTAL_MORTALITY_MT),
                   dis_mt = sum(SUM_RELEASED_DEAD_MT),
                   land_mt = sum(SUM_RETAINED_MT))

#Break out by trip type
aggTripType <- ca_rec %>%
  dplyr::group_by(RECFIN_TRIP_TYPE_NAME) %>%
  dplyr::summarize(tot_mt = sum(SUM_TOTAL_MORTALITY_MT),
                   dis_mt = sum(SUM_RELEASED_DEAD_MT),
                   land_mt = sum(SUM_RETAINED_MT))

#Aggregate over years and output catch time series
aggCatch_rec <- ca_rec %>%
  dplyr::group_by(RECFIN_YEAR) %>%
  dplyr::summarize(tot_mt = sum(SUM_TOTAL_MORTALITY_MT),
                   dis_mt = sum(SUM_RELEASED_DEAD_MT),
                   land_mt = sum(SUM_RETAINED_MT)) %>%
  data.frame()
#write.csv(aggCatch_rec, here("data","CAquillback_recfin_catches.csv"), row.names = FALSE)


#################-
## Plotting ----
#################-

ggplot(aggCatch_rec, aes(y = tot_mt, x = RECFIN_YEAR)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Landings (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#By discard/landing
ggplot(aggCatch_rec %>% tidyr::pivot_longer(cols = c(dis_mt, land_mt)), 
       aes(y = value, x = RECFIN_YEAR)) +
  geom_bar(position = "stack", stat = "identity", aes(fill = name)) +
  xlab("Year") +
  ylab("Mortality (MT)") +
  scale_fill_discrete(labels = c("Discard", "Landings")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_blank())
ggsave(here('data_explore_figs',"recfin_mortality.png"),
       width = 6, height = 4)

#Compare current mortality with mortality from the 2021 assessment
#All very similar with greatest difference being slightly less catch in 2020 
#and very slightly higher catch in 2006, 2008 and 2009 this time around
plot(x = aggCatch_rec$RECFIN_YEAR, y = aggCatch_rec$tot_mt, type = "l", lwd = 2, col = "black")
lines(x = catch_recfin_2021$Year, y = catch_recfin_2021$CA_mort_mt, lty = 1, col = "green", lwd= 2)
plot((merge(aggCatch_rec, catch_recfin_2021, by.x = "RECFIN_YEAR", by.y = "Year") %>% 
        dplyr::mutate("diff" = round(tot_mt - CA_mort_mt, 3)))$diff, x = c(2005:2020))


#By fleet type - total mortality
ggplot(aggFleetYr, aes(y = tot_mt, x = RECFIN_YEAR)) +
  geom_bar(position = "stack", stat = "identity", aes(fill = mode)) +
  xlab("Year") +
  ylab("Total mortality (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"recfin_mortality_fleet.png"),
       width = 6, height = 4)


#By district - total mortality
ggplot(aggDistYr, aes(y = tot_mt, x = RECFIN_YEAR)) +
  geom_bar(position = "stack", stat = "identity", aes(fill = district)) +
  xlab("Year") +
  ylab("Total mortality (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"recfin_mortality_district.png"),
       width = 6, height = 4)

ggplot(aggDistYr, aes(y = tot_mt, x = RECFIN_YEAR)) +
  geom_bar(position = "fill", stat = "identity", aes(fill = district)) +
  xlab("Year") +
  ylab("Proporation of total mortality (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"recfin_mortality_district_percent.png"),
       width = 6, height = 4)


#By district and fleet type - total mortality
ggplot(aggDistFleetYr, aes(y = tot_mt, x = RECFIN_YEAR)) +
  geom_bar(position = "stack", stat = "identity", aes(fill = district)) +
  xlab("Year") +
  facet_wrap(~ mode) +
  ylab("Total mortality (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"recfin_mortality_district_fleet.png"),
       width = 6, height = 4)

ggplot(aggDistFleetYr, aes(y = tot_mt, x = RECFIN_YEAR)) +
  geom_bar(position = "fill", stat = "identity", aes(fill = district)) +
  xlab("Year") +
  facet_wrap(~ mode) +
  ylab("Proporation of total mortality (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"recfin_mortality_district_fleet_percent.png"),
       width = 6, height = 4)



#-----------------------------------------------------------------------------#

# Process MRFSS catch data ----

#-----------------------------------------------------------------------------#

##
#Load catch data
##

# MRFSS Recreational - 1980-1989, 1993-2004 Landings mtons
ca_mrfss = read.csv(here("data-raw","MRFSS-CTE510-California-quillback-1980---2004_17.Oct.2024.csv"), header = T)

#Pull 2021 assessment values for comparison. These are recFIN landings + discards
dir = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/output catch"
#dir = "T:/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/output catch"  #Melissa's path
catch_mrfss_2021 = read.csv(file.path(dir,"quillback_ca_mrfss_total_motalityMT.csv"), header = T) %>%
  dplyr::select(c("Year","North", "South"))
catch_mrfss_2021$total_mt <- catch_mrfss_2021$North + catch_mrfss_2021$South


########################-
## Explore the data ----
########################-

#Looking across fields to see if anything is odd
table(ca_mrfss$YEAR, useNA = "always")
table(ca_mrfss$SCI_NAME, useNA = "always")
table(ca_mrfss$MODE, useNA = "always")
table(ca_mrfss$MODE_FX, useNA = "always") #This seems more complete
table(ca_mrfss$MODE, ca_mrfss$MODE_FX, useNA = "always")
table(ca_mrfss$AREA, useNA = "always")
table(ca_mrfss$AREA_X, useNA = "always")
table(ca_mrfss$AREA, ca_mrfss$AREA_X, useNA = "always")
table(ca_mrfss$SP_CODE, useNA = "always")
table(ca_mrfss$ST_NAME, useNA = "always")
table(ca_mrfss$SUB_REG_NAME, useNA = "always")
table(ca_mrfss$WGT_AB1, useNA = "always") #supposed to be in kgs
table(ca_mrfss$WGT_B1, useNA = "always")
table(ca_mrfss$TOT_CAT, useNA = "always") #Estimated total catch (A+B1+B2)
table(ca_mrfss$LANDING, useNA = "always") #Estimate of total harvest (A+B1)
table(ca_mrfss$TOT_CAT - ca_mrfss$LANDING, useNA = "always") #Some difference
table(ca_mrfss$SMP_TRIP) #number of fishers sampled per trip. All above 3

apply(ca_mrfss, 2, FUN = function(x) x = sum(is.na(x))) #Many empty entries
table(ca_mrfss$TYPE, useNA = "always") #Cant find what this means in metadata <- TO DO: FOLLOW UP
table(ca_mrfss$SUB_WGT, useNA = "always") #Cant find what this means in metadata <- TO DO: FOLLOW UP
table(ca_mrfss$TSP_LEN, useNA = "always") #Sum length of A fish 
table(ca_mrfss$TSP_WGT, useNA = "always") #Sum weight of A fish
table(ca_mrfss$TSP_HARV, useNA = "always")
table(ca_mrfss$TSPCLAIM, useNA = "always")
table(ca_mrfss$ESTHARV, useNA = "always") #Estimate of B1
table(ca_mrfss$ESTCLAIM, useNA = "always") #Estimate of A
table(ca_mrfss$SURVEY, useNA = "always") #What is PCPS? Not in metadata  <- TO DO: FOLLOW UP
#Melissa: PCPS is the Party Charter Phone Survey used to estimate CPFV effort; surveys CPFV operators
#88 blanks in the survey type? What are these?
#What are these flags?  <- TO DO: FOLLOW UP
table(ca_mrfss$OUTFLG, useNA = "always") 
table(ca_mrfss$POOL_FLG, useNA = "always")
table(ca_mrfss$EX_FLG, useNA = "always")
table(ca_mrfss$FLAG_WGT, useNA = "always")
#Melissa: weight flag: blank - real weight or weight missing; 0 = real 0; should be alphabetic. I don't have a definition for X
#Plot the various catch amounts
plot(ca_mrfss$WGT_AB1, ca_mrfss$TOT_CAT) #these are not the same
abline(0,1)
#Which field is not entirely clear. Used WGT_AB1 in 2021 based on this discussion 
#https://github.com/pfmc-assessments/california-data/discussions/2 and so plan to 
#use again <- TO DO: SEEK CONFIRMATION


#########################-
## Process the data ----
#########################-

##Simplify variables

#Convert total mortality to mt
ca_mrfss$tot_mt <- ca_mrfss$WGT_AB1/1000

#Modes
ca_mrfss$mode <- dplyr::case_when(ca_mrfss$MODE_FX == 6 ~ "PC",
                                  ca_mrfss$MODE_FX == 7 ~ "PR",
                                  TRUE ~ "OTH")

#Dont have a district or sub-area category to determine location within state


##Aggregate across variables. No checking of number is needed since SMP_TRIP
# (number of fishers sampled) is all > 3 and so catch from any individual record 
# can be shown

#Break out by fleet modes
aggFleet_mrfss <- ca_mrfss %>%
  dplyr::group_by(mode) %>%
  dplyr::summarize(tot_mt = sum(tot_mt, na.rm = TRUE))

aggFleetYr_mrfss <- ca_mrfss %>%
  dplyr::group_by(mode, YEAR) %>%
  dplyr::summarize(tot_mt = sum(tot_mt, na.rm = TRUE)) %>%
  data.frame()


#Break out by water area
aggArea_mrfss <- ca_mrfss %>%
  dplyr::group_by(AREA_X) %>%
  dplyr::summarize(tot_mt = sum(tot_mt, na.rm = TRUE))

aggAreaYr_mrfss <- ca_mrfss %>%
  dplyr::group_by(AREA_X, YEAR) %>%
  dplyr::summarize(tot_mt = sum(tot_mt, na.rm = TRUE)) %>%
  data.frame()


#Aggregate over years and output the catch time series
aggCatch_mrfss <- ca_mrfss %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarize(tot_mt = sum(tot_mt, na.rm = TRUE)) %>%
  data.frame()
#write.csv(aggCatch_mrfss, here("data","CAquillback_mrfss_catches.csv"), row.names = FALSE)



#################-
## Plotting ----
#################-

ggplot(aggCatch_mrfss, aes(y = tot_mt, x = YEAR)) +
  geom_bar(position = "stack", stat = "identity", fill = "#00BFC4") +
  xlab("Year") +
  ylab("Landings (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"mrfss_mortality.png"),
       width = 6, height = 4)

#Compare current mortality with mortality from the 2021 assessment
#These are the same
plot(x = aggCatch_mrfss$YEAR, y = aggCatch_mrfss$tot_mt, type = "l", lwd = 2, col = "black")
lines(x = catch_mrfss_2021$Year, y = catch_mrfss_2021$total_mt, lty = 1, col = "green", lwd= 2)
plot((merge(aggCatch_mrfss, catch_mrfss_2021, by.x = "YEAR", by.y = "Year") %>% 
        dplyr::mutate("diff" = round(tot_mt - total_mt, 3)))$diff, x = c(1980:1989, 1993:2004))


#By fleet type - total mortality
ggplot(aggFleetYr_mrfss, aes(y = tot_mt, x = YEAR)) +
  geom_bar(position = "stack", stat = "identity", aes(fill = mode)) +
  xlab("Year") +
  ylab("Total mortality (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"mrfss_mortality_fleet.png"),
       width = 6, height = 4)

# Plotting MRFSS and RecFIN catches together combined by fleet
aggFleetYr_comb <- rbind(aggFleetYr_mrfss, aggFleetYr[,c("mode", "YEAR", "tot_mt")])
ggplot(aggFleetYr_comb, aes(y = tot_mt, x = YEAR)) +
  geom_bar(position = "stack", stat = "identity", aes(fill = mode)) +
  xlab("Year") +
  ylab("Total mortality (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"rec_comb_mortality_fleet.png"),
       width = 6, height = 4)


#By water area - total mortality
ggplot(aggAreaYr_mrfss, aes(y = tot_mt, x = YEAR)) +
  geom_bar(position = "stack", stat = "identity", aes(fill = factor(AREA_X))) +
  xlab("Year") +
  ylab("Total mortality (MT)") +
  scale_fill_discrete(labels = c("OCEAN (<= 3 MI)", "OCEAN (> 3 MI)", "INLAND", "UNK", "NA")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#-----------------------------------------------------------------------------#

# Process RecFIN bio sampling data ----

#-----------------------------------------------------------------------------#

##
#Load the data
##

# RecFIN Recreational - 2004-2023
ca_bio_rec = read.csv(here("data-raw","RecFIN-SD501-CALIFORNIA-quillback-1983---2023_2.Oct.2024.csv"), header = T)

# If pull the nonconfidential data (SD001) lose five fields, though the last is blank
# [1] "RECFIN_DATE"      "COUNTY_NUMBER"    "INTERVIEW_SITE"  
# [4] "INTERVIEW_TIME"   "CPFV_LOCATION_ID"
# Seems like the conf data is not needed, however we do need it to determine unique trips, so keep
#ca_bio_rec_noconf = read.csv(here("data-raw","RecFIN-SD001-CALIFORNIA-quillback-1983---2023_2.Oct.2024.csv"), header = T)
#colnames(ca_bio_rec)[!(colnames(ca_bio_rec) %in% colnames(ca_bio_rec_noconf))]


########################-
## Explore the data ----
########################-

table(ca_bio_rec$STATE_NAME, useNA = "always")
table(ca_bio_rec$RECFIN_YEAR, useNA = "always")
table(ca_bio_rec$RECFIN_PORT_NAME, useNA = "always")
table(ca_bio_rec$RECFIN_TRIP_TYPE_NAME, useNA = "always")
table(ca_bio_rec$AGENCY_WATER_AREA_NAME, useNA = "always")
table(ca_bio_rec$RECFIN_MODE_NAME, useNA = "always")
table(ca_bio_rec$SPECIES_NAME, useNA = "always")
table(ca_bio_rec$AGENCY_LENGTH_UNITS, useNA = "always") #a few len units are 'm'
table(ca_bio_rec$IS_AGENCY_LENGTH_WITHIN_MAX, useNA = "always") #a few extreme len
table(ca_bio_rec$AGENCY_WEIGHT, useNA = "always") #a lot of unmeasured weights
table(ca_bio_rec$AGENCY_WEIGHT_UNITS, useNA = "always") #a few weight units are 'k'
table(ca_bio_rec$RECFIN_IMPUTED_LENGTH, useNA = "always") #no imputted lengths
table(ca_bio_rec$RECFIN_LENGTH_TYPE, useNA = "always") #unknown length type
table(ca_bio_rec$RECFIN_IMPUTED_WEIGHT_KG, useNA = "always")
table(ca_bio_rec$RECFIN_SEX_CODE, useNA = "always") #nearly all unsexed
table(ca_bio_rec$IS_RETAINED, useNA = "always") #very few released
table(ca_bio_rec$IS_RETAINED, ca_bio_rec$AGENCY_WEIGHT_UNITS, useNA = "always") #and these are of unit "k"
table(ca_bio_rec$SOURCE_CODE)
table(ca_bio_rec$SOURCE_CODE, ca_bio_rec$IS_RETAINED) #those released were from CPFV boats
table(ca_bio_rec$CAUGHT_BY_OBSERVED_ANGLER) #yet those CPFV boats were not observed
table(ca_bio_rec$COUNTY_NUMBER, useNA = "always") #22 without a county number
table(ca_bio_rec$INTERVIEW_SITE, useNA = "always") #37 without interview site

#Check length units
plot(ca_bio_rec$AGENCY_LENGTH) #there appear to be some outliers....
plot(ca_bio_rec$RECFIN_LENGTH_MM) 
plot(ca_bio_rec$AGENCY_LENGTH - ca_bio_rec$RECFIN_LENGTH_MM) #...but these are in mm
ca_bio_rec %>% dplyr::filter(AGENCY_LENGTH_UNITS == "M") #I guess M means mm
#Melissa: M = missing is what it should be, but looks to be the measurements of discarded fish 
ca_bio_rec %>% dplyr::filter(RECFIN_LENGTH_MM > 600)

#There are three fish outside the max: 999, 804, and 640. Of these, the 804 fish
#has a measured weight. The 999 doesn't seem right, and the 640 also doesn't have a weight. 
plot(ca_bio_rec$RECFIN_LENGTH_MM, ca_bio_rec$AGENCY_WEIGHT)
#Im guessing the weights above 4kg are actually pounds other than for this huge one
#which could be accurate. However, these would be all timers so likely anything above
#600 is inaccurate

#Among the weights there are many weights that are imputted. Just a few measured weights
#seem off, though there are some that are unusually precise. 
#Overall if use weight from recfin data, we should exclude these large weights that 
#seem like they are pounds instead of kg, and any measured weights with more than 2 digits
#of precision
#Melissa: the weights will not be as accurate as fishery surveys; I would not recommend using weights from the onboard CPFV
#samples due to the inabaility to accurately weigh fish on a moving vessel

#There does not appear to be any differences in the type of length measurement 
#so assume all are fork length. 

#Among the weight units. K only is used on released fish from CPFV so
#assume all are kg
ca_bio_rec %>% dplyr::filter(AGENCY_WEIGHT_UNITS != "") 



########################-
## Process the data ----
########################-

##Simplify variables

#Modes
ca_bio_rec$mode <- dplyr::case_when(ca_bio_rec$RECFIN_MODE_NAME == "PARTY/CHARTER BOATS" ~ "PC",
                                ca_bio_rec$RECFIN_MODE_NAME == "PRIVATE/RENTAL BOATS" ~ "PR",
                                TRUE ~ NA)

#Area (port although rec data is sampled by district and these are districts)
ca_bio_rec$area <- dplyr::case_when(grepl("BAY AREA", ca_bio_rec$RECFIN_PORT_NAME) ~ "Bay",
                                    grepl("CENTRAL", ca_bio_rec$RECFIN_PORT_NAME) ~ "Central",
                                    grepl("REDWOOD", ca_bio_rec$RECFIN_PORT_NAME) ~ "Redwood",
                                    grepl("WINE", ca_bio_rec$RECFIN_PORT_NAME) ~ "Wine",
                                    grepl("SOUTH", ca_bio_rec$RECFIN_PORT_NAME) ~ "South",
                                    TRUE ~ NA)

#Sexes
#There are five fish with sex codes. Set all to unidentified
ca_bio_rec$sex <- "U"

#From explorations remove any lengths above 600 (this removes three fish: 640, 804, and 999)
#These are flagged within recfin as not being within max
#Also removes 1 fish with NA length
ca_bio_rec <- ca_bio_rec[which(ca_bio_rec$RECFIN_LENGTH_MM < 600), ]

#Set AGENCY_WEIGHT to NA if they are > 4 kg or come from the imputed weight 
ca_bio_rec[which(ca_bio_rec$AGENCY_WEIGHT > 4), "AGENCY_WEIGHT"] <- NA #only three after removing long ones
#We have removed any weights greater than 2 decimal digits in the past, but since all records 
#have inputted weights, and all are different than AGENCY_WEIGHT, my guess is these were
#converted from lbs and therefore are likely to be valid measurements. 
table(nchar(sub('.*\\.', '', ca_bio_rec$AGENCY_WEIGHT))) #just a few have more than two decimals
test <- ca_bio_rec[which(nchar(sub('.*\\.', '', ca_bio_rec$AGENCY_WEIGHT)) > 2),] 

round(test$AGENCY_WEIGHT,4) - test$RECFIN_IMPUTED_WEIGHT_KG
#Of these, none have the same agency weight as imputed weight. So keep all

#Trip
#Following the approach as was used for copper rockfish and canary rockfish in 2023
#See discussion#34 - https://github.com/pfmc-assessments/CA_quillback_rockfish_2025/discussions/34
#Can remove numeric identifier if no issue with confidentiality
ca_bio_rec$tripID <- as.integer(as.factor(
  paste0(ca_bio_rec$RECFIN_DATE, ca_bio_rec$COUNTY_NUMBER, 
         ca_bio_rec$INTERVIEW_SITE, ca_bio_rec$AGENCY_WATER_AREA_NAME, 
         ca_bio_rec$mode)
  ))

#Output basic bio data for later use for analysis and comps
ca_bio_rec$source <- "recfin"
ca_bio_rec$length_cm <- ca_bio_rec$RECFIN_LENGTH_MM/10
out_bio <- ca_bio_rec %>% dplyr::select("Year" = RECFIN_YEAR, 
                                    length_cm,
                                    "weight_kg" = AGENCY_WEIGHT,
                                    sex,
                                    area,
                                    mode,
                                    "disp" = IS_RETAINED,
                                    source,
                                    SOURCE_CODE,
                                    tripID)
#write.csv(out_bio, here("data","CAquillback_rec_bio.csv"), row.names = FALSE)



#################-
## Plotting ----
#################-


#CONCLUSIONS:
#IS_RETAINED doesnt have enough data to be informative. Only on PC
#PC and PR are very similar when compared within areas
#Primary pattern is that fish in Wine and Redwood are larger


#Lengths over time show little variation
ggplot(ca_bio_rec, aes(y = RECFIN_LENGTH_MM, x = RECFIN_YEAR)) +
  geom_point(colour = "#00BFC4")
ggsave(here('data_explore_figs',"recfin_length.png"), 
       width = 6, height = 4)

##
#By disposition (released only on CPFV vessels)
##

#Group dot and density plot together
par(mfrow=c(2,1))
p1 <- ggplot(ca_bio_rec, aes(y = RECFIN_LENGTH_MM, x = RECFIN_YEAR, color = IS_RETAINED)) +
  geom_point()
p2 <- ggplot(ca_bio_rec, aes(x = RECFIN_LENGTH_MM)) +
  geom_density(aes(colour = IS_RETAINED))

grid.arrange(p1, p2, nrow = 2)
g <- gridExtra::arrangeGrob(p1, p2, nrow=2) #generates g
ggsave(here('data_explore_figs',"recfin_length_disposition.png"), g, 
       width = 6, height = 4)

#This is on so few samples that I dont think it is informative (its also only on PC)


##
#By mode
## 

#Modes are either private/charter or private/rental with a few unknown
#PR is catching larger fish
ggplot(ca_bio_rec, aes(y = RECFIN_LENGTH_MM, x = mode)) +
  geom_violin(aes(fill = mode))
ggsave(here('data_explore_figs',"recfin_length_mode_violin.png"), 
       width = 6, height = 4)

#Released fish are only on PC mode, so repeat with only retained fish and use denisty
ggplot(ca_bio_rec %>% dplyr::filter(IS_RETAINED %in% c("RETAINED")),
       aes(x = RECFIN_LENGTH_MM)) +
  geom_density(aes(color = mode))
ggsave(here('data_explore_figs',"recfin_length_mode_density.png"), 
       width = 6, height = 4)

#Not obviously based on water area... 
ggplot(ca_bio_rec %>% dplyr::filter(IS_RETAINED %in% c("RETAINED")),
       aes(x = RECFIN_LENGTH_MM)) +
  geom_density(aes(color = AGENCY_WATER_AREA_NAME))
#...though "not known" is a much more common entry for PC than it is for PR
table(ca_bio_rec$AGENCY_WATER_AREA_NAME, ca_bio_rec$mode)

#By mode and port
ggplot(ca_bio_rec, aes(y = RECFIN_LENGTH_MM, x = mode)) +
  geom_violin(aes(fill = mode)) +
  facet_wrap(~area)
ggsave(here('data_explore_figs',"recfin_length_mode_area_violin.png"), 
       width = 6, height = 4)
#Based on this, PC and PR are similar within each district. 
#Therefore, the fact that PC is smaller than PR must mean that there are more PR
#in Redwood, which is noticeably larger than other districts.
table(ca_bio_rec$area, ca_bio_rec$mode) #More PR in redwood


##
#By areas
##

#by port group (really districts)
ggplot(ca_bio_rec, aes(y = RECFIN_LENGTH_MM, x = RECFIN_YEAR, color = area)) +
  geom_point() +
  labs(color = "District")

ggplot(ca_bio_rec, aes(color = area, y = RECFIN_LENGTH_MM, x = RECFIN_YEAR)) +
  geom_point() + 
  facet_wrap(~area) +
  labs(color = "Port group")
ggsave(here('data_explore_figs',"recfin_length_area.png"), 
       width = 6, height = 4)

ggplot(ca_bio_rec, aes(color = area, y = RECFIN_LENGTH_MM, x = RECFIN_YEAR)) +
  geom_point() + 
  facet_wrap(~area + mode) +
  labs(color = "Port group")
ggsave(here('data_explore_figs',"recfin_length_area_mode.png"), 
       width = 6, height = 4)

#Density plots show larger fish in Redwood (and to an extent Wine) 
ggplot(ca_bio_rec %>% dplyr::filter(!area %in% c("South")), 
       aes(x = RECFIN_LENGTH_MM)) +
  geom_density(aes(colour = area))
ggsave(here('data_explore_figs',"recfin_length_area_density.png"), 
       width = 6, height = 4)

#Fish are also larger in areas where they are longer
ggplot(ca_bio_rec %>% dplyr::filter(!area %in% c("South")), 
       aes(x = AGENCY_WEIGHT)) +
  geom_density(aes(colour = area))
ggsave(here('data_explore_figs',"recfin_weight_area_density.png"), 
       width = 6, height = 4)

#Compare lengths to southern Oregon fish (also remove the two very large lengths near 900, and the one above 600)
#This is 2021 data right now because dont have access to recfin data warehouse
#TO DO: Update it with current data from Oregon. 
or_bio_rec <- read.csv("U:/Stock assessments/quillback_rockish_2021_FRAM network files/Quillback_Rockfish/data/RecFIN Sample Data/Quillback_RecFIN_BIO-LW_2001-2020.csv") %>%
  dplyr::filter(RecFIN.Port.Name %in% c("BROOKINGS", "GOLD BEACH", "PORT ORFORD")) %>%
  dplyr::filter(RecFIN.Length.MM < 600)
ggplot(ca_bio_rec %>% dplyr::filter(!area %in% c("South")), 
       aes(x = RECFIN_LENGTH_MM)) +
  geom_density(aes(colour = area)) +
  geom_density(aes(x = RecFIN.Length.MM, colour = RecFIN.Port.Name), data = or_bio_rec, linetype = 2)
ggsave(here('data_explore_figs',"recfin_length_area_density_withOregon.png"), 
       width = 6, height = 4)

#-----------------------------------------------------------------------------#

# Process MRFSS bio sampling data ----

#-----------------------------------------------------------------------------#

##
#Load the data
##

# MRFSS Recreational - 1980-1989, 1993-2003
ca_mrfss_bio <- read.csv(here("data-raw","MRFSS-SD509-CALIFORNIA-quillback-1980---2003_22.Nov.2024.csv"), header = T)


########################-
## Explore the data ----
########################-

table(ca_mrfss_bio$YEAR)
table(ca_mrfss_bio$ST_NAME)
table(ca_mrfss_bio$CNTY)
table(ca_mrfss_bio$SUB_REG) #Seven are from south of conception, explore more below
table(ca_mrfss_bio$CNTY, ca_mrfss_bio$SUB_REG, useNA = "always")
table(ca_mrfss_bio$DIST) #NOTE this is distance from shore (1 = <3 nm, 2 = >3 nm, 6 = not sure, 8 = NA)
table(ca_mrfss_bio$MODE_FX_NAME) #mode of fishing collapsed
table(ca_mrfss_bio$MODE_F_NAME) #mode of fishing
table(ca_mrfss_bio$MODE_F_NAME, ca_mrfss_bio$MODE_FX_NAME) #best to remove shore based samples
table(ca_mrfss_bio$AREA_X_NAME) #collapsed area of fishing
table(ca_mrfss_bio$AREA_NAME) #area of fishing
table(ca_mrfss_bio$MODE_FX_NAME, ca_mrfss_bio$AREA_NAME) #these dont align with beach fishing. Keep in bay
table(ca_mrfss_bio$SP_CODE)
table(ca_mrfss_bio$INTSITE) #site code
table(ca_mrfss_bio$GEAR) #type of gear (1 = hook and line, 8 = spear, 99 = refused)
table(ca_mrfss_bio$MODE_FX_NAME, ca_mrfss_bio$GEAR) #Spear is on private/rental. Check on lengths
table(ca_mrfss_bio$LNGTH, useNA = "always") #fork length - 38 records without length
table(ca_mrfss_bio$WGT, useNA = "always") #weight in kg
table(ca_mrfss_bio$T_LEN, useNA = "always") #total length - 64 records without T_LEN
table(ca_mrfss_bio$WGT_FLAG) #m=missing r=outlier z=oversize (0 = measured)
table(ca_mrfss_bio$INVALID) #BAD ASCII DATA READ

#Important fields without any information though many more fields also have no entries
apply(ca_mrfss_bio, 2, FUN = function(x) x = sum(is.na(x))) #Many empty entries
table(ca_mrfss_bio$LENGTH)
table(ca_mrfss_bio$LEN)
table(ca_mrfss_bio$DISTRICT) #CRFS Coastal District
table(ca_mrfss_bio$DISPO)
table(ca_mrfss_bio$DISP3) #3 is eaten or plan to eat. Hard to say what the others are.
table(ca_mrfss_bio$PORT)

#Check subregion
#Seven fish are from South of Point Conception. This is unexpected. 
#One is from San Luis Obispo (CNTY = 79) yet has an INTSITE with a record I cant
#corroborate in the region-county-intsite to district cross table file "st_sub_reg_cnty_int_site_lookup_20200914.xlsx"
#Other six dont have information on CNTY location. Overall, keep in.
#From cross table file, CNTY 79 is assigned to the central district
ca_mrfss_bio[ca_mrfss_bio$SUB_REG == 1,]

#Check beach/bank MODE_FX_NAME and spear GEAR to see if sizes are different.
#Sizes aren't obviously different, but plan to remove spear gear
table(ca_mrfss_bio$MODE_FX_NAME, ca_mrfss_bio$GEAR)
plot(ca_mrfss_bio$LNGTH, col = as.factor(ca_mrfss_bio$GEAR), pch=19) #two red circles (spear), one green (refused)
ca_mrfss_bio[which(ca_mrfss_bio$GEAR %in% c(8, 99)),]

#Check length values
#Unclear in the metadata whether these are measured. Some metadata say T_LEN is measured,
#but there are a number of entries with many digits. Other metadata dont say what is measured
plot(ca_mrfss_bio$YEAR, ca_mrfss_bio$LNGTH) 
plot(ca_mrfss_bio$YEAR, ca_mrfss_bio$T_LEN) 
#LNGTH fills in some gaps in T_LEN for 1994 and 1995, otherwise both have values
cbind(table(ca_mrfss_bio$YEAR, is.na(ca_mrfss_bio$T_LEN)), table(ca_mrfss_bio$YEAR, is.na(ca_mrfss_bio$LNGTH)))
#Differences between them are not consistent, but MOST of the time T_len > LNGTH
plot(ca_mrfss_bio$LNGTH, ca_mrfss_bio$T_LEN) 
abline(-8.696, 1.034) #Relationship from Echeverria and Lenarz 1984
plot(ca_mrfss_bio$T_LEN, ca_mrfss_bio$LNGTH) 
abline(9.075, 0.965) #Relationship from Echeverria and Lenarz 1984
plot(ca_mrfss_bio$LNGTH - ca_mrfss_bio$T_LEN)
plot(ca_mrfss_bio$LNGTH, ca_mrfss_bio$LNGTH - ca_mrfss_bio$T_LEN)

#Check number of decimals
table(nchar(sub('.*\\.', '', ca_mrfss_bio$T_LEN))) #no more than three digits
table(nchar(sub('.*\\.', '', ca_mrfss_bio$LNGTH))) #as many as 8 digits
table(nchar(sub('.*\\.', '', ca_mrfss_bio$WGT))) #there are clearly some that are converted

#Check wgt values
#There doesn't seem to be a consistent pattern. While 1, m, o, r, z seem to all have many digits,
#weights with the 0 flag (which is supposedly measured) have many digits too, and thus dont seem measured
plot(ca_mrfss_bio$WGT)
table(ca_mrfss_bio$WGT_FLAG)
table(ca_mrfss_bio$WGT,ca_mrfss_bio$WGT_FLAG)
#The plot of weight and length make it seem pretty obvious most of these are imputted
plot(ca_mrfss_bio$WGT, ca_mrfss_bio$LNGTH)
#From recfin, there were a few records of large individuals (> 4kg) that seemed to be pounds and not kg. 
#However here, the two fish >4kg are also very long, and measurements are one digit. Thus it appears these
#are indeed kgs so dont through out based on them being large
ca_mrfss_bio[which(ca_mrfss_bio$WGT>4),]


########################-
## Process the data ----
########################-

##Remove unrepresentative records

#Remove the 38 records without LNGTH
ca_mrfss_bio <- ca_mrfss_bio[!is.na(ca_mrfss_bio$LNGTH),]

#Remove the 4 shore based fishing (dock/pier and jetty) and the 2 beach bank samples
ca_mrfss_bio <- ca_mrfss_bio[which(ca_mrfss_bio$MODE_FX_NAME %in% c("Party/Charter Boat", "Private/Rental Boat")),]

#Remove the 2 spear gear records
ca_mrfss_bio <- ca_mrfss_bio[which(ca_mrfss_bio$GEAR %in% c(1,99)),]


##Simplify variables

#Modes
ca_mrfss_bio$mode <- dplyr::case_when(ca_mrfss_bio$MODE_FX_NAME == "Party/Charter Boat" ~ "PC",
                                    ca_mrfss_bio$MODE_FX_NAME == "Private/Rental Boat" ~ "PR",
                                    TRUE ~ NA)

#Area 
#Read in the lookup table for CNTY-INTSITE to District
#Only CNTY 23 is potentially split across districts (INTSITE 106 is Wine, all others are Redwood)
lookup <- data.frame(read_excel(here("data-raw", "st_sub_reg_cnty_int_site_lookup_20200914.xlsx"), 
                                sheet = "INTSITE"))
table(lookup$DISTRICT_NAME,lookup$CNTY)
table(ca_mrfss_bio[(ca_mrfss_bio$CNTY == 23), "INTSITE"], useNA="always") 
#Order it this way so that CNTY 23 is assigned to redwood...
ca_mrfss_bio$area <- dplyr::case_when(ca_mrfss_bio$CNTY %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 1), "CNTY"]) ~ "South",
                                      ca_mrfss_bio$CNTY %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 2), "CNTY"]) ~ "Channel",
                                      ca_mrfss_bio$CNTY %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 3), "CNTY"]) ~ "Central",
                                      ca_mrfss_bio$CNTY %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 4), "CNTY"]) ~ "Bay",
                                      ca_mrfss_bio$CNTY %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 6), "CNTY"]) ~ "Redwood",
                                      ca_mrfss_bio$CNTY %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 5), "CNTY"]) ~ "Wine",
                                      TRUE ~ NA)
#...then assign CNTY 23 with INTSITE 106 to wine. Therefore, Im assuming CNTY is Redwood unless known to be Wine
ca_mrfss_bio[which(ca_mrfss_bio$CNTY == 23 & ca_mrfss_bio$INTSITE == 106), "area"] <- "Wine"

#Sexes
#F_sex has records of 8, which does not align with metadata so assume unknown
ca_mrfss_bio$sex <- "U"

#Disposition
#DISP3 lists out many with value 3 (which in metadata corresponds to DISP = 3 which are "eaten").
#Assume all are retained
ca_mrfss_bio$disp <- "RETAINED"

#Add additional weight and length flag 
#Base on the number of decimals for measured (decimals < 2), calculated (decimals > 2), or missing (NA) values
#Need to divide LNGTH by 100 because sub() includes numbers to the left of the decimal and these are either whole of many decimal
#Need to divide T_LEN by 100 and (decimals < or > 4) because have 0, 2, or 3 decimals
ca_mrfss_bio$lngth_flag = ca_mrfss_bio$tlen_flag = ca_mrfss_bio$wgt_flag = "missing"
ca_mrfss_bio[which(sapply(ca_mrfss_bio$LNGTH, function(x) nchar(sub('.*\\.', '', x/100))) > 2), "lngth_flag"] = "computed"
ca_mrfss_bio[which(sapply(ca_mrfss_bio$LNGTH, function(x) nchar(sub('.*\\.', '', x/100))) <= 2), "lngth_flag"] = "measured"
ca_mrfss_bio[which(sapply(ca_mrfss_bio$T_LEN, function(x) nchar(sub('.*\\.', '', x/100))) > 4), "tlen_flag"] = "computed"
ca_mrfss_bio[which(sapply(ca_mrfss_bio$T_LEN, function(x) nchar(sub('.*\\.', '', x/100))) <= 4), "tlen_flag"] = "measured"
ca_mrfss_bio[which(sapply(ca_mrfss_bio$WGT, function(x) nchar(sub('.*\\.', '', x))) > 2), "wgt_flag"] = "computed"
ca_mrfss_bio[which(sapply(ca_mrfss_bio$WGT, function(x) nchar(sub('.*\\.', '', x))) <= 2), "wgt_flag"] = "measured"
#Per discussion #2 in the california-data github, LNGTH was calculated before 1993 but has added data in 1997-1998 (I wonder if this is the dup Deb-WV data)
plot(ca_mrfss_bio$YEAR, ca_mrfss_bio$LNGTH, col = factor(ca_mrfss_bio$lngth_flag))
plot(ca_mrfss_bio$YEAR, ca_mrfss_bio$T_LEN, col = factor(ca_mrfss_bio$tlen_flag)) #not as clear of a split (looks like odd ones are the 1 decimal values)
plot(ca_mrfss_bio$YEAR, ca_mrfss_bio$WGT, col = factor(ca_mrfss_bio$wgt_flag))
#Can get measured weights and lengths together to use as weight length curve should we wish
plot(ca_mrfss_bio$WGT, ca_mrfss_bio$LNGTH, col = factor(ca_mrfss_bio$lngth_flag))
plot(ca_mrfss_bio$WGT, ca_mrfss_bio$LNGTH, col = factor(ca_mrfss_bio$wgt_flag))
plot(ca_mrfss_bio$WGT, ca_mrfss_bio$LNGTH, col = factor(ca_mrfss_bio$wgt_flag == "measured" & ca_mrfss_bio$lngth_flag  == "measured"))

#Trip
#Following the approach as was used for copper rockfish and canary rockfish in 2023 but
#since the number of trips is the same as when just using ID_CODE, just use ID_CODE 
#See discussion#34 - https://github.com/pfmc-assessments/CA_quillback_rockfish_2025/discussions/34
#Can remove numeric identifier if no issue with confidentiality
length(unique(ca_mrfss_bio$ID_CODE))
length(unique(paste0(ca_mrfss_bio$YEAR, ca_mrfss_bio$ID_CODE, ca_mrfss_bio$INTSITE,
                     ca_mrfss_bio$AREA_X, ca_mrfss_bio$MODE_FX)))
ca_mrfss_bio$tripID <- as.integer(as.factor(ca_mrfss_bio$ID_CODE))


#Output basic bio data for later use for analysis and comps
ca_mrfss_bio$source <- "mrfss"
ca_mrfss_bio$length_cm <- ca_mrfss_bio$LNGTH/10
out_mrfss_bio <- ca_mrfss_bio %>% dplyr::select("Year" = YEAR, 
                                        length_cm,
                                        "weight_kg" = WGT,
                                        sex,
                                        area,
                                        mode,
                                        disp,
                                        lngth_flag,
                                        wgt_flag,
                                        source,
                                        tripID)
#write.csv(out_mrfss_bio, here("data","CAquillback_mrfss_bio.csv"), row.names = FALSE)


#################-
## Plotting ----
#################-

#Lengths over time show some decline
ggplot(ca_mrfss_bio, aes(y = LNGTH, x = YEAR)) +
  geom_point(colour = "#00BFC4")
ggsave(here('data_explore_figs',"mrfss_length.png"), 
       width = 6, height = 4)

##
#No disposition information
##

##
#By mode
## 

#PR is catching larger fish
ggplot(ca_mrfss_bio, aes(y = LNGTH, x = mode)) +
  geom_violin(aes(fill = mode))
ggsave(here('data_explore_figs',"mrfss_length_mode_violin.png"), 
       width = 6, height = 4)

#Not much difference between PR and PC by mode other than in Redwood
ggplot(ca_mrfss_bio, aes(y = LNGTH, x = mode)) +
  geom_violin(aes(fill = mode)) +
  facet_wrap(~area)
ggsave(here('data_explore_figs',"mrfss_length_mode_area_violin.png"), 
       width = 6, height = 4)
#Based on this, PC and PR are similar within each district. 
#Therefore, the fact that PC is smaller than PR must mean that there are more PR
#in Redwood, which is noticeably larger than other districts.
table(ca_mrfss_bio$area, ca_mrfss_bio$mode) #More PR in redwood. Saw this in RecFIN too

#Using a density figure for mode and area
ggplot(ca_mrfss_bio, aes(x = LNGTH)) +
  geom_density(aes(color = mode)) +
  facet_wrap(~area)


##
#By areas
##

#by port group (really districts)
ggplot(ca_mrfss_bio, aes(y = LNGTH, x = YEAR, color = area)) +
  geom_point() +
  labs(color = "District")

#Was there some special sampling done in redwood. Nothing is really less than 300 mm
ggplot(ca_mrfss_bio, aes(color = area, y = LNGTH, x = YEAR)) +
  geom_point() + 
  facet_wrap(~area) +
  labs(color = "District")
ggsave(here('data_explore_figs',"mrfss_length_area.png"), 
       width = 6, height = 4)

ggplot(ca_mrfss_bio, aes(color = area, y = LNGTH, x = YEAR)) +
  geom_point() + 
  facet_wrap(~area + mode) +
  labs(color = "DISTRICT")
ggsave(here('data_explore_figs',"mrfss_length_area_mode.png"), 
       width = 6, height = 4)

#Density plots show larger fish in Redwood (and to an extent Wine) 
ggplot(ca_mrfss_bio, aes(x = LNGTH)) +
  geom_density(aes(colour = area))
ggsave(here('data_explore_figs',"mrfss_length_area_density.png"), 
       width = 6, height = 4)

#Fish are also larger in areas where they are longer
ggplot(ca_mrfss_bio %>% dplyr::filter(!area %in% c("South")), 
       aes(x = WGT)) +
  geom_density(aes(colour = area))
ggsave(here('data_explore_figs',"mrfss_weight_area_density.png"), 
       width = 6, height = 4)



#-----------------------------------------------------------------------------#

# Process historical bio sampling data ----

#-----------------------------------------------------------------------------#

########################-
## Deb Wilson-Vandenberg dataset 1987-1998 ----
########################-

# Pull 2021 assessment file for deb length data.
#dir = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/postSSC_request_data"
#deb_bio = data.frame(read_excel(file.path(dir,"Quillback Rockfish Length Data from Central California Onboard Sampling_Jul12_2021.xlsx"), sheet = "Sheet1"))

# This file was provided by CDFW in 2021 but does not have trip specific information. 
# For the 2021 assessment we assumed number of fish per year equaled number of trips. 
# The original data has trip info though so we should use that.

# #--------------COMMENTING THIS SECTION OUT. USED R-32BIT TO PULL AND SAVE AS A CSV----------#
#
# #HAVE TO USE R-32BIT VERSION though and
# #these files are confidential, so access is restricted.
# #To avoid recopying the data, pull it from an existing location
# 
# library(RODBC)
# 
# if(Sys.getenv("USERNAME") == "Brian.Langseth") {
#   deb_dir <- "//nwcfile.nmfs.local/Home/Brian.Langseth/Stock assessments/lingcod_2021/Lingcod_2021"
#   save_dir <- "//nwcfile.nmfs.local/Home/Brian.Langseth/Stock assessments/CA_quillback_rockfish_2025"
#   }
# 
# deb <- file.path(deb_dir, "data-raw", "CPFV-Onboard Data.mdb")
# conDeb <- odbcConnectAccess(deb)
# RODBC::sqlTables(conDeb)
# deb.trip <- RODBC::sqlFetch(conDeb, "AllTrp")
# deb.len <- RODBC::sqlFetch(conDeb, "Length")
# deb.spc <- RODBC::sqlFetch(conDeb, "SppCode_4_Digit")
# deb.port <- RODBC::sqlFetch(conDeb, "Port_Cmplx")
# RODBC::odbcCloseAll()
# 
# #Add trip numbers and species names and county names. 
# #Save as csv so dont have to deal with RODBC again
# deb.data1 <- merge(deb.len, deb.trip, by = "TRIPNOSAMP")
# deb.data2 <- merge(deb.data1, deb.spc, by = "SP")
# deb.data3 <- merge(deb.data2, deb.port[,c("PORT", "PORTNAME", "COUNTY", "PORTCPLX")], by = "PORT")
# #write.csv(deb.data3, file.path(save_dir, "data-raw", "CONFIDENTIAL_debWV_lengths_AllSpecies.csv"), row.names = FALSE)
#
# #--------------END OF COMMENTED SECTION--------------#

## Load and process deb data with quillback only

deb_bio <- read.csv(here("data-raw", "CONFIDENTIAL_debWV_lengths_AllSpecies.csv"), header = T) %>%
  dplyr::filter(COMMON == "Quillback rockfish")

table(deb_bio$FATE, useNA = "always")
table(deb_bio$LANDING, useNA = "always")
table(deb_bio$PORTNAME, useNA = "always")
table(deb_bio$COUNTY, useNA = "always")

#Assign Countys in Deb's data to district from the lookup table. 
#Have to load two worksheets from the lookup table:
#First to get the county number, second to assign county number to district.
#Although county 23 (Humboldt) has one INTSITE that is assigned to Wine, the port is Eureka
#which is Redwood
cty_lookup <- data.frame(read_excel(here("data-raw", "st_sub_reg_cnty_int_site_lookup_20200914.xlsx"), 
                                sheet = "CNTY")) %>% dplyr::filter(ST == 6)
lookup <- data.frame(read_excel(here("data-raw", "st_sub_reg_cnty_int_site_lookup_20200914.xlsx"), 
                                sheet = "INTSITE"))

deb_bio$cnty <- sapply(deb_bio$COUNTY, FUN = function(x) cty_lookup[which(x == cty_lookup$COUNTY_NAME), "CNTY"], USE.NAMES = FALSE)
deb_bio$area <- dplyr::case_when(deb_bio$cnty %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 1), "CNTY"]) ~ "South",
                                 deb_bio$cnty %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 2), "CNTY"]) ~ "Channel",
                                 deb_bio$cnty %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 3), "CNTY"]) ~ "Central",
                                 deb_bio$cnty %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 4), "CNTY"]) ~ "Bay",
                                 deb_bio$cnty %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 6), "CNTY"]) ~ "Redwood",
                                 deb_bio$cnty %in% unique(lookup[which(lookup$DISTRICT_NUMBER == 5), "CNTY"]) ~ "Wine",
                                 TRUE ~ NA)

## Simplify variables

table(deb_bio$FATE) #all kept
deb_bio$disp <- "RETAINED"
deb_bio$source <- "deb"
deb_bio$sex <- "U"
deb_bio$mode <- "PC"
deb_bio$weight_kg <- NA

deb_bio$length_cm <- (9.075 + 0.965*deb_bio$TL)/10 #get to FL. From echeverria and lenarz 1984
plot(deb_bio$length_cm-deb_bio$TL/10)


## Output basic bio data for later use for analysis and comps
out_deb <- deb_bio %>% dplyr::select("Year" = YEAR,
                                     length_cm,
                                     weight_kg,
                                     sex,
                                     area,
                                     mode,
                                     disp,
                                     source,
                                     "tripID" = AllTRPID)

#write.csv(out_deb, here("data","CAquillback_deb_bio.csv"), row.names = FALSE)


## Check for duplicates of MRFSS data

dups1 <- ca_mrfss_bio %>% dplyr::filter(YEAR %in% c(1997:1998), mode == "PC")
dups1$MONTH <- as.integer(substr(dups1$ID_CODE, 10,11))
dups1$DAY <- as.integer(substr(dups1$ID_CODE, 12,13))
dups1 <- dups1 %>% dplyr::select(YEAR, MONTH, DAY, T_LEN, LNGTH, CNTY)

dups2 <- deb_bio %>% dplyr::filter(YEAR %in%  c(1997:1998)) %>% 
  dplyr::select(YEAR, MONTH, DAY, TL, cnty)

#Plot lengths by day for each dataset 
plot(dups1$DAY, dups1$T_LEN) #MRFSS
points(dups2$DAY, dups2$TL, col = 2) #Deb data
#There are some data that are different. These appear to be ones where T_LEN was not measured
plot(dups1$DAY, dups1$T_LEN, col = (dups1$T_LEN %% 1 == 0)) #plots only T_LEN that are integers
points(dups2$DAY, dups2$TL, col = 2) #Deb data 

#Combine to determine how many are duplicated
dups1b <- dups1[which(dups1$T_LEN %% 1 == 0),] %>% 
  dplyr::select(YEAR, MONTH, DAY, T_LEN, CNTY) %>%
  dplyr::rename(., TL = "T_LEN", "cnty" = CNTY)
dups1b$source = "mrfss"
dups2$source = "deb"
comb <- rbind(dups1b, dups2)
test <- comb[order(comb$YEAR, comb$MONTH, comb$DAY, comb$TL),]
#There are four data points in deb that aren't in mrfss
#The last one matches the length of a single data point in mrfss that isn't duplicated
#My gess is that the day is different but that this fish is the same
#      Year Month Day  TL cnty source
# 123  1997    03  23 308   97    deb
# 510  1997    04  19 302   97    deb
# 610  1997    05  26 337   97    deb
# 65   1998    04  11 260   97  mrfss
# 74   1998    04  18 260   13    deb
plot(dups2$DAY, dups2$TL, col = 2) #Deb data 
points(dups1$DAY, dups1$T_LEN, col = (dups1$T_LEN %% 1 == 0)) #plots only T_LEN that are integers

#Check conversions from TL to FL within duplicated MRFSS data
dups1$convertFL <- 9.075 + dups1$T_LEN*0.965
plot(dups1$convertFL - dups1$LNGTH) #doesnt match up perfectly

# Altogether the 1997 and 1998 appear duplicated. Use Deb data in these years to 
# replace PC mode lengths from MRFSS




########################-
## Miller and Gotshall 1957-1972 ----
########################-

## Load the data - Table 3 is of lengths
# Quillback lengths only for 1959-1960
milgot_bio <- data.frame(read_excel(here("data-raw", "Miller_Gotshall_catch_length_data.xlsx"), sheet = "Table 3",
                                    col_types = c(rep("guess",7),"text")))

#Check variables
table(milgot_bio$Year)
table(milgot_bio$Month)
table(milgot_bio$Fishery)
table(milgot_bio$Species)
table(milgot_bio$Port)
table(milgot_bio$other.notes..comments)


## Process data

#Retain only quillback lengths
milgot_bio <- milgot_bio %>% dplyr::filter(Species == "QLBK")
#Expand dataset so each row equals one length (will remove the count column)
milgot_bio <- milgot_bio %>% tidyr::uncount(Count)


## Simplify variables

milgot_bio$length_cm <- milgot_bio$Length..cm.
milgot_bio$weight_kg <- NA
#Replace port with district
milgot_bio$area <- dplyr::case_when(milgot_bio$Port == "Bodega Bay" ~ "Bay",
                                    milgot_bio$Port == "Ft. Bragg" ~ "Wine",
                                    milgot_bio$Port == "Princeton" ~ "Bay",
                                    milgot_bio$Port == "Shelter Cove" ~ "Wine",
                                    TRUE ~ NA)
milgot_bio$mode <- dplyr::case_when(milgot_bio$Fishery %in% c("skiff", "Skiff") ~ "PR",
                                    milgot_bio$Fishery == "CPFV" ~ "PC",
                                    TRUE ~ NA)
milgot_bio$sex <- "U"
milgot_bio$disp <- "RETAINED"
milgot_bio$source <- "MilGot"

#No real way to determine number of trips. The number of fish on a 
#year-month-port-mode combination is never larger than the number of possible
#trips for that same combination. Suggest use number of unique
#year-month-port-mode combinations instead of number of fish.
milgot_bio %>% dplyr::count(Year, Month, Port, mode)
milgot_bio$tripID <- paste0(milgot_bio$Year, milgot_bio$Month,
                            milgot_bio$Port, milgot_bio$mode)

out_milgot <- milgot_bio %>% dplyr::select(Year,
                                           length_cm,
                                           weight_kg,
                                           sex,
                                           area,
                                           mode,
                                           disp,
                                           source,
                                           tripID)


########################-
## Miller and Geibel 1959-1960 ----
########################-

## Load the data

milgei_bio <- data.frame(read_excel(here("data-raw", "bio_quillback_rockfish_Miller_NorCal_Lengths_59_72.xlsx"), sheet = "Sheet1"))

#Check variables
table(milgei_bio$YEAR)
table(milgei_bio$NUMBER_OF_FISH)
table(milgei_bio$COAST_DIST)
table(milgei_bio$Counties,milgei_bio$COAST_DIST) #Coast_dist must be district
table(milgei_bio$MODE)
table(milgei_bio$mode_description)
table(milgei_bio$Counties)


## Process data

#Expand dataset so each row equals one length (will remove the count column)
milgei_bio <- milgei_bio %>% tidyr::uncount(NUMBER_OF_FISH)


## Simplify variables

milgei_bio$length_cm <- milgei_bio$LENGTH/10
milgei_bio$weight_kg <- NA
#Replace port with district
milgei_bio$area <- dplyr::case_when(milgei_bio$Counties == "DelNorte_Humboldt" ~ "Redwood",
                                    milgei_bio$Counties == "Mendocino_Sonoma" ~ "Wine",
                                    milgei_bio$Counties == "SanFranciscoBay" ~ "Bay",
                                    milgei_bio$Counties == "SantaCruz_Monterey" ~ "Central",
                                    TRUE ~ NA)
milgei_bio$mode <- dplyr::case_when(milgei_bio$mode_description == "private_rental" ~ "PR",
                                    milgei_bio$mode_description == "party_boat" ~ "PC",
                                    TRUE ~ NA)
milgei_bio$sex <- "U"
milgei_bio$disp <- "RETAINED"
milgei_bio$source <- "MilGei"

#No real way to determine number of trips. Does not have month so will have to use
#year-district-mode. Doesn't produce very many trips but this is the finest level
#of detail with have
milgei_bio %>% dplyr::count(YEAR, area, mode)
milgei_bio$tripID <- paste0(milgei_bio$YEAR, milgei_bio$Counties, milgei_bio$mode)

out_milgei <- milgei_bio %>% dplyr::select("Year" = YEAR,
                                           length_cm,
                                           weight_kg,
                                           sex,
                                           area,
                                           mode,
                                           disp,
                                           source,
                                           tripID)

#write.csv(rbind(out_milgot, out_milgei), here("data","CAquillback_historical_bio.csv"), row.names = FALSE)


#################-
### Plotting of Miller and G*s----
#################-

hist_bio <- rbind(out_milgot, out_milgei)

#Lengths over time show some decline
ggplot(hist_bio, aes(y = length_cm*10, x = Year)) +
  geom_point(colour = "#00BFC4")

##
#By mode
## 

#PR is catching larger fish
ggplot(hist_bio, aes(y = length_cm*10, x = mode)) +
  geom_violin(aes(fill = mode))

#Cant really distinguish here. Not enough contrast other than for wine
ggplot(hist_bio, aes(y = length_cm*10, x = mode)) +
  geom_violin(aes(fill = mode)) +
  facet_wrap(~area)

#Using a density figure for mode and area
ggplot(hist_bio, aes(x = length_cm*10)) +
  geom_density(aes(color = mode)) +
  facet_wrap(~area)


##
#By areas
##

ggplot(hist_bio, aes(y = length_cm*10, x = Year, color = area)) +
  geom_point() +
  labs(color = "District")

ggplot(hist_bio, aes(color = area, y = length_cm*10, x = Year)) +
  geom_point() + 
  facet_wrap(~area) +
  labs(color = "District")

ggplot(hist_bio, aes(color = area, y = length_cm*10, x = Year)) +
  geom_point() + 
  facet_wrap(~area + mode) +
  labs(color = "DISTRICT")

#Density plots show larger fish in Redwood (and to an extent Wine) 
ggplot(hist_bio, aes(x = length_cm*10)) +
  geom_density(aes(colour = area))


##
#By data set
##

#Similar across data sources

ggplot(hist_bio, aes(y = length_cm*10, x = source)) +
  geom_violin(aes(fill = source)) + 
  facet_wrap(~Year)

ggplot(hist_bio, aes(x = length_cm*10)) +
  geom_density(aes(colour = source)) +
  facet_wrap(~area)

ggplot(hist_bio, aes(x = length_cm*10)) +
  geom_density(aes(colour = source)) +
  facet_wrap(~mode)


########################-
## Geibel and Collier 1992-1998 ----
########################-

## Load the data

geicol_bio <- data.frame(read.csv(here("data-raw", "Geibel_Collier_qlbk_lengths.csv")))


#Check variables
table(geicol_bio$YEAR)
table(geicol_bio$DAY)
table(geicol_bio$MONTH)
table(geicol_bio$PORTNAME)
table(geicol_bio$COUNTY)
table(geicol_bio$LENGTH)


## Simplify variables

geicol_bio$length_cm <- geicol_bio$LENGTH/10
geicol_bio$weight_kg <- NA

#Replace port with district - all from Redwood
geicol_bio$area <- "Redwood"

#all PR                                  
geicol_bio$mode <- "PR"
geicol_bio$sex <- "U"
geicol_bio$disp <- "RETAINED"
geicol_bio$source <- "GeiCol"

#Get the number of trips from a combo of variables
#There is no real way to match the effort data to lengths from the same trip
#Fish on the same day and port could be from the same trip or not...
geicol_bio %>% dplyr::count(YEAR, MONTH, DAY, PORT)
geicol_bio$tripID <- paste0(geicol_bio$YEAR, geicol_bio$MONTH, 
                            geicol_bio$DAY, geicol_bio$PORT)

geicol_bio %>% dplyr::count(PORTNAME)



out_geicol <- geicol_bio %>% dplyr::select("Year" = YEAR,
                                           length_cm,
                                           weight_kg,
                                           sex,
                                           area,
                                           mode,
                                           disp,
                                           source,
                                           tripID)
#write.csv(out_geicol), here("data","CAquillback_historical_bio_skiff.csv"), row.names = FALSE)
#Melissa - can merge this with other data if you'd like
#Brian - I think we can keep this separate, much like Deb's data. 


#################-
### Plotting of Giebel and Collier's data----
#################-

#Lengths similar over time
ggplot(out_geicol, aes(y = length_cm*10, x = Year)) +
  geom_point(colour = "#00BFC4")

##
#By mode
## 

#Smaller fish in 1998. Other yeas fairly consistent
ggplot(out_geicol, aes(y = length_cm*10, x = as.factor(Year))) +
  geom_violin(aes(fill = as.factor(Year))) 

#Using a density figure for year - all fairly similar
ggplot(out_geicol, aes(x = length_cm*10)) +
  geom_density(aes(color = as.factor(Year))) 


##
#By Port
##

ggplot(geicol_bio, aes(y = length_cm*10, x = YEAR, color = PORTNAME)) +
  geom_point() +
  labs(color = "Port")

ggplot(geicol_bio, aes(color = PORTNAME, y = length_cm*10, x = as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~PORTNAME) +
  labs(color = "District")

#Density plots show slightly larger fish in Crescent City 
#Its strange that the lenght and effort data have different port codings
ggplot(geicol_bio, aes(x = length_cm*10)) +
  geom_density(aes(colour = PORTNAME))
