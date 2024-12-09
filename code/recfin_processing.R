##############################################################################################################
#
# 	Purpose: Explore RecFIN/MRFSS quillback rockfish landings/discards 
#            before putting into form for use in SS. Also to explore
#            biological data
#
#   Created: Oct 3, 2024
#			  by Brian Langseth 
#
##############################################################################################################

library(here)
library(ggplot2)
library(magrittr)
#devtools::install_github("pfmc-assessments/PacFIN.Utilities")
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

# MRFSS Recreational - 1980-2004 Landings mtons
ca_mrfss = read.csv(here("data-raw","MRFSS-CTE510-California-quillback-1980---2004_17.Oct.2024.csv"), header = T)

#Pull 2021 assessment values for comparison. These are recFIN landings + discards
dir = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/output catch"
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
#What are these flags?  <- TO DO: FOLLOW UP
table(ca_mrfss$OUTFLG, useNA = "always") 
table(ca_mrfss$POOL_FLG, useNA = "always")
table(ca_mrfss$EX_FLG, useNA = "always")
table(ca_mrfss$FLAG_WGT, useNA = "always")

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


#By water area - total mortality
ggplot(aggAreaYr_mrfss, aes(y = tot_mt, x = YEAR)) +
  geom_bar(position = "stack", stat = "identity", aes(fill = factor(AREA_X))) +
  xlab("Year") +
  ylab("Total mortality (MT)") +
  scale_fill_discrete(labels = c("OCEAN (<= 3 MI)", "OCEAN (> 3 MI)", "INLAND", "UNK", "NA")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#-----------------------------------------------------------------------------#

# Load RecFIN bio sampling data ----

#-----------------------------------------------------------------------------#

##
#Load the data
##

# RecFIN Recreational - 2004-2023
ca_bio_rec = read.csv(here("data-raw","RecFIN-SD001-CALIFORNIA-quillback-1983---2023_2.Oct.2024.csv"), header = T)

# If pull the confidential data (SD501) have five additional fields, though the last is blank
# [1] "RECFIN_DATE"      "COUNTY_NUMBER"    "INTERVIEW_SITE"  
# [4] "INTERVIEW_TIME"   "CPFV_LOCATION_ID"
# Seems like the conf data is not needed
#ca_bio_recfin_conf = read.csv(here("data-raw","RecFIN-SD501-CALIFORNIA-quillback-1983---2023_2.Oct.2024.csv"), header = T)
#colnames(ca_bio_recfin_conf)[!(colnames(ca_bio_recfin_conf) %in% colnames(ca_bio_recfin))]



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

#Check length units
plot(ca_bio_rec$AGENCY_LENGTH) #there appear to be some outliers....
plot(ca_bio_rec$RECFIN_LENGTH_MM) 
plot(ca_bio_rec$AGENCY_LENGTH - ca_bio_rec$RECFIN_LENGTH_MM) #...but these are in mm
ca_bio_rec %>% dplyr::filter(AGENCY_LENGTH_UNITS == "M") #I guess M means mm
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

#Output basic bio data for later use for analysis and comps
ca_bio_rec$source <- "recfin"
out_bio <- ca_bio_rec %>% dplyr::select("Year" = RECFIN_YEAR, 
                                    "length_cm" = RECFIN_LENGTH_MM,
                                    "weight_kg" = AGENCY_WEIGHT,
                                    sex,
                                    area,
                                    mode,
                                    "disp" = IS_RETAINED,
                                    source,
                                    SOURCE_CODE)
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


#-----------------------------------------------------------------------------#

# Load MRFSS bio sampling data ----

#-----------------------------------------------------------------------------#

##
#Load the data
##
# MRFSS Recreational - 1980-2003
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
abline(0,1)
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


#Output basic bio data for later use for analysis and comps
ca_mrfss_bio$source <- "mrfss"
out_mrfss_bio <- ca_mrfss_bio %>% dplyr::select("Year" = YEAR, 
                                        "length_cm" = LNGTH,
                                        "weight_kg" = WGT,
                                        sex,
                                        area,
                                        mode,
                                        disp,
                                        lngth_flag,
                                        wgt_flag,
                                        source)
#write.csv(out_mrfss_bio, here("data","CAquillback_mrfss_bio.csv"), row.names = FALSE)

#################-
## Plotting ----
#################-