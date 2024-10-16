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

#-----------------------------------------------------------------------------#

# Load catch data ----

#-----------------------------------------------------------------------------#
# RecFIN Recreational - 2005-2023 Landings mtons
ca_rec = read.csv(here("data-raw","RecFIN-CTE001-California-quillback-1990---2023_2.Oct.2024.csv"), header = T)

#Pull 2021 assessment values for comparison. These are recFIN landings + discards
dir = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/output catch"
catch2021 = read.csv(file.path(dir,"recreational_catch_by_area_model_Feb2021.csv"), header = T) %>%
  dplyr::select(c("Year","CA_mort_mt"))



##############################################################################-
## Explore the data ----
##############################################################################-

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


##############################################################################-
## Process the data ----
##############################################################################-

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

#Aggregate over years
aggCatch <- ca_rec %>%
  dplyr::group_by(RECFIN_YEAR) %>%
  dplyr::summarize(tot_mt = sum(SUM_TOTAL_MORTALITY_MT),
                   dis_mt = sum(SUM_RELEASED_DEAD_MT),
                   land_mt = sum(SUM_RETAINED_MT)) %>%
  data.frame()
#write.csv(aggCatch, here("data","CAquillback_recfin_catches.csv"), row.names = FALSE)



################-
## Plotting ----
################-

ggplot(aggCatch, aes(y = tot_mt, x = RECFIN_YEAR)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Landings (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#By discard/landing
ggplot(aggCatch %>% tidyr::pivot_longer(cols = c(dis_mt, land_mt)), 
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
plot(x = aggCatch$RECFIN_YEAR, y = aggCatch$tot_mt, type = "l", lwd = 2, col = "black")
lines(x = catch2021$Year, y = catch2021$CA_mort_mt, lty = 1, col = "green", lwd= 2)
plot((merge(aggCatch, catch2021, by.x = "RECFIN_YEAR", by.y = "Year") %>% 
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

# Load bio sampling data ----

#-----------------------------------------------------------------------------#

# RecFIN Recreational - 2005-2023 Landings mtons
ca_bio_rec = read.csv(here("data-raw","RecFIN-SD001-CALIFORNIA-quillback-1983---2023_2.Oct.2024.csv"), header = T)

# If pull the confidential data (SD501) have five additional fields, though the last is blank
# [1] "RECFIN_DATE"      "COUNTY_NUMBER"    "INTERVIEW_SITE"  
# [4] "INTERVIEW_TIME"   "CPFV_LOCATION_ID"
# Seems like the conf data is not needed
#ca_bio_recfin_conf = read.csv(here("data-raw","RecFIN-SD501-CALIFORNIA-quillback-1983---2023_2.Oct.2024.csv"), header = T)
#colnames(ca_bio_recfin_conf)[!(colnames(ca_bio_recfin_conf) %in% colnames(ca_bio_recfin))]



##############################################################################-
## Explore the data ----
##############################################################################-

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



##############################################################################-
## Process the data ----
##############################################################################-

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
ca_bio_rec$source = "recfin"
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


################-
## Plotting ----
################-

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


