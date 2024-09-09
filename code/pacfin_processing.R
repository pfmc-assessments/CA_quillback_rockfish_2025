##############################################################################################################
#
# 	Purpose: Explore PacFIN quillback rockfish landings/discards 
#            before putting into form for use in SS. Also to explore
#            biological data
#
#   Created: Sept 4, 2024
#			  by Brian Langseth 
#
##############################################################################################################

library(here)
library(ggplot2)
library(magrittr)
#devtools::install_github("pfmc-assessments/PacFIN.Utilities")
library(PacFIN.Utilities)

#-----------------------------------------------------------------------------#

# Load catch data ----

#-----------------------------------------------------------------------------#
# PacFIN Commercial - 1984-2023 Landings mtons
load(here("data-raw", "PacFIN.QLBK.CompFT.26.Jul.2024.RData"))
catch = catch.pacfin %>% dplyr::filter(AGENCY_CODE == "C")

#Pull 2021 assessment values for comparison. These are pacFIN landings; no discards yet
dir = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/output catch"
catch2021 = read.csv(file.path(dir,"pacfin_catch_by_area_Feb2021.csv"), header = T) %>%
  dplyr::select(c("year","ca"))


##############################################################################-
#Explore the data
##############################################################################-

#Looking across fields to see if anything is odd
table(catch$REMOVAL_TYPE_NAME)
table(catch$DISPOSITION_NAME)
#There are 2 discard records of just over 3 lbs. If these are in the fish
#ticket record then they were sold, so keep them in.
table(catch$LANDING_YEAR,catch$DISPOSITION_NAME)
table(catch$CONDITION_NAME)
table(catch$FLEET_CODE)
table(catch$GEAR_NAME)
table(catch$PACFIN_GEAR_CODE)
table(catch$PACFIN_GROUP_GEAR_CODE) #This is what the expansions are based on
table(catch$CATCH_AREA_DESCRIPTION) 
#There are 3 records caught >42 degree N. This amount is small (~7.5 lbs).
#We assign where landed and these would count toward CA ACL so keep in.
table(catch$AREA_TYPE_NAME)
table(catch$PACFIN_CATCH_AREA_CODE)
table(catch$PORT_NAME)
table(catch$COUNTY_CODE)
table(catch$PACFIN_GROUP_PORT_CODE) #This is what the expansions are based on
table(catch$PACFIN_GROUP_CATCH_AREA_CODE)
table(catch$SPECIES_CODE_NAME)
#Nominal quillback is a decent amount, all since 1994
table(catch$LANDING_YEAR, catch$SPECIES_CODE_NAME)
table(catch$CDFW_AREA_BLOCK)

#Some plots to see basic patterns among fields
plot(catch$LANDED_WEIGHT_LBS/catch$NUM_OF_FISH)
#Number of fish is rarely entered. Default looks like 0
plot(catch$PACFIN_YEAR-catch$LANDING_YEAR) #These are the same
plot(table(catch$LANDING_MONTH)) #summer is most common time period


##############################################################################-
#Process the data
##############################################################################-

#Simplify disposition to alive vs. dead, and group gear codes together

catch$disp <- "dead"
catch[catch$DISPOSITION_CODE == "F", "disp"] <- "alive"

#Break out by disposition
aggDisp <- catch %>%
  dplyr::group_by(disp, LANDING_YEAR) %>%
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS)) %>%
  data.frame()
#Breaking out by dealer, name, and ID (Dealer is most restrictive)
#shows 1994 and 1995 alive has < 3
aggDispN <- catch %>%
  dplyr::group_by(disp, LANDING_YEAR) %>%
  dplyr::summarize(N = length(unique(DEALER_ID))) %>%
  data.frame()


#Simplify into gear groupings

table(catch$PACFIN_GEAR_CODE, catch$PACFIN_GROUP_GEAR_CODE)

#If we break down by gears there are confidentiality issues for some of the lesser gears
#Dealer is the most restrictive
aggGear <- catch %>% 
  dplyr::group_by(PACFIN_GROUP_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS)) %>%
  data.frame()
aggN <- catch %>% 
  dplyr::group_by(PACFIN_GROUP_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(N = length(unique(VESSEL_NAME))) %>% 
  data.frame()
aggID <- catch %>% 
  dplyr::group_by(PACFIN_GROUP_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(N = length(unique(VESSEL_ID))) %>% 
  data.frame()
aggDealer <- catch %>% 
  dplyr::group_by(PACFIN_GROUP_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(N = length(unique(DEALER_ID))) %>%  
  data.frame()


#Aggregate into port groups

aggPort <- catch %>%
  dplyr::group_by(PACFIN_GROUP_PORT_CODE) %>%
  dplyr::summarize(sum = round(sum(LANDED_WEIGHT_MTONS),4)) %>%
  data.frame()
aggPortYear <- catch %>%
  dplyr::group_by(PACFIN_GROUP_PORT_CODE, LANDING_YEAR) %>%
  dplyr::summarize(sum = round(sum(LANDED_WEIGHT_MTONS),4)) %>%
  data.frame() 

#If we break out by county there are confidentiality issues and dealer is 
#the most restrictive for port, but for port/year vessel name is
aggPortN <- catch %>%
  dplyr::group_by(PACFIN_GROUP_PORT_CODE) %>%
  dplyr::summarize(N = length(unique(DEALER_ID))) %>%
  data.frame() 
aggPortYearN <- catch %>%
  dplyr::group_by(PACFIN_GROUP_PORT_CODE, LANDING_YEAR) %>%
  dplyr::summarize(N = length(unique(VESSEL_NAME))) %>%
  data.frame() 


#Explore by sector
#Ultimately not very helpful. Nearly all is nearshore and only reported since 2002
#however discards may be different. 

aggSectorYear <- catch %>% 
  dplyr::group_by(FOS_GROUNDFISH_SECTOR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(sum = round(sum(LANDED_WEIGHT_MTONS),4)) %>%
  data.frame()
aggSectorYearN <- catch %>% 
  dplyr::group_by(FOS_GROUNDFISH_SECTOR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(N = length(unique(DEALER_ID))) %>%
  data.frame()


#Aggregate catch by year and add in NA for any years without data
#Use this as the most likely time series to start model with 

aggCatch <- catch %>%
  dplyr::group_by(LANDING_YEAR) %>%
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS)) %>%
  data.frame() %>% 
  merge(., data.frame("LANDING_YEAR" = c(1984:2023)), by = "LANDING_YEAR", all = TRUE)



#############-
#Plotting
#############-

#Compare current landings with landings from pacfin for the 2021 assessment
#All very similar with greatest difference being slightly less catch in 2017 this 
#time around
plot(x = aggCatch$LANDING_YEAR, y = aggCatch$sum, type = "l", lwd = 2, col = "black")
lines(x = catch2021$year, y = catch2021$ca, lty = 1, col = "green", lwd= 2)
plot((merge(aggCatch, catch2021, by.x = "LANDING_YEAR", by.y = "year") %>% 
        dplyr::mutate("diff" = round(sum - ca, 3)))$diff, x = c(1984:2020))


#Plot by disposition
#Filter out records (by disposition) that have fewer than 3 dealers (I looked at
#vessel and vessel name and dealer was most restrictive)
dontShow = c(which(aggDispN$N<3))

ggplot(aggDisp[-dontShow,], aes(fill = disp, y = sum, x = LANDING_YEAR)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Landings (MT)") + 
  ggtitle("PacFIN landings of quillback by disposition - Filtered for confidentiality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"pacfin_landings_disp.png"),
       width = 6, height = 4)


#Plot by gear
#Filter out records (by gear) that have fewer than 3 dealers which is most restrictive
#Not informative
dontShow = unique(c(which(aggDealer$N<3)))

ggplot(aggGear[-dontShow,], aes(fill = PACFIN_GROUP_GEAR_CODE, y = sum, x = LANDING_YEAR)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Landings (MT)") + 
  ggtitle("PacFIN landings of quillback by gear - Filtered for confidentiality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"pacfin_landings_gear.png"),
       width = 6, height = 4)


#Plot by port group
#Filter out records (by port group) that have fewer than 3 vessel names (which is most restrictive)
dontShow = c(which(aggPortYearN$N<3))

ggplot(aggPort[-dontShow,], aes(x = PACFIN_GROUP_PORT_CODE, y = sum)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("County") +
  ylab("Landings (MT)") + 
  ggtitle("PacFIN landings of quillback by port group - Filtered for confidentiality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"pacfin_landings_port_group.png"),
       width = 6, height = 4)

#and now port by year
dontShow = c(which(aggPortYearN$N<3))

ggplot(aggPortYear[-dontShow,], aes(group = PACFIN_GROUP_PORT_CODE, x = LANDING_YEAR, y = sum)) + 
  geom_line(aes(colour = PACFIN_GROUP_PORT_CODE)) + 
  xlab("Year") +
  ylab("Landings (MT)") + 
  ggtitle("PacFIN landings of quillback by port group and year - Filtered for confidentiality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"pacfin_landings_port_group_year.png"),
       width = 6, height = 4)


#Plot by sector - Not really informative when based only on landings
#Filter out records (by sector) that have fewer than 3 dealers (which is most restrictive)
dontShow = unique(c(which(aggSectorYearN$N<3)))

ggplot(aggSectorYear[-dontShow,], aes(fill = FOS_GROUNDFISH_SECTOR_CODE, y = sum, x = LANDING_YEAR)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Landings (MT)") + 
  ggtitle("PacFIN landings of quillback by sector - Filtered for confidentiality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"pacfin_landings_sector.png"),
       width = 6, height = 4)



#-----------------------------------------------------------------------------#

# Load bio sampling data ----

#-----------------------------------------------------------------------------#
