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


