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
#devtools::load_all("U:/Stock assessments/PacFIN.Utilities")
#devtools::install_github("pfmc-assessments/PacFIN.Utilities")
library(PacFIN.Utilities)
library(gridExtra)

#-----------------------------------------------------------------------------#

# Load catch data ----

#-----------------------------------------------------------------------------#
# PacFIN Commercial - 1984-2023 Landings mtons
load(here("data-raw", "PacFIN.QLBK.CompFT.11.Dec.2024.RData"))
catch = catch.pacfin %>% dplyr::filter(AGENCY_CODE == "C")

#Pull 2021 assessment values for comparison. These are pacFIN landings; no discards yet
dir = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/output catch"
catch2021 = read.csv(file.path(dir,"pacfin_catch_by_area_Feb2021.csv"), header = T) %>%
  dplyr::select(c("year","ca"))


##############################################################################-
## Explore the data ----
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
table(catch$PACFIN_GEAR_CODE, catch$PACFIN_GROUP_GEAR_CODE)
table(catch$CATCH_AREA_DESCRIPTION) 
#There are 3 records caught >42 degree N. This amount is small (~7.5 lbs).
#We assign where landed and these would count toward CA ACL so keep in.
table(catch$AREA_TYPE_NAME)
table(catch$PACFIN_CATCH_AREA_CODE)
table(catch$PACFIN_GROUP_CATCH_AREA_CODE)
table(catch$COUNTY_CODE)
table(catch$PORT_NAME)
table(catch$PACFIN_GROUP_PORT_CODE) #This is what the expansions are based on but
#county is nearly a one to one match so county could be fine too. Go with port group
table(catch$COUNTY_CODE, catch$PACFIN_GROUP_PORT_CODE)
table(catch$IOPAC_PORT_GROUP)
table(catch$IOPAC_PORT_GROUP, catch$PACFIN_GROUP_PORT_CODE, useNA = "always")
#suggest using pacfin group port code because IOPAC has more NAs
table(catch$SPECIES_CODE_NAME) #same as MARKET_CATEGORY_NAME
#Nominal quillback is a decent amount, all since 1994
table(catch$LANDING_YEAR, catch$SPECIES_CODE_NAME)
table(catch$LANDING_YEAR, catch$IS_SPECIES_COMP_USED)
table(catch$CDFW_AREA_BLOCK)

#Some plots to see basic patterns among fields
plot(catch$LANDED_WEIGHT_LBS/catch$NUM_OF_FISH)
#Number of fish is rarely entered. Default looks like 0
plot(catch$PACFIN_YEAR-catch$LANDING_YEAR) #These are the same
plot(table(catch$LANDING_MONTH)) #summer is most common time period


##############################################################################-
## Process the data ----
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

#If we break down by gears there are confidentiality issues for some of the lesser gears
#Dealer is the most restrictive
#Ultimately, gear groupings isn't really helpful as nearly all is HKL, which
#is an aggregate of refined gear codes LGL and VHL/POL
aggGear <- catch %>% 
  dplyr::group_by(PACFIN_GROUP_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS)) %>%
  data.frame()
aggGearN <- catch %>% 
  dplyr::group_by(PACFIN_GROUP_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(N = length(unique(VESSEL_NAME))) %>% 
  data.frame()
aggGearID <- catch %>% 
  dplyr::group_by(PACFIN_GROUP_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(N = length(unique(VESSEL_ID))) %>% 
  data.frame()
aggGearDealer <- catch %>% 
  dplyr::group_by(PACFIN_GROUP_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(N = length(unique(DEALER_ID))) %>%  
  data.frame()

#If break down by Gear Code, not Gear Group Code. 
#Gear group code  is what expansions are based on, but doing this by gear
#to get a sense of approximate possible breakdown. 
aggGear2 <- catch %>% 
  dplyr::group_by(PACFIN_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(sum = sum(LANDED_WEIGHT_MTONS)) %>%
  data.frame()
aggGearN2 <- catch %>% 
  dplyr::group_by(PACFIN_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(N = length(unique(VESSEL_NAME))) %>% 
  data.frame()
aggGearID2 <- catch %>% 
  dplyr::group_by(PACFIN_GEAR_CODE, LANDING_YEAR) %>% 
  dplyr::summarize(N = length(unique(VESSEL_ID))) %>% 
  data.frame()
aggGearDealer2 <- catch %>% 
  dplyr::group_by(PACFIN_GEAR_CODE, LANDING_YEAR) %>% 
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
aggPortDisp <- catch %>%
  dplyr::group_by(PACFIN_GROUP_PORT_CODE, disp, LANDING_YEAR) %>%
  dplyr::summarize(sum = round(sum(LANDED_WEIGHT_MTONS),4)) %>%
  data.frame()  

#If we break out by port code there are confidentiality issues and dealer is 
#the most restrictive except for port/year where vessel name is
aggPortN <- catch %>%
  dplyr::group_by(PACFIN_GROUP_PORT_CODE) %>%
  dplyr::summarize(N = length(unique(DEALER_ID))) %>%
  data.frame() 
aggPortYearN <- catch %>%
  dplyr::group_by(PACFIN_GROUP_PORT_CODE, LANDING_YEAR) %>%
  dplyr::summarize(N = length(unique(VESSEL_NAME))) %>%
  data.frame() 
aggPortDispN <- catch %>%
  dplyr::group_by(PACFIN_GROUP_PORT_CODE, disp, LANDING_YEAR) %>%
  dplyr::summarize(N = length(unique(DEALER_ID))) %>%
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
  dplyr::summarize(mtons = sum(LANDED_WEIGHT_MTONS)) %>%
  data.frame() %>% 
  merge(., data.frame("LANDING_YEAR" = c(1984:2024)), by = "LANDING_YEAR", all = TRUE)
#write.csv(aggCatch[aggCatch$LANDING_YEAR %in% c(1984:2023),], here("data","CAquillback_pacfin_landings.csv"), row.names = FALSE)


################-
## Plotting ----
################-

ggplot(aggCatch, aes(y = mtons, x = LANDING_YEAR)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Landings (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"pacfin_landings.png"),
       width = 6, height = 4)

#Compare current landings with landings from pacfin for the 2021 assessment
#All very similar with greatest difference being slightly less catch in 2017 this 
#time around
plot(x = aggCatch$LANDING_YEAR, y = aggCatch$mtons, type = "l", lwd = 2, col = "black")
lines(x = catch2021$year, y = catch2021$ca, lty = 1, col = "green", lwd= 2)
plot((merge(aggCatch, catch2021, by.x = "LANDING_YEAR", by.y = "year") %>% 
        dplyr::mutate("diff" = round(mtons - ca, 3)))$diff, x = c(1984:2020))


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


#Plot by gear (really gear group)
#Filter out records (by gear) that have fewer than 3 dealers which is most restrictive
#Not informative
dontShow = unique(c(which(aggGearDealer$N<3)))

ggplot(aggGear[-dontShow,], aes(fill = PACFIN_GROUP_GEAR_CODE, y = sum, x = LANDING_YEAR)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Landings (MT)") + 
  ggtitle("PacFIN landings of quillback by gear - Filtered for confidentiality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"pacfin_landings_gear.png"),
       width = 6, height = 4)

#Now by more refined gear (but not what expansion is based on) category
dontShow = unique(c(which(aggGearDealer2$N<3)))
ggplot(aggGear2[-dontShow,], aes(fill = PACFIN_GEAR_CODE, y = sum, x = LANDING_YEAR)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("Year") +
  ylab("Landings (MT)") + 
  ggtitle("PacFIN landings of quillback by gear - Filtered for confidentiality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"pacfin_landings_gear_FOR_EXPLORE_ONLY.png"),
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

ggplot(aggPortYear[-dontShow,], aes(fill = PACFIN_GROUP_PORT_CODE, x = LANDING_YEAR, y = sum)) + 
  geom_bar(position = "stack", stat = "identity") + 
  xlab("Year") +
  ylab("Landings (MT)") + 
  ggtitle("PacFIN landings of quillback by port group and year - Filtered for confidentiality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(fill = "Port")
ggsave(here('data_explore_figs',"pacfin_landings_port_group_year.png"),
       width = 6, height = 4)

#and now by port by year and disposition
dontShow = c(which(aggPortDispN$N<3))

ggplot(aggPortDisp[-dontShow,], aes(fill = PACFIN_GROUP_PORT_CODE, x = LANDING_YEAR, y = sum)) + 
  geom_bar(position = "stack", stat = "identity") + 
  xlab("Year") +
  ylab("Landings (MT)") +
  facet_wrap(~ disp) +
  ggtitle("PacFIN landings of quillback by port group, disposition, and year \nFiltered for confidentiality") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(fill = "Port")
ggsave(here('data_explore_figs',"pacfin_landings_port_group_disp_year.png"),
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

# PacFIN Commercial - 1978-2022
load(here("data-raw", "PacFIN.QLBK.bds.11.Dec.2024.RData"))
bio = bds.pacfin %>% dplyr::filter(AGENCY_CODE == "C")

##############################################################################-
## Explore the data ----
##############################################################################-

table(bio$SAMPLE_TYPE_DESC)
table(bio$SAMPLE_METHOD_CODE) #all random
table(bio$DATA_TYPE)
table(bio$AGENCY_CONDITION_CODE)
table(bio$PACFIN_CONDITION_CODE) #some alive
#Most of the live fish fishery is out of crescent city
table(bio$PACFIN_GROUP_PORT_CODE, bio$PACFIN_CONDITION_CODE, useNA = "always")
table(bio$SAMPLE_YEAR) #why is there 1978 entries when the catch data does not go that far back?
#This is not uncommon, petrale is a good example. Bio samples go back farther but
#catch data only to 1981
table(bio$AGENCY_CODE)
table(bio$PACFIN_PORT_CODE, bio$PACFIN_GROUP_PORT_CODE)
table(bio$AGENCY_GEAR_CODE, bio$PACFIN_GEAR_CODE)
table(bio$AGENCY_GEAR_CODE, bio$PACFIN_GROUP_PORT_CODE) #SFA is all LGL
table(bio$VESSEL_ID)
table(bio$VESSEL_NAME)
table(bio$FISHING_AREA_BLOCK_NUMBER)
table(bio$PSMFC_CATCH_AREA_CODE)
table(bio$DEPTH_AVERAGE_FATHOMS)
table(bio$MARKET_CATEGORY)
table(bio$PACFIN_SPECIES_CODE)
table(bio$OBSERVED_FREQUENCY)
table(bio$FISH_LENGTH_TYPE_CODE, useNA = "always") #fork length, but a number are NAs
#No lengths are taken for these fish
table(bio$FISH_LENGTH_TYPE_DESC, useNA = "always")
table(bio$FISH_LENGTH_TYPE_CODE, is.na(bio$FISH_LENGTH), useNA = "always")
table(bio$SAMPLE_YEAR, is.na(bio$FISH_LENGTH)) #Why are we missing lengths. These
#samples were for species composition samples only and so weren't measured. 
table(bio$FISH_LENGTH)
table(bio$FORK_LENGTH)
table(bio$SEX_CODE) #mostly unsexed but quite a few with sexes
table(bio$SAMPLE_YEAR, bio$SEX_CODE) #sexed really only since 2019
table(bio$AGENCY_FISH_MATURITY_CODE) #some data here, likely not detailed enough
table(bio$FINAL_FISH_AGE_CODE)
table(bio$AGE_COUNT) #thirteen fish with double reads
table(bio$FINAL_FISH_AGE_IN_YEARS)
table(bio$AGENCY_GRADE_CODE, useNA = "always")
table(bio$PACFIN_GRADE_NAME, useNA = "always")
table(bio$PACFIN_GRADE_NAME, bio$PACFIN_CONDITION_CODE) #grade code doesn't
#doesn't seem to be dependent on disposition. Alive fish are both large and small
table(bio$FISH_LENGTH_UNITS, useNA = "always")
table(bio$FISH_LENGTH_IS_ESTIMATED)


#There are a number of samples in 2018 and 2019 without lengths. Why?
nolen <- bio[is.na(bio$FISH_LENGTH),]
table(nolen$SAMPLE_YEAR)
table(nolen$PACFIN_GROUP_PORT_CODE) #nearly all are from eureka
table(nolen$PACFIN_CONDITION_CODE) #all are alive
table(nolen$AGENCY_GEAR_CODE) #nearly all from LGL 
table(nolen$SAMPLE_ID) #coming from 5 different trips
#See above. These were species-composition samples only so weren't measure. 
#Will want to remove these.



##############################################################################-
## Process the data ----
##############################################################################-

# #Run clean PacFIN to see how it processes the bio data. No bio data removed
# bio_clean <- cleanPacFIN(Pdata = bio, CLEAN=TRUE, verbose=TRUE)
# # N SAMPLE_TYPEs changed from M to S for special samples from OR: 0
# # N not in keep_sample_type (SAMPLE_TYPE): 0
# # N with SAMPLE_TYPE of NA: 0
# # N not in keep_sample_method (SAMPLE_METHOD): 0
# # N with SAMPLE_NO of NA: 0
# # N without length: 146
# # N without Age: 3096
# # N without length and Age: 3096
# # N sample weights not available for OR: 0
# # N records: 3123
# # N remaining if CLEAN: 3123
# # N removed if CLEAN: 0

#Simplify disposition to alive vs. dead
#Only species marked alive have a condition code. Assume all others are dead.
bio$disp <- "dead"
bio[which(bio$PACFIN_CONDITION_CODE == "A"), "disp"] <- "alive"

#Reogranized port group codes from North to South
bio$group_port_NS <-  dplyr::case_when(bio$PACFIN_GROUP_PORT_CODE == "BDA" ~ "4BDA",
                                          bio$PACFIN_GROUP_PORT_CODE == "BGA" ~ "3BGA",
                                          bio$PACFIN_GROUP_PORT_CODE == "CCA" ~ "1CCA",
                                          bio$PACFIN_GROUP_PORT_CODE == "ERA" ~ "2ERA",
                                          bio$PACFIN_GROUP_PORT_CODE == "MNA" ~ "6MNA",
                                          bio$PACFIN_GROUP_PORT_CODE == "MRA" ~ "7MRA",
                                          bio$PACFIN_GROUP_PORT_CODE == "SFA" ~ "5SFA")

#Remove the fish without lengths
bio <- bio[which(!is.na(bio$FISH_LENGTH)),]

#Output basic bio data for later use for analysis
bio$source <- "pacfin"
bio$length_cm <- bio$FISH_LENGTH/10
out <- bio %>% dplyr::select("Year" = SAMPLE_YEAR,
                             length_cm,
                             "weight_kg" = FISH_WEIGHT, #No weights in PacFIN
                             "age" = FINAL_FISH_AGE_IN_YEARS,
                             "sex" = SEX_CODE,
                             "area" = PACFIN_GROUP_PORT_CODE,
                             disp,
                             source)
#write.csv(out, here("data","CAquillback_com_bio.csv"), row.names = FALSE)




################-
## Plotting ----
################-

#Lengths over time show a lot of trends
ggplot(bio, aes(y = FISH_LENGTH, x = SAMPLE_YEAR)) +
  geom_point(colour = "#00BFC4")
ggsave(here('data_explore_figs',"pacfin_length.png"), 
       width = 6, height = 4)

##
#By disposition
##

#Group dot and density plot together
par(mfrow=c(2,1))
p1 <- ggplot(bio, aes(y = FISH_LENGTH, x = SAMPLE_YEAR, color = disp)) +
  geom_point()
p2 <- ggplot(bio, aes(x = FISH_LENGTH)) +
  geom_density(aes(colour = disp))
#Doesn't seem to have differences between live and dead fish fishery

grid.arrange(p1, p2, nrow = 2)
g <- gridExtra::arrangeGrob(p1, p2, nrow=2) #generates g
ggsave(here('data_explore_figs',"pacfin_length_disposition.png"), g, 
       width = 6, height = 4)


##
#By areas
##

#by port group
ggplot(bio, aes(y = FISH_LENGTH, x = SAMPLE_YEAR, color = PACFIN_GROUP_PORT_CODE)) +
  geom_point() +
  labs(color = "Port group")

#Two big signals, increasing size in during the 2000s (occurs only in CCA), 
#and then diverse sizes in last few years because of sampling in ERA. 
#Maybe also smaller fish in SFA and maybe BDA (is this the live fish fishery?)
#Why only sample in eureka in recent years? Is this representative of rest of areas. 
#Why only sample in CCA in aughts? Is this representative of rest of areas.
ggplot(bio, aes(color = PACFIN_GROUP_PORT_CODE, y = FISH_LENGTH, x = SAMPLE_YEAR)) +
  geom_point() + 
  facet_wrap(~PACFIN_GROUP_PORT_CODE) +
  labs(color = "Port group")
ggsave(here('data_explore_figs',"pacfin_length_port_group.png"), 
       width = 6, height = 4)

#Smaller sizes in BDA and SFA does not appear to an effect of the live fish fishery
ggplot(bio, aes(y = FISH_LENGTH, x = SAMPLE_YEAR)) +
  geom_point(aes(fill = factor(disp), colour = factor(disp)), shape = 21) + 
  facet_wrap(~PACFIN_GROUP_PORT_CODE)
ggsave(here('data_explore_figs',"pacfin_length_port_group_disposition.png"), 
       width = 6, height = 4)

#Density plots also show limited difference in live/dead lengths
#BGA difference mostly due to different timing for live/dead
ggplot(bio, aes(x = FISH_LENGTH)) +
  geom_density(aes(colour = disp)) + 
  facet_wrap(~PACFIN_GROUP_PORT_CODE)
ggsave(here('data_explore_figs',"pacfin_length_port_group_disposition_density.png"), 
       width = 6, height = 4)


#Test whether areas have different lengths irrespective of years and organize
#by ports north to south to see if there is a north south gradient.
ggplot(bio, aes(fill = group_port_NS, y = FISH_LENGTH, x = group_port_NS)) +
  geom_violin()
ggsave(here('data_explore_figs',"pacfin_length_port_group_violin.png"), 
       width = 6, height = 4)
#Bodega and San Fran fish are smaller but crescent city, eureka, and bragg are
#all similar.
ggplot(bio, aes(color = group_port_NS, x = FISH_LENGTH)) +
  geom_density()
#Seems like the more likely explanation is depth. This is probably not the best
#dataset to look at differences in size by latitude.
ggsave(here('data_explore_figs',"pacfin_length_port_group_density.png"), 
       width = 6, height = 4)

#Compare to southern Oregon fish (mostly Orford)
bioOR = bds.pacfin %>% 
  dplyr::filter(AGENCY_CODE == "O") %>%
  dplyr::filter(PACFIN_GROUP_PORT_CODE == "BRA")
table(bioOR$PACFIN_PORT_NAME, bioOR$PACFIN_GROUP_PORT_CODE)
ggplot(bio, aes(color = group_port_NS, x = FISH_LENGTH)) +
  geom_density() +
  geom_density(aes(color = PACFIN_PORT_NAME, x = FISH_LENGTH), data = bioOR, linetype = 2)
ggsave(here('data_explore_figs',"pacfin_length_port_group_density_withOregon.png"), 
       width = 6, height = 4)
  
    
##
#by gear
## 

#Catches are nearly all HKL group, but here the gear codes are not grouped 
#so look at finer scale codes
ggplot(bio, aes(y = FISH_LENGTH, x = PACFIN_GEAR_CODE)) +
  geom_violin(aes(fill = PACFIN_GEAR_CODE))
#Looking just at the main HKL and LGL codes does not show strong differences
#Looking at differences over years, or ports doesn't seem worthwhile
ggplot(bio %>% dplyr::filter(PACFIN_GEAR_CODE %in% c("HKL", "LGL")),
       aes(x = FISH_LENGTH)) +
  geom_density(aes(colour = PACFIN_GEAR_CODE))

#By gear and port
ggplot(bio, aes(y = FISH_LENGTH, x = PACFIN_GEAR_CODE)) +
  geom_violin(aes(fill = PACFIN_GEAR_CODE)) +
  facet_wrap(~PACFIN_GROUP_PORT_CODE)
ggplot(bio %>% dplyr::filter(PACFIN_GEAR_CODE %in% c("HKL", "LGL")),
       aes(x = FISH_LENGTH)) +
  geom_density(aes(colour = PACFIN_GEAR_CODE)) +
  facet_wrap(~PACFIN_GROUP_PORT_CODE)
#Seems like any "gear" difference is really a difference in area since
#SFA and BDA which are smaller are from only one gear. Among CCA, where
#both gears exist, the sizes are similar.








