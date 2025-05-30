##############################################################################################################
#
# 	Purpose: Evaluate quillback rockfish commercial discarding
#
#			  by Brian Langseth on December 31, 2024
#       
#       Copied from quillback_discard_exploration.R in the 2021 github repo
#       https://github.com/pfmc-assessments/quillback_rockfish_2021/blob/master/code/quillback_discard_exploration.R
#       and modified for 2025 assessment
#
#
##############################################################################################################

library(dplyr)
library(here)
library(magrittr)
#pak::pkg_install("pfmc-assessments/nwfscSurvey")
library(nwfscSurvey)
library(ggplot2)


########################-
# Load the GEMM - the GEMM includes information for commercial discards ----
########################-

# # The gemm splits data north and south of 40 10
# # Pulled from nwfscSurvey on January 2, 2024
# species = c("Quillback Rockfish", 
#             "Quillback Rockfish (California)", 
#             "Quillback Rockfish (Washington/Oregon)")
# gemm <- nwfscSurvey::pull_gemm(common_name = species)
# #Cant specify the dir because with three names the file cant save.
# #To run, run the function without a dir, and then manually save as a csv
# #write.csv(gemm, here("data-raw", "gemm_Quillback_rockfish_Jan_2_2024.csv"), row.names = FALSE)

#Only need to run once, so can read directly from the saved file
gemm <- read.csv(here("data-raw", "gemm_Quillback_rockfish_Jan_2_2024.csv"), header = T)

########################-
## Explore the data ---
########################-

#Total discards are all discards, not dead discards
plot(gemm$total_discard_mt - gemm$total_discard_with_mort_rates_applied_mt)
plot(gemm$total_discard_and_landings_mt - 
       gemm$total_discard_with_mort_rates_applied_and_landings_mt)
#Use "total_discard_with_mort_rates_applied_and_landings_mt" as total

#But these dont seem to add up
#UPDATE: I talked with Kayleigh about these and she said there are some rounding issues, and 
#also some specific adjustments Oregon does for some of their species. Research also is only
#presented as total mortality. 
#What I am using is appropriate
plot(gemm$total_discard_with_mort_rates_applied_and_landings_mt - gemm$total_discard_with_mort_rates_applied_mt - gemm$total_landings_mt)


#"Quillback" occurs on midwater hake and research. These are small amounts, and Im not sure
#if the research values are in California or not.
table(gemm$sector, gemm$species)
table(gemm$year, gemm$species)
gemm %>% 
  dplyr::group_by(sector) %>% 
  dplyr::filter(species == "Quillback Rockfish") %>%
  dplyr::summarize(sum_tot_mort = round(sum(total_discard_with_mort_rates_applied_and_landings_mt),3),
                   sum_dis = round(sum(total_discard_mt),3),
                   sum_dis_mort = round(sum(total_discard_with_mort_rates_applied_mt),3),
                   sum_lan = round(sum(total_landings_mt),3)) %>%
  dplyr::mutate(., "dis_mort_prop" = round(sum_dis_mort/sum_tot_mort,3)) %>% 
  data.frame()
#...compare to those with only Quillback Rockfish (California)
#Note that discard mort rates are much higher for OA fixed gear than the Nearshore sector
gemm %>% 
  dplyr::group_by(sector) %>% 
  dplyr::filter(species == "Quillback Rockfish (California)") %>%
  dplyr::summarize(sum_tot_mort = round(sum(total_discard_with_mort_rates_applied_and_landings_mt),3),
                   sum_dis = round(sum(total_discard_mt),3),
                   sum_dis_mort = round(sum(total_discard_with_mort_rates_applied_mt),3),
                   sum_lan = round(sum(total_landings_mt),3)) %>%
  dplyr::mutate(., "dis_mort_prop" = round(sum_dis_mort/sum_tot_mort,3)) %>% 
  data.frame() 


########################-
## Process the data ---
########################-

#Use only Quillback Rockfish (California) given that Quillback rockfish may not be california
#Also exclude research catches since these are not commercial

gemm_ca <- gemm %>% dplyr::filter(species == "Quillback Rockfish (California)") %>%
  dplyr::filter(sector != "Research")

#Add grouped sector combining various commercial sectors into one. 
#Because this is only CA rockfish, the commercial component is the California component

gemm_ca$grouped_sector = NA
gemm_ca$grouped_sector[gemm_ca$sector == "California Recreational"] = "ca_rec"
gemm_ca$grouped_sector[is.na(gemm_ca$grouped_sector)] = "ca_comm"
table(gemm_ca$sector, gemm_ca$grouped_sector)

#Produce aggregated numbers by grouped sector and calculated discard mortality rate
gemm_ca %>%
  dplyr::group_by(grouped_sector) %>% 
  dplyr::summarize(Tot_Dead = round(sum(total_discard_with_mort_rates_applied_and_landings_mt),3),
                   Discard = round(sum(total_discard_mt),3),
                   Dead_Discard = round(sum(total_discard_with_mort_rates_applied_mt),3),
                   Landings = round(sum(total_landings_mt),3)) %>%
  dplyr::mutate(., "dis_mort_prop" = round(Dead_Discard/Tot_Dead,3)) %>% 
  data.frame()

#Now do this by grouped sector and year
all_yr <- gemm_ca %>%
  dplyr::group_by(grouped_sector, year) %>% 
  dplyr::summarize(Tot_Dead = round(sum(total_discard_with_mort_rates_applied_and_landings_mt),3),
                   Discard = round(sum(total_discard_mt),3),
                   Dead_Discard = round(sum(total_discard_with_mort_rates_applied_mt),3),
                   Landings = round(sum(total_landings_mt),3)) %>%
  dplyr::mutate(., "dis_mort_prop" = round(Dead_Discard/Tot_Dead,3)) %>% 
  data.frame() 

#And by sector and year (but keep grouped_sector in)
sec_yr <- gemm_ca %>%
  dplyr::group_by(sector, year, grouped_sector) %>% 
  dplyr::summarize(Tot_Dead = round(sum(total_discard_with_mort_rates_applied_and_landings_mt),3),
                   Discard = round(sum(total_discard_mt),3),
                   Dead_Discard = round(sum(total_discard_with_mort_rates_applied_mt),3),
                   Landings = round(sum(total_landings_mt),3)) %>%
  dplyr::mutate(., "dis_mort_prop" = round(Dead_Discard/Tot_Dead,3)) %>% 
  data.frame() 

#write.csv(sec_yr, file = here("data", "CAquillback_gemm_mortality_and_discard.csv"), row.names = FALSE)


# #And by sector and year (but keep grouped_sector in) for FAA
# #This is by N/S of 40'10 which we ultimately aren't going to use since it 
# #doesn't capture Bragg in the north
# gemm_ca$faa <- dplyr::case_when(grepl("North", gemm_ca$grouping) ~ "North",
#                                 grepl("South", gemm_ca$grouping) ~ "South")
# sec_yr_faa <- gemm_ca %>%
#   dplyr::group_by(sector, year, grouped_sector, faa) %>% 
#   dplyr::summarize(Tot_Dead = round(sum(total_discard_with_mort_rates_applied_and_landings_mt),3),
#                    Discard = round(sum(total_discard_mt),3),
#                    Dead_Discard = round(sum(total_discard_with_mort_rates_applied_mt),3),
#                    Landings = round(sum(total_landings_mt),3)) %>%
#   dplyr::mutate(., "dis_mort_prop" = round(Dead_Discard/Tot_Dead,3)) %>% 
#   data.frame() 
# #write.csv(sec_yr_faa, file = here("data", "CAquillback_gemm_mortality_and_discard_FAA.csv"), row.names = FALSE)


##
#Plot the values - These are discard mort proportions (dead discard / total mort)
##

#Amount of mortality due to dead discards by grouped sector
ggplot(all_yr, aes(y = dis_mort_prop, x = year, colour = grouped_sector)) +
  geom_line() +
  xlab("Year") +
  ylab("Dead Discard Proportion") +
  ggtitle("GEMM proportion of mortality due to discard") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"gemm_discard_mortality_grouped_sector.png"),
       width = 6, height = 4)

#Only plot for the sectors that have more that 1% percentage of total mortality
ggplot(sec_yr %>% dplyr::filter(sector %in% c("California Recreational",
                                              "Directed P Halibut",
                                              "Nearshore",
                                              "OA Fixed Gear - Hook & Line")), 
       aes(y = dis_mort_prop, x = year, colour = sector)) +
  geom_line() +
  xlab("Year") +
  ylab("Dead Discard Proportion") +
  ggtitle("GEMM proportion of mortality due to discard") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"gemm_discard_mortality_sector.png"),
       width = 6, height = 4)

#Seems like changes started in 2022 (though OA fixed gear has no estimate in 2021)
sec_yr %>% dplyr::filter(sector %in% c("Nearshore", "OA Fixed Gear - Hook & Line"),
                         year %in% c(2018:2023)) %>%
  dplyr::select(sector, year, Tot_Dead, Dead_Discard, dis_mort_prop)


#Amount of total mortality by sector and year
ggplot(sec_yr %>% dplyr::filter(sector %in% c("California Recreational",
                                              "Directed P Halibut",
                                              "Nearshore",
                                              "OA Fixed Gear - Hook & Line")), 
       aes(y = Tot_Dead, x = year, colour = sector)) +
  geom_line() +
  xlab("Year") +
  ylab("Metric tons") +
  ggtitle("GEMM total mortality") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"gemm_discard_total_mortality_sector.png"),
       width = 6, height = 4)


#Breakdown of mortality (landings vs dead discards for the commercial fleet) 
ggplot(sec_yr %>% 
         tidyr::pivot_longer(cols = c("Tot_Dead", "Discard", "Dead_Discard", "Landings"), values_to = "mt") %>%
         dplyr::filter(name %in% c("Landings", "Dead_Discard"),
                       grouped_sector %in% c("ca_comm")),
       aes(y = mt, x = year, fill = name)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Mortality (MT)") +
  ggtitle("GEMM total commercial mortality") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"gemm_comm_mortality.png"),
       width = 6, height = 4)


#-----------------------------------------------------------------------------------
# WCGOP data below is not necessary
# For 2021 assessment, needed these to divide out GEMM values by state
# This cycle, Quillback Rockfish (California) parses these out already
#-----------------------------------------------------------------------------------

load(here("data-raw", "WCGOP", "CONFIDENTIAL_Observer_Catch_Data_2002_2022.Rdat"))
load(here("data-raw", "WCGOP", "CONFIDENTIAL_EMLogbook_Catch_Data_2022.Rdat"))

obquil <- OBCatch %>% dplyr::filter(species == "Quillback Rockfish")
emquil <- EMCatch %>% dplyr::filter(species == "Quillback Rockfish")

table(obquil$R_STATE, obquil$D_STATE) #Depart and return to CA
table(emquil$R_STATE, emquil$D_STATE) #Depart and return to CA

obquil <- OBCatch %>% dplyr::filter(species == "Quillback Rockfish",
                                    D_STATE == "CA")
emquil <- EMCatch %>% dplyr::filter(species == "Quillback Rockfish",
                                    D_STATE == "CA")


#Nothing about length in EMLogbook
table(emquil$SET_DEPTH)
table(emquil$AVG_DEPTH)
table(emquil$OBSERVED)
table(emquil$CATCH_DISPOSITION)
table(emquil$NetType)
table(emquil$SRC)
table(emquil$sector)
table(emquil$gear)
table(emquil$PACFIN_PORT_CODE)

#Nothing about length in observer catch
table(obquil$GEAR_TYPE)
table(obquil$GEAR)
table(obquil$gear)
table(obquil$PCID)
table(obquil$SET_DEPTH)
table(obquil$AVG_DEPTH)
table(obquil$CATCH_DISPOSITION)
table(obquil$FISHERY)
table(obquil$cs_sector)


## Now try with biological data

load(here("data-raw", "WCGOP", "CONFIDENTIAL_Observer_Biological_Data_2002_2022.Rdat"))

bioquil <- OBBio2 %>% dplyr::filter(species == "Quillback Rockfish")

table(bioquil$R_STATE.x, bioquil$D_STATE) #Depart and return to CA

bioquil <- OBBio2 %>% dplyr::filter(species == "Quillback Rockfish",
                                    D_STATE == "CA")

table(bioquil$GEAR_TYPE)
table(bioquil$TARGET)
table(bioquil$D_PORT_GROUP)
table(bioquil$R_PORT_GROUP)
table(bioquil$SET_YEAR)
table(bioquil$SET_DEPTH)
table(bioquil$SET_DEPTH_UM)
table(bioquil$AVG_DEPTH)
table(bioquil$FISHERY)
table(bioquil$CATCH_DISPOSITION) #D means discard
table(bioquil$DISCARD_REASON)
table(bioquil$SET_YEAR, bioquil$DISCARD_REASON) #most discards are in 2022
table(bioquil$SET_YEAR, bioquil$D_PORT_GROUP) #Recent years are CC and EA
table(bioquil$DISCARD_REASON, bioquil$D_PORT_GROUP) #Pretty even across ports
table(bioquil$LENGTH)
table(bioquil$LENGTH_UM)
table(bioquil$SEX)
table(bioquil$SPECIMEN_WEIGHT) #only 6 fish with weight
table(bioquil$AGE)
table(bioquil$sector)
table(bioquil$gear)

##
# Plots
##

#Plots of lengths

#Regulation is longer, but 'Market' sample size is 15 fish so not sure if relevant
#Regulation is nearly all 2022 fish
ggplot(bioquil %>% dplyr::filter(DISCARD_REASON %in% c("Market", "Regulation")), aes(x = LENGTH)) +
       geom_density(aes(color = DISCARD_REASON)) +
  ggtitle("WCGOP discard lengths - Reason equals market or regulation, All Years") 
#If remove 2022 - then lengths are very different. Market and regulation are more simlar
ggplot(bioquil %>% dplyr::filter(DISCARD_REASON %in% c("Market", "Regulation"),
                                 SET_YEAR != 2022), aes(x = LENGTH)) +
  geom_density(aes(color = DISCARD_REASON)) +
  ggtitle("WCGOP discard lengths - Reason equals market or regulation, Not 2022") 

#Plots for depth

#No real pattern of discarding by depth nor size by depth
ggplot(bioquil %>% dplyr::filter(DISCARD_REASON %in% c("Market", "Regulation")), 
       aes(x = AVG_DEPTH, y = LENGTH)) +
  geom_point(aes(color = DISCARD_REASON)) +
  ggtitle("Discard length at set depth (fathoms)") 

## Main conclusions:

# There are 15 samples with discard designation of 'market'. These average around 30 cm. 
# There are 260 samples with discard designation of 'regulation', and 247 of these are in 2022. These average around 40 cm.
# Altogether, there are so few samples outside of 2022-regulation group.
# The main take home is that recent discards do not appear to be of small fish. 


wc <- ggplot(bioquil %>% dplyr::filter(DISCARD_REASON %in% c("Market", "Regulation")), aes(x = LENGTH)) + 
  geom_density(aes(color = DISCARD_REASON)) +
  ggtitle("WCGOP discard lengths - Reason equals market or regulation, All Years")
wc
wc + geom_density(data = out %>% dplyr::filter(Year == 2022), aes(x = length_cm, color = "black")) +
  scale_colour_manual(labels = c("2022 PacFIN", "Market", "Regulation"), 
                     values = c("#000000", "#F8766D", "#00BFC4"))
wc + geom_density(data = out %>% dplyr::filter(Year < 2022), aes(x = length_cm, color = "black")) +
  scale_colour_manual(labels = c("PacFIN < 2022", "Market", "Regulation"), 
                      values = c("#000000", "#F8766D", "#00BFC4"))



# ####
# #For report here is the summary for oregon and washignton quillback
# ####
# gemm_n <- gemm %>% dplyr::filter(species == "Quillback Rockfish (Washington/Oregon)") %>%
#   dplyr::filter(sector != "Research")
# 
# gemm_n$grouped_sector = NA
# gemm_n$grouped_sector[gemm_n$sector == "Washington Recreational"] = "wa_rec"
# gemm_n$grouped_sector[gemm_n$sector == "Oregon Recreational"] = "or_rec"
# gemm_n$grouped_sector[is.na(gemm_n$grouped_sector)] = "n_comm"
# table(gemm_n$sector, gemm_n$grouped_sector)
# 
# temp <- gemm_n %>% 
#           dplyr::group_by(year) %>% 
#           dplyr::summarize(sum_tot_mort = round(sum(total_discard_with_mort_rates_applied_and_landings_mt),3),
#                            sum_dis = round(sum(total_discard_mt),3),
#                            sum_dis_mort = round(sum(total_discard_with_mort_rates_applied_mt),3),
#                            sum_lan = round(sum(total_landings_mt),3)) %>%
#           dplyr::mutate(., "dis_mort_prop" = round(sum_dis_mort/sum_tot_mort,3)) %>% 
#           data.frame()
# 
# mean(tail(temp$sum_tot_mort,5)) #last five years 2019-2023
# mean(tail(temp$sum_tot_mort,10)) #last ten years 2014-2023


