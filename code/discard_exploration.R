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


########################-
# Load the GEMM - the GEMM includes information for commercial discards ----
########################-

# The gemm splits data north and south of 40 10
# Pulled from nwfscSurvey on January 2, 2024
species = c("Quillback Rockfish", 
            "Quillback Rockfish (California)", 
            "Quillback Rockfish (Washington/Oregon)")
gemm <- nwfscSurvey::pull_gemm(common_name = species)
#Cant specify the dir because with three names the file cant save.
#To run, run the function without a dir, and then manually save as a csv
#write.csv(gemm, here("data-raw", "gemm_Quillback_rockfish_Jan_2_2024.csv"), row.names = FALSE)

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
#TO DO: Figure out which are to do used
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
  mutate(., "dis_mort_rate" = round(sum_dis_mort/sum_tot_mort,3)) %>% 
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
  mutate(., "dis_mort_rate" = round(sum_dis_mort/sum_tot_mort,3)) %>% 
  data.frame() 


########################-
## Process the data ---
########################-

#Use only Quillback Rockfish (California) given that Quillback rockfish may not be california
gemm_ca <- gemm %>% dplyr::filter(species == "Quillback Rockfish (California)")

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
  mutate(., "dis_mort_rate" = round(Dead_Discard/Tot_Dead,3)) %>% 
  data.frame()

#Now do this by grouped sector and year
all_yr <- gemm_ca %>%
  dplyr::group_by(grouped_sector, year) %>% 
  dplyr::summarize(Tot_Dead = round(sum(total_discard_with_mort_rates_applied_and_landings_mt),3),
                   Discard = round(sum(total_discard_mt),3),
                   Dead_Discard = round(sum(total_discard_with_mort_rates_applied_mt),3),
                   Landings = round(sum(total_landings_mt),3)) %>%
  mutate(., "dis_mort_rate" = round(Dead_Discard/Tot_Dead,3)) %>% 
  data.frame() 

#write.csv(all_yr, file = here("data", "CAquillback_gemm_mortality_and_discard.csv"), row.names = FALSE)


##
#Plot the values
##

ggplot(all_yr, aes(y = dis_mort_rate, x = year, colour = grouped_sector)) +
  geom_line() +
  xlab("Year") +
  ylab("Discard Mortality Rate") +
  ggtitle("GEMM discard mortality rate") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"gemm_discard_mortality_grouped_sector.png"),
       width = 6, height = 4)

#Compare to regular sector and year
sec_yr <- gemm_ca %>%
  dplyr::group_by(sector, year) %>% 
  dplyr::summarize(Tot_Dead = round(sum(total_discard_with_mort_rates_applied_and_landings_mt),3),
                   Discard = round(sum(total_discard_mt),3),
                   Dead_Discard = round(sum(total_discard_with_mort_rates_applied_mt),3),
                   Landings = round(sum(total_landings_mt),3)) %>%
  mutate(., "dis_mort_rate" = round(Dead_Discard/Tot_Dead,3)) %>% 
  data.frame() 
#Only plot for the sectors that have more that 1% percentage of total mortality
ggplot(sec_yr %>% dplyr::filter(sector %in% c("California Recreational",
                                              "Directed P Halibut",
                                              "Nearshore",
                                              "OA Fixed Gear - Hook & Line")), 
       aes(y = dis_mort_rate, x = year, colour = sector)) +
  geom_line() +
  xlab("Year") +
  ylab("Discard Mortality Rate") +
  ggtitle("GEMM discard mortality rate") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(here('data_explore_figs',"gemm_discard_mortality_sector.png"),
       width = 6, height = 4)



#-----------------------------------------------------------------------------------
# WCGOP data below is not necessary
# For 2021 assessment, needed these to divide out GEMM values by state
# This cycle, Quillback Rockfish (California) parses these out already
#-----------------------------------------------------------------------------------

# load(here("data-raw", "WCGOP", "CONFIDENTIAL_Observer_Catch_Data_2002_2022.Rdat"))
# load(here("data-raw", "WCGOP", "CONFIDENTIAL_EMLogbook_Catch_Data_2022.Rdat"))
# 
# obquil <- OBCatch %>% dplyr::filter(species == "Quillback Rockfish")
# emquil <- EMCatch %>% dplyr::filter(species == "Quillback Rockfish")
# 
# table(obquil$R_STATE, obquil$D_STATE) #Depart and return to CA
# table(emquil$R_STATE, emquil$D_STATE) #Depart and return to CA
# 
# obquil <- OBCatch %>% dplyr::filter(species == "Quillback Rockfish",
#                                     D_STATE == "CA")
# emquil <- EMCatch %>% dplyr::filter(species == "Quillback Rockfish",
#                                     D_STATE == "CA")

