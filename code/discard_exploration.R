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
#       Used in catches.R file
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
# Pulled from nwfscSurvey on December 31, 2024
species = c("Quillback Rockfish", 
            "Quillback Rockfish (California)", 
            "Quillback Rockfish (Washington/Oregon)")
gemm <- nwfscSurvey::pull_gemm(common_name = species)
#Cant specify the dir because with three names the file cant save.
#To run, run the function without a dir, and then manually save as a csv
#write.csv(gemm, here("data-raw", "gemm_Quillback_rockfish_Dec_31_2024.csv"))


########################-
## Explore the data ---
########################-

# Remove the research removals -- 
# Research removals are generally not included with commercial landings (although this does not need to be the case)
# however, removing them here allows you to correctly calculate the discard rate based on commercial data only
#gemm = gemm[!gemm$Sector %in% "Research", ] 

#Total discards are all discards, not dead discards
plot(gemm$total_discard_mt - gemm$total_discard_with_mort_rates_applied_mt)
plot(gemm$total_discard_and_landings_mt - 
       gemm$total_discard_with_mort_rates_applied_and_landings_mt)
#Use "total_discard_with_mort_rates_applied_and_landings_mt" as total

#But these dont seem to add up
#TO DO: Figure out which are to do bused
plot(gemm$total_discard_with_mort_rates_applied_and_landings_mt - gemm$total_discard_with_mort_rates_applied_mt- gemm$total_landings_mt)


#"Quillback" occurs on midwater hake and research. These are small amounts...
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
#...compared to those with only Quillback Rockfish (California)
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

#Use only Quillback Rockfish (California)
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





## CONTINUE HERE





#-----------------------------------------------------------------------------------
# Load the WCGOP discard totals 
# Kayleigh suggested:
# multiply by proportion of catch observed in an area compared to coastwide -- 
# not the proportion discarded in a given area. To do that, I suggest calculating the 
# proportion of observed catch (D+R) within each area (compared to coastwide) in each year. 
# So, for example, for 2018 it might be 5% of observed in WA, 10% observed in OR, 
# 40% observed N of Pt Conc, and 45% observed S of Pont Conc (adding up to 100% coastwide). 
# We could then multiply the total discard from the GEMM by each of those area proportions to
# estimate the discard in each area. We could also explore using the same method but with 
# only observed discard rather than observed discard AND retained together.
#-----------------------------------------------------------------------------------

ncs = read.csv(file.path(dir, "quillback_OB_DisRatios_noboot_ncs_state_2020-10-16.csv"))
cs = read.csv(file.path(dir,  "quillback_OB_DisRatios_noboot_cs_state_2020-10-16.csv"))

ret_ncs = aggregate(Observed_RETAINED.MTS ~ ryear + State, data = ncs, drop = FALSE,FUN = sum)
dis_ncs = aggregate(Observed_DISCARD.MTS  ~ ryear + State, data = ncs, drop = FALSE,FUN = sum)
ret_cs  = aggregate(Observed_RETAINED.MTS ~ ryear + State, data = cs, drop = FALSE, FUN = sum)
dis_cs  = aggregate(Observed_DISCARD.MTS  ~ ryear + State, data = cs, drop = FALSE,FUN = sum)
ret_ncs[is.na(ret_ncs)] = dis_ncs[is.na(dis_ncs)] = ret_cs[is.na(ret_cs)] = dis_cs[is.na(dis_cs)] = 0

tot_obs = data.frame(Year = sort(unique(ret_ncs$ryear)),
					 ca = ret_ncs[which(ret_ncs$State == "CA"),3] + dis_ncs[which(dis_ncs$State == "CA"),3],
					 or = ret_ncs[which(ret_ncs$State == "OR"),3] + dis_ncs[which(dis_ncs$State == "OR"),3],
					 wa = ret_ncs[which(ret_ncs$State == "WA"),3] + dis_ncs[which(dis_ncs$State == "WA"),3])

tot_obs[which(tot_obs$Year %in% ret_cs$ryear), "ca"] = tot_obs[which(tot_obs$Year %in% ret_cs$ryear), "ca"] +  
					ret_cs[which(ret_cs$State == "CA"), 3] + dis_cs[which(dis_cs$State == "CA"), 3]
tot_obs[which(tot_obs$Year %in% ret_cs$ryear), "or"] = tot_obs[which(tot_obs$Year %in% ret_cs$ryear), "or"] +  
          ret_cs[which(ret_cs$State == "OR"), 3] + dis_cs[which(dis_cs$State == "OR"), 3]
tot_obs[which(tot_obs$Year %in% ret_cs$ryear), "wa"] = tot_obs[which(tot_obs$Year %in% ret_cs$ryear), "wa"] +  
					ret_cs[which(ret_cs$State == "WA"), 3] + dis_cs[which(dis_cs$State == "WA"), 3]	

#Ratio of dead discards by state
ratio = cbind(tot_obs$Year, tot_obs[,-1] / apply(tot_obs[,-1], 1, sum))

#Dead discard values (mt) by state
dead = data.frame(Year = tot_obs$Year,
				  ca = ratio$ca * all[which(all$Area == "commercial"), "Dead_Discard"],
				  or = ratio$or * all[which(all$Area == "commercial"), "Dead_Discard"],
				  wa = ratio$wa * all[which(all$Area == "commercial"), "Dead_Discard"] ) 

#write.csv(dead, file = file.path(dir, "quillback_commercial_discard.csv"), row.names = FALSE)



#-----------------------------------------------------------------------------------
# Load the WCGOP discard totals by gear
# Calculate ratio based on sum of fixed and trawl
#-----------------------------------------------------------------------------------

ncs = read.csv(file.path(dir, "quillback_OB_DisRatios_noboot_ncs_Gears_State_2020-11-17.csv"))
cs = read.csv(file.path(dir,  "quillback_OB_DisRatios_noboot_cs_Gears_State_2020-11-17.csv"))

ret_ncs = aggregate(Observed_RETAINED.MTS ~ ryear + State + gear2, data = ncs, drop = FALSE,FUN = sum)
dis_ncs = aggregate(Observed_DISCARD.MTS  ~ ryear + State + gear2, data = ncs, drop = FALSE,FUN = sum)
ret_cs  = aggregate(Observed_RETAINED.MTS ~ ryear + State + gear2, data = cs, drop = FALSE, FUN = sum)
dis_cs  = aggregate(Observed_DISCARD.MTS  ~ ryear + State + gear2, data = cs, drop = FALSE,FUN = sum)
ret_ncs[is.na(ret_ncs)] = dis_ncs[is.na(dis_ncs)] = ret_cs[is.na(ret_cs)] = dis_cs[is.na(dis_cs)] = 0

tot_obs_fixed = data.frame(Year = sort(unique(ret_ncs$ryear)),
                     ca = ret_ncs[which(ret_ncs$State == "CA" & ret_ncs$gear2 == "FixedGears"),4] + dis_ncs[which(dis_ncs$State == "CA" & dis_ncs$gear2 == "FixedGears"),4],
                     or = ret_ncs[which(ret_ncs$State == "OR" & ret_ncs$gear2 == "FixedGears"),4] + dis_ncs[which(dis_ncs$State == "OR" & dis_ncs$gear2 == "FixedGears"),4],
                     wa = ret_ncs[which(ret_ncs$State == "WA" & ret_ncs$gear2 == "FixedGears"),4] + dis_ncs[which(dis_ncs$State == "WA" & dis_ncs$gear2 == "FixedGears"),4])

tot_obs_fixed[which(tot_obs_fixed$Year %in% ret_cs$ryear), "ca"] = tot_obs_fixed[which(tot_obs_fixed$Year %in% ret_cs$ryear), "ca"] +  
  ret_cs[which(ret_cs$State == "CA" & ret_cs$gear2 == "FixedGears"), 4] + dis_cs[which(dis_cs$State == "CA" & dis_cs$gear2 == "FixedGears"), 4]
tot_obs_fixed[which(tot_obs_fixed$Year %in% ret_cs$ryear), "or"] = tot_obs_fixed[which(tot_obs_fixed$Year %in% ret_cs$ryear), "or"] +  
  ret_cs[which(ret_cs$State == "OR" & ret_cs$gear2 == "FixedGears"), 4] + dis_cs[which(dis_cs$State == "OR" & dis_cs$gear2 == "FixedGears"), 4]
tot_obs_fixed[which(tot_obs_fixed$Year %in% ret_cs$ryear), "wa"] = tot_obs_fixed[which(tot_obs_fixed$Year %in% ret_cs$ryear), "wa"] +  
  ret_cs[which(ret_cs$State == "WA" & ret_cs$gear2 == "FixedGears"), 4] + dis_cs[which(dis_cs$State == "WA" & dis_cs$gear2 == "FixedGears"), 4]	

#Ratio of dead discards by state
ratio_fixed = cbind(tot_obs_fixed$Year, tot_obs_fixed[,-1] / apply(tot_obs[,-1], 1, sum)) #need to use total sum to properly apportion discard from just fixed gears

#Dead discard values (mt) by state
dead_fixed = data.frame(Year = tot_obs_fixed$Year,
                  ca = ratio_fixed$ca * all[which(all$Area == "commercial"), "Dead_Discard"],
                  or = ratio_fixed$or * all[which(all$Area == "commercial"), "Dead_Discard"],
                  wa = ratio_fixed$wa * all[which(all$Area == "commercial"), "Dead_Discard"] ) 
