##############################################################################################################
#
# 	Purpose: Compile catch streams for use in stock synthesis
#
#   Created: Sept 26, 2024
#			  by Brian Langseth 
#
#   Uses output from the following scripts, combines them, and then fills in gaps
#     pacfin_processing.R
#     recfin_processing.R
#     discard_exploration.R
#
#
##############################################################################################################

library(here)
library(readxl)
library(magrittr)
library(ggplot2)

#---------------------------------------------------------------------------------------------------------------#

# Load the data ----

#---------------------------------------------------------------------------------------------------------------#

###
## Historical commercial landings ---- 
###

# # These files are copied from the 2021 quillback archives, and then saved in the data-raw folder
# file.copy(from = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/PacFIN Catch/ca_hist_commercial_1916_1968_ej_Feb2021_CAlandingsCaughtORWA.csv",
#           to = here("data-raw","ca_hist_commercial_1916_1968_ej_Feb2021_CAlandingsCaughtORWA.csv"),
#           overwrite = FALSE)
# file.copy(from = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/PacFIN Catch/ca_hist_commercial_1969_1980_ej.csv",
#           to = here("data-raw","ca_hist_commercial_1969_1980_ej.csv"),
#           overwrite = FALSE)

# Historical 1916-1968 Landings (from EJ based on Ralston reconstruction) metric tons. 
# Original 1916-1968 data have other and trawl gear, but trawl component very small relative to other.
# In csv they are aggregated into a single time series. We are unlikely to break out by gear in model. 
# These include landings in CA caught in OR/WA waters, which total 0.27 MT and are no more than 0.079 in any one year 
ca_com_hist1 <- read.csv(here("data-raw", "ca_hist_commercial_1916_1968_ej_Feb2021_CAlandingsCaughtORWA.csv"))

# Historical 1969-1980 Landings lbs
# Original 1969-1980 data have hook and line and trawl gear. All is trawl except in BRG: 5 lbs for 1978, 3 lbs for 1979
# In csv they are aggregated into time series by area. We are unlikely to break out by gear in model. 
ca_com_hist2 <- read.csv(here("data-raw", "ca_hist_commercial_1969_1980_ej.csv"))

#Convert lbs to MT
ca_com_hist2$QLBKmt <- ca_com_hist2$Grand.Total/2204.62 

#Combine
ca_com_hist <- rbind(ca_com_hist1, ca_com_hist2[c("Year","QLBKmt")])
ca_com_hist[is.na(ca_com_hist)] <- 0

## Plot the data

ggplot(ca_com_hist, aes(y = QLBKmt, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Landings (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 1968.5, linetype = "dashed")
# ggsave(here('data_explore_figs',"hist_com_landings.png"),
#        width = 6, height = 4)

#Scaled to match scale of pacfin landings
ggplot(ca_com_hist, aes(y = QLBKmt, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Landings (MT)") +
  ylim(0,50) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 1968.5, linetype = "dashed")
# ggsave(here('data_explore_figs',"hist_com_landings_scaled.png"),
#        width = 6, height = 4)



###
## PacFIN landings ----
###

# Output from pacfin_processing.R
# Data from 1984-2023
ca_pacfin <- read.csv(here("data", "CAquillback_pacfin_landings.csv"))



###
## Historical recreational landings ----
###

# # These files are copied from the 2021 quillback archives, and then saved in the data-raw folder
# file.copy(from = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/RecFIN Catch/ca_hist_recreational_1928_1980_ej.csv",
#           to = here("data-raw","ca_hist_recreational_1928_1980_ej.csv"),
#           overwrite = FALSE)

# Historical 1928-1980 Landings metric tons
# Use the value from this data file for 1980 because Ralston considered it more accurate than MRFSS
ca_rec_hist <- read.csv(here("data-raw","ca_hist_recreational_1928_1980_ej.csv"))
ca_rec_hist$QLBKmt <- ca_rec_hist$QLBKmt_North + ca_rec_hist$QLBKmt_South

# We also have this data broken out by CPFV (charter) and shore/skiff (private) from John Field back in Jan 2021
# Ratio of numbers was applied  to weight data
ca_rec_hist_by_fleet <- data.frame(read_excel(here("data-raw", "2021spp.Rec.NandSConception.xlsx"), 
                                           sheet = "Quillback", skip = 3 ))
ca_rec_hist$QLBKmt_pc <- ca_rec_hist_by_fleet$CPFV.tons
ca_rec_hist$QLBKmt_pr <- ca_rec_hist_by_fleet$skiff.shore.tons


## Plot the data

plot(ca_rec_hist$Year, ca_rec_hist$QLBKmt_pr, type = "l", lwd = 3, 
     xlab = "Year", ylab = "Historical rec landings (mt)")
lines(ca_rec_hist$Year, ca_rec_hist$QLBKmt_pc, lwd = 3, col = "green")
legend("topleft", c("CPFV", "Skiff/Shore"), col = c(3, 1), lty = 1, lwd = 3, bty = "n")

ca_rec_hist_wide <- ca_rec_hist %>% dplyr::select(c(Year, QLBKmt_pc, QLBKmt_pr)) %>%
  tidyr::pivot_longer(cols = c("QLBKmt_pc", "QLBKmt_pr"), 
                      names_to = c("mode"),
                      values_to = "mt")

ggplot(ca_rec_hist_wide, aes(y = mt, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Landings (MT)") +
  #scale_fill_manual(values = c("#00BA38", "#619CFF")) + #If break out by mode
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# ggsave(here('data_explore_figs',"hist_rec_landings.png"),
#        width = 6, height = 4)

#Scaled to match scale of pacfin landings
ggplot(ca_rec_hist_wide, aes(y = mt, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Landings (MT)") +
  #scale_fill_manual(values = c("#00BA38", "#619CFF")) + #If break out by mode
  ylim(0,40) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# ggsave(here('data_explore_figs',"hist_rec_landings_scaled.png"),
#        width = 6, height = 4)



###
## RecFIN/MRFSS landings ----
###

# Output from recfin_processing.R
# Currently I add fishing modes together and only have a single aggregate time series. 

# MRFSS data from 1980-2004 (replace the 1980 value with historical 1980 value)
ca_rec_mrfss <- read.csv(here("data", "CAquillback_mrfss_catches.csv"))

# RecFIN data from 2005-2023
ca_rec_recfin <- read.csv(here("data", "CAquillback_recfin_catches.csv"))

# Add 2020 proxy values from (https://github.com/pfmc-assessments/california-data/tree/main/recreational-fishery/proxy%202020%20data)
# to value from RecFIN for 2020. Only 0.889 mt
proxy2020 <- sum(read_excel(here("data-raw", "CDFWRec_QuillbackRF_AvgProxyValuesApr-Jun2020.xlsx"), sheet = "QuillbackRF")$`Proxy Average Value (mt)`)
ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR == 2020, c("tot_mt", "land_mt")] <- 
  ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR == 2020, c("tot_mt", "land_mt")] + proxy2020

# Add the 2020 and 2021 unallocated rockfish genus catch assigned to quillback. 
# Values pulled from github on Dec 17, 2024 from
# https://github.com/pfmc-assessments/california-data/blob/main/recreational-fishery/proxy%202020%20data/genus_allocate_quillback_20241205.csv
# Add allocated values (2020 for PR and PC, 2021 for PC only) to those that already exist in recfin 
# Because have no discards in 2020 or 2021, add allocated values to both landings and total mortality
update2020_21 <- utils::read.csv(file = here("data-raw", "genus_allocate_quillback_20241205.csv"), header = TRUE)
alloc_val <- update2020_21 %>% dplyr::group_by(year, mode, orig_allocated) %>% 
  dplyr::summarize(sum = sum(quillback_kg) * 0.001) #0.001 to get into MT
ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR %in% c(2020),][,c("tot_mt", "land_mt")] <- 
  ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR %in% c(2020),][,c("tot_mt", "land_mt")] + 
  sum(alloc_val[alloc_val$year == 2020 & alloc_val$orig_allocated == "allocated",]$sum)
ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR %in% c(2021),][,c("tot_mt", "land_mt")] <- 
  ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR %in% c(2021),][,c("tot_mt", "land_mt")] + 
  alloc_val[alloc_val$year == 2021 & alloc_val$orig_allocated == "allocated",]$sum


###
## GEMM discard values ----
###

# Output from discard_exploration.R
# Data from 2002-2023
gemm_sector <- read.csv(here("data", "CAquillback_gemm_mortality_and_discard.csv"))
gemm <- gemm_sector %>%
  dplyr::group_by(grouped_sector, year) %>% 
  dplyr::summarize(Tot_Dead = round(sum(Tot_Dead),3),
                   Discard = round(sum(Discard),3),
                   Dead_Discard = round(sum(Dead_Discard),3),
                   Landings = round(sum(Landings),3)) %>%
  dplyr::mutate(., "dis_mort_prop" = round(Dead_Discard/Tot_Dead,3)) %>% 
  data.frame() 

#How different are the gemm values from PacFIN
plot(ca_pacfin[ca_pacfin$LANDING_YEAR >= 2002, "LANDING_YEAR"],
     ca_pacfin[ca_pacfin$LANDING_YEAR >= 2002, "mtons"], 
     type = "l", lwd = 3, xlab = "Year", ylab = "Metric tons")
lines(gemm[gemm$grouped_sector == "ca_comm", "year"],
      gemm[gemm$grouped_sector == "ca_comm", "Landings"], col = 2, lwd = 3)
plot(ca_pacfin[ca_pacfin$LANDING_YEAR >= 2002, "mtons"] - 
       gemm[gemm$grouped_sector == "ca_comm", "Landings"])

#How different are the gemm values from RecFIN
#Landings - 2020 and 2021 difference because of proxy and unallocated rockfish values
plot(ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR >= 2002, "RECFIN_YEAR"],
     ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR >= 2002, "land_mt"], 
     type = "l", lwd = 3, xlab = "Year", ylab = "Metric tons")
lines(gemm[gemm$grouped_sector == "ca_rec", "year"],
      gemm[gemm$grouped_sector == "ca_rec", "Landings"], col = 2, lwd = 3)
plot(ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR >= 2002, "land_mt"] - 
       gemm[gemm$grouped_sector == "ca_rec", "Landings"])
#Discards
plot(ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR >= 2002, "RECFIN_YEAR"],
     ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR >= 2002, "dis_mt"], 
     type = "l", lwd = 3, xlab = "Year", ylab = "Metric tons")
lines(gemm[gemm$grouped_sector == "ca_rec", "year"],
      gemm[gemm$grouped_sector == "ca_rec", "Dead_Discard"], col = 2, lwd = 3)
plot(ca_rec_recfin[ca_rec_recfin$RECFIN_YEAR >= 2002, "dis_mt"] - 
       gemm[gemm$grouped_sector == "ca_rec", "Dead_Discard"])


#---------------------------------------------------------------------------------------------------------------#

# Combine data into single overall catch data frame and output ----

#---------------------------------------------------------------------------------------------------------------#

###
# Create new data frame
###

ca_catch <- data.frame("Year" = 1916:2024)
ca_catch$com_tot <- NA
ca_catch$com_lan <- NA
ca_catch$com_dis <- NA
ca_catch$rec_tot <- NA
ca_catch$rec_lan <- NA
ca_catch$rec_dis <- NA


###-----------------------###
## Commercial data ----
###-----------------------###


###
# Add historical commercial data
###

ca_catch[ca_catch$Year %in% ca_com_hist$Year, "com_lan"] <- ca_com_hist$QLBKmt


###
# Add pacfin data and fill in gaps
###

ca_catch[ca_catch$Year %in% ca_pacfin$LANDING_YEAR, "com_lan"] <- ca_pacfin$mtons

## Fill in gaps

#Sampling occurred during 1981-1983, and in 1985. Therefore assume lack of samples mean 0 landings
ca_catch[ca_catch$Year %in% c(1981:1983, 1985), "com_lan"] <- 0


###
# Add commercial discards
###

#Add discards to pacfin years and sum for total

ca_catch[ca_catch$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_dis"] <-
  gemm[gemm$grouped_sector == "ca_comm",  "Dead_Discard"]
ca_catch[ca_catch$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_tot"] <-
  ca_catch[ca_catch$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_lan"] +
  ca_catch[ca_catch$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_dis"]
  
#Add to historical years

#First need historical proportion of dead discards. Chose proportional to landings
#Calculate from years 2002-2021
hist_dis_prop <- sum(gemm_sector[gemm_sector$sector == "Nearshore" & 
                                   gemm_sector$year %in% c(2002:2021), "Dead_Discard"]) /
  sum(gemm_sector[gemm_sector$sector == "Nearshore" & 
                    gemm_sector$year %in% c(2002:2021), "Landings"])

#Then calculate dead discards...
ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_dis"] <-
  (ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_lan"]) *
  hist_dis_prop
#...and total mortality
ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_tot"] <-
  ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_dis"] +
  ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_lan"]



###-----------------------###
## Recreational data ----
###-----------------------###


###
# Add historical recreational data
###

ca_catch[ca_catch$Year %in% ca_rec_hist$Year, "rec_lan"] <- ca_rec_hist$QLBKmt


###
# Add MRFSS data and fill in gaps
###

#Do not add 1980 MRFSS value. Use the estimate from the historical reconstruction instead
ca_catch[(ca_catch$Year %in% ca_rec_mrfss$YEAR) & (!ca_catch$Year %in% 1980), c("rec_tot", "rec_lan", "rec_dis")] <- 
  ca_rec_mrfss[!(ca_rec_mrfss$YEAR %in% 1980), c("tot_mt", "landEst_mt", "disEst_mt")]

## Fill in gaps

# Add extra PC estimates for 1993-1995
# Values (either pc_rat or pc_avg) calculated in recfin_processing.R
# Can apply ratio to landings and discards, but addition only to landings otherwise double counted
# pc_rat = 0.7468163
# ca_catch[ca_catch$Year %in% c(1993:1995), c("rec_tot", "rec_dis", "rec_lan")] <- (1 + pc_rat) * 
#   ca_catch[ca_catch$Year %in% c(1993:1995), c("rec_tot", "rec_dis", "rec_lan")]
pc_avg = 2.460985
ca_catch[ca_catch$Year %in% c(1993:1995), c("rec_tot", "rec_lan")] <- pc_avg + 
  ca_catch[ca_catch$Year %in% c(1993:1995), c("rec_tot", "rec_lan")]

#Fill in missing 1990-1992 years
#If based on averages of nearby points
plot(y = ca_catch[ca_catch$Year %in% c(1980:2004),]$rec_tot, x = 1980:2004, type="b", ylab = "CA mrfss catch mt for model")
average_vals <- c(mean(ca_catch$rec_tot[ca_catch$Year %in% c(1987:1989)], na.rm = T), #3 yr average before
                  mean(ca_catch$rec_tot[ca_catch$Year %in% c(1987:1989, 1993:1995)], na.rm = T), #3 yr average before and after
                  mean(ca_catch$rec_tot[ca_catch$Year %in% c(1993:1995)], na.rm = T)) #3 yr average after
lines(y = average_vals, x = 1990:1992, col = 2, lwd = 3)
#If based on average of 1989 and 1993 (as was done for the 2021 assessment)
simp_avg <- mean(ca_catch$rec_tot[ca_catch$Year %in% c(1989, 1993)], na.rm = T)
lines(y = rep(simp_avg, 3) , x = 1990:1992, lwd = 3)
#If Based on linear interpolation between 1989 and 1993
impute_trend <- (ca_catch$rec_tot[ca_catch$Year %in% c(1993)] - ca_catch$rec_tot[ca_catch$Year %in% c(1989)]) / (1993-1989)
impute_vals <- ca_catch$rec_tot[ca_catch$Year %in% c(1989)] + 1:3*impute_trend
points(y = impute_vals, x = 1990:1992, pch=19, col=5)

ca_catch[ca_catch$Year %in% c(1990:1992), "rec_tot"] <- average_vals


###
# Add RecFIN data
###

ca_catch[ca_catch$Year %in% ca_rec_recfin$RECFIN_YEAR, c("rec_tot", "rec_lan", "rec_dis")] <- 
  ca_rec_recfin[, c("tot_mt", "land_mt", "dis_mt")]


###
# Add historical recreational discards
###

#Calculate average from mrfss. Weight by catch - 1.1%
ca_rec_hist_discard_rate <- sum(ca_rec_mrfss$disEst_mt, na.rm = TRUE) / 
  sum(ca_rec_mrfss$landEst_mt, na.rm = TRUE)

#Then calculate discards mortality...
ca_catch[ca_catch$Year %in% ca_rec_hist$Year, "rec_dis"] <- ca_rec_hist_discard_rate *
  ca_catch[ca_catch$Year %in% ca_rec_hist$Year, "rec_lan"]
#...and sum for total mortality
ca_catch[ca_catch$Year %in% ca_rec_hist$Year, "rec_tot"] <- 
  ca_catch[ca_catch$Year %in% ca_rec_hist$Year, "rec_dis"] +
  ca_catch[ca_catch$Year %in% ca_rec_hist$Year, "rec_lan"]


#---------------------------------------------------------------------------------------------------------------#

# Output final total removals file ----

#---------------------------------------------------------------------------------------------------------------#

#Full version with discards and landings
#write.csv(round(ca_catch,3), file = file.path(here("data", "CAquillback_total_removals.csv")), row.names = FALSE)

# Commercial estimates:
# 1916-1968 Ralston Reconstruction landings plus historical dead discard amount (0.25% of landings)
# 1969-1980 CalCOM Reconstruction landings plus historical dead discard amount (0.25% of landings)
# 1981-2001 PacFIN landings plus historical dead discard amount (0.25% of landings)
# 2002-2023 PacFIN landings plus dead discards from GEMM for non-"California Recreational" sectors
# 
# Historical dead discard percentage (0.25%) is from the GEMM and is the proportion of dead discards (summed over years) to landings (summed over years) in the nearshore sector for 2002-2021. 
# 
# Recreational estimates:
# 1928-1980 Ralston Reconstruction landings plus historical dead discard amount (1.1% of landings). Ralston value used for 1980 in place of MRFSS estimate. 
# 1981-2004 MRFSS total mortality (WGT_AB1). Note that the 2004 CRFS catch estimate is located within the MRFSS dataset.
# For 1993-1995 when PC sampling was not taking place, added estimate for PC based on overall average of PC estimates 1980-2004
# For 1990-1992 when all sampling was not taking place, applied average of nearby years (averaged 1987-1989 for 1990, 1987-1989 and 1993-1995 for 1991, and 1993-1995 for 1992)
# 2005-2023 CFRS total mortality
# 
# Historical dead discard percentage (1.1%) is from MRFSS and is the proportion of type B1 (ESTHARV; summed over years) to type A (ESTCLAIM + ESTHARV; summed over years) mortality for 1980-2004. 


# Plot overall values
ca_catch_long <- ca_catch %>% tidyr::pivot_longer(cols = -Year,
                                                  names_to = c("fleet", "type"), 
                                                  names_sep = "_",
                                                  values_to = "mt")

ggplot(ca_catch_long %>% dplyr::filter(type == "tot"), aes(y = mt, x = Year, fill = fleet)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Mortality (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = c(0.1, 0.7))
ggsave(here('data_explore_figs',"ALL_landings.png"),
       width = 6, height = 4)


#Plot of rec (mrfss and recfin) landings and discards. 
#This plot does not include imputted values for 1990-1992
#but does include additional 1993-1995 PC landings
ggplot(ca_catch_long %>% dplyr::filter(type %in% c("lan", "dis"), 
                                       fleet == "rec",
                                       Year %in% c(1980:2023)), aes(y = mt, x = Year, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Mortality (MT)") +
  scale_fill_manual(labels = c("Dead discards", "Landings"), values = c("#F8766D","#00BFC4")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = c(0.7, 0.7))
ggsave(here('data_explore_figs',"rec_comb_mortality_with_discards.png"),
       width = 6, height = 4)

