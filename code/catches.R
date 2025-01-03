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

#---------------------------------------------------------------------------------------------------------------#

# Load the data ----

#---------------------------------------------------------------------------------------------------------------#

###
# Historical commercial landings ---- 
###

# These files are copied from the 2021 quillback archives, and then saved in the data-raw folder
file.copy(from = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/PacFIN Catch/ca_hist_commercial_1916_1968_ej_Feb2021_CAlandingsCaughtORWA.csv",
          to = here("data-raw","ca_hist_commercial_1916_1968_ej_Feb2021_CAlandingsCaughtORWA.csv"),
          overwrite = FALSE)
file.copy(from = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/PacFIN Catch/ca_hist_commercial_1969_1980_ej.csv",
          to = here("data-raw","ca_hist_commercial_1969_1980_ej.csv"),
          overwrite = FALSE)

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


## Plot the data

ggplot(ca_com_hist, aes(y = QLBKmt, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Landings (MT)") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 1968.5, linetype = "dashed")
ggsave(here('data_explore_figs',"hist_com_landings.png"),
       width = 6, height = 4)

#Scaled to match scale of pacfin landings
ggplot(ca_com_hist, aes(y = QLBKmt, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Landings (MT)") +
  ylim(0,50) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 1968.5, linetype = "dashed")
ggsave(here('data_explore_figs',"hist_com_landings_scaled.png"),
       width = 6, height = 4)



###
# PacFIN landings ----
###

# Output from pacfin_processing.R
# Data from 1984-2023
ca_pacfin <- read.csv(here("data", "CAquillback_pacfin_landings.csv"))



###
# Historical recreational landings ----
###

# These files are copied from the 2021 quillback archives, and then saved in the data-raw folder
file.copy(from = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/RecFIN Catch/ca_hist_recreational_1928_1980_ej.csv",
          to = here("data-raw","ca_hist_recreational_1928_1980_ej.csv"),
          overwrite = FALSE)

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

plot(ca_rec_hist$Year, ca_rec_hist$QLBKmt_pr, type = "l", lwd = 3, 
     xlab = "Year", ylab = "Historical rec landings (mt)")
lines(ca_rec_hist$Year, ca_rec_hist$QLBKmt_pc, lwd = 3, col = "green")
legend("topleft", c("CPFV", "Skiff/Shore"), col = c(3, 1), lty = 1, lwd = 3, bty = "n")



###
# RecFIN/MRFSS landings ----
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
# GEMM discard values ----
###

# Output from discard_exploration.R
# Data from 2002-2023
gemm <- read.csv(here("data", "CAquillback_gemm_mortality_and_discard.csv"))

#How different are the gemm values from PacFIN
plot(ca_pacfin[ca_pacfin$LANDING_YEAR >= 2002, "LANDING_YEAR"],
     ca_pacfin[ca_pacfin$LANDING_YEAR >= 2002, "mtons"], 
     type = "l", lwd = 3, xlab = "Year", ylab = "Metric tons")
lines(gemm[gemm$grouped_sector == "ca_comm", "year"],
      gemm[gemm$grouped_sector == "ca_comm", "Landings"], col = 2, lwd = 3)
plot(ca_pacfin[ca_pacfin$LANDING_YEAR >= 2002, "mtons"] - 
       gemm[gemm$grouped_sector == "ca_comm", "Landings"])

#How different are the gemm values from RecFIN
#Landings
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

##
#Create new data frame
##

ca_catch <- data.frame("Year" = 1916:2024)
ca_catch$com_tot <- NA
ca_catch$com_lan <- NA
ca_catch$com_dis <- NA
ca_catch$rec_tot <- NA
ca_catch$rec_lan <- NA
ca_catch$rec_dis <- NA


##
#Add historical commercial data
##

ca_com_hist <- rbind(ca_com_hist1, ca_com_hist2[c("Year","QLBKmt")])
ca_catch[ca_catch$Year %in% ca_com_hist$Year, "com_lan"] <- ca_com_hist$QLBKmt


##
#Add pacfin data
##

ca_catch[ca_catch$Year %in% ca_pacfin$LANDING_YEAR, "com_lan"] <- ca_pacfin$mtons

## Fill in gaps

##
#Add commercial discards
##

#Add discards to pacfin years and sum for total

ca_catch[ca_catch$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_dis"] <-
  gemm[gemm$grouped_sector == "ca_comm",  "Dead_Discard"]
ca_catch[ca_catch$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_tot"] <-
  ca_catch[ca_catch$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_lan"] +
  ca_catch[ca_catch$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_dis"]
  
#Add to historical years

#First need historical proportion of total mortality that is discard 
#Calculate from years 2002-2018
hist_dis_prop <- sum(gemm[gemm$grouped_sector == "ca_comm" & gemm$year %in% c(2002:2018), "Dead_Discard"]) /
  sum(gemm[gemm$grouped_sector == "ca_comm" & gemm$year %in% c(2002:2018), "Tot_Dead"])
#Then calculate total...
ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_tot"] <-
  (ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_lan"]) /
  (1-hist_dis_prop)
#...and substract from total the landings to get discards
ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_dis"] <-
  ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_tot"] -
  ca_catch[!ca_catch$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), "com_lan"]
        

##
#Add historical recreational data
##

ca_catch[ca_catch$Year %in% ca_rec_hist$Year, "rec_lan"] <- ca_rec_hist$QLBKmt


##
#Add MRFSS data
##

#Do not add 1980 MRFSS value. Use the estimate from the historical reconstruction instead
ca_catch[(ca_catch$Year %in% ca_rec_mrfss$YEAR) & (!ca_catch$Year %in% 1980), "rec_tot"] <- 
  ca_rec_mrfss[!(ca_rec_mrfss$YEAR %in% 1980),]$tot_mt


##
#Add RecFIN data
##

ca_catch[ca_catch$Year %in% ca_rec_recfin$RECFIN_YEAR, c("rec_tot", "rec_lan", "rec_dis")] <- 
  ca_rec_recfin[, c("tot_mt", "land_mt", "dis_mt")]


##
#Output final total removals file
##

#write.csv(round(ca_catch,3), file = file.path(here("data", "CAquillback_total_removals.csv")), row.names = FALSE)





