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
#
#
##############################################################################################################

library(here)
library(readxl)

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
