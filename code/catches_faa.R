##############################################################################################################
#
# 	Purpose: Compile catch streams for use in stock synthesis for FAA sensitivity
#
#   Created: Mar 18, 2025
#			  by Brian Langseth 
#
#   Follows script in catches.R but breaks out by FAA
#   This is intentionally pared down from catches.R
#   For details on processes, look at catches.R
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

#All are in district 2 (so North)
ca_com_hist1 <- read.csv(here("data-raw", "ca_hist_commercial_1916_1968_ej_Feb2021_CAlandingsCaughtORWA.csv"))
ca_com_hist1$North <- ca_com_hist1$QLBKmt
ca_com_hist1$South <- 0

#Separate by Crescent City, Eureka, and Bragg in North, and other port groups in South
ca_com_hist2 <- read.csv(here("data-raw", "ca_hist_commercial_1969_1980_ej.csv"))
ca_com_hist2$North <- rowSums(ca_com_hist2[,c("CRS", "ERK", "BRG")], na.rm = TRUE)/2204.62
ca_com_hist2$South <- rowSums(ca_com_hist2[,c("BDG", "OSF", "MNT", "MRO", 
                                              "OSB", "OLA", "OSD", "OCA")], na.rm = TRUE)/2204.62
#Combine
ca_com_hist_faa <- rbind(ca_com_hist1[,c("Year", "North", "South")], ca_com_hist2[c("Year", "North", "South")])



###
## PacFIN landings ----
###

# Output from pacfin_processing.R

# For FAA (its confidential so keep in confidential)
ca_pacfin_faa <- read.csv(here("data", "confidential_noShare", "CAquillback_pacfin_FAA_landings.csv"))
ca_pacfin_faa$ratio <- ca_pacfin_faa$North / rowSums(ca_pacfin_faa[,c("North", "South")], na.rm = TRUE)

#Set NAs to zeros
ca_pacfin_faa[is.na(ca_pacfin_faa)] <- 0
#Set 2024 landings to zero (because so small and confidential)
ca_pacfin_faa[ca_pacfin_faa$LANDING_YEAR == 2024, "North"] <- NA



###
## Albin data ----
###

#Read in Albin data to approximate MRFSS and historical period breakdown
#This data combines Sonoma and Mendocino (so adds Bodega into the north) 
#so not perfect, but better than arbitrary assumptions
albin_faa <- readxl::read_excel(here("data-raw", "Albin_qlbk_only.xlsx"), na = "*",
                                col_types = c(rep("numeric", 2), "text", rep("numeric", 18)))
albin <- data.frame(albin_faa[-1,]) #remove the first row which is a header row
#Calculate ratio of landings in Del Norte/Humboldt and Mendocino/Sonoma
#Estimates are columns 4, 7, 10, 13, and 16. Total estimates is 19
albin_ratio <- sum(albin[,c("Del.Norte.Humboldt...4", "Mendocino.Sonoma...7")], na.rm = TRUE) / 
  sum(albin$Total...19)


###
## Historical recreational landings ----
###

# Historical 1928-1980 Landings metric tons - these are N/S of Pt. Conception
# Use the value from this data file for 1980 because Ralston considered it more accurate than MRFSS
ca_rec_hist <- read.csv(here("data-raw","ca_hist_recreational_1928_1980_ej.csv"))
ca_rec_hist$QLBKmt <- ca_rec_hist$QLBKmt_North + ca_rec_hist$QLBKmt_South

#Add north and south fleets as areas designations
ca_rec_hist_faa <- data.frame("Year" = ca_rec_hist$Year,
                              "North" = albin_ratio * ca_rec_hist$QLBKmt,
                              "South" = (1 - albin_ratio) * ca_rec_hist$QLBKmt)



###
## RecFIN/MRFSS landings ----
###

# Output from recfin_processing.R

# MRFSS data from 1980-2004 (replace the 1980 value with historical 1980 value)
ca_rec_mrfss <- read.csv(here("data", "CAquillback_mrfss_catches.csv"))
ca_rec_mrfss_faa <- data.frame("YEAR" = ca_rec_mrfss$YEAR,
                               "tot_mt_North" = albin_ratio * ca_rec_mrfss$tot_mt,
                               "disEst_mt_North" = albin_ratio * ca_rec_mrfss$disEst_mt,
                               "landEst_mt_North" = albin_ratio * ca_rec_mrfss$landEst_mt,
                               "tot_mt_South" = (1 - albin_ratio) * ca_rec_mrfss$tot_mt,
                               "disEst_mt_South" = (1 - albin_ratio) * ca_rec_mrfss$disEst_mt,
                               "landEst_mt_South" = (1 - albin_ratio) * ca_rec_mrfss$landEst_mt)
                               

# RecFIN data from 2005-2024
ca_rec_recfin_faa <- read.csv(here("data", "CAquillback_recfin_catches_FAA.csv"))

#Add 2020 proxy for FAA
proxy2020 <- sum(read_excel(here("data-raw", "CDFWRec_QuillbackRF_AvgProxyValuesApr-Jun2020.xlsx"), sheet = "QuillbackRF")$`Proxy Average Value (mt)`)
faa_ratio2020 <- ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR == 2020 & ca_rec_recfin_faa$faa == "North", c("tot_mt", "land_mt")] /
  colSums(ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR == 2020, c("tot_mt", "land_mt")])
#North and South component of it assuming same ratio of 2020 recfin quantities
ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR == 2020 & ca_rec_recfin_faa$faa == "North", c("tot_mt", "land_mt")] <- 
  proxy2020 * faa_ratio2020 + ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR == 2020 & ca_rec_recfin_faa$faa == "North", c("tot_mt", "land_mt")]
ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR == 2020 & ca_rec_recfin_faa$faa == "South", c("tot_mt", "land_mt")] <- 
  proxy2020 * (1 - faa_ratio2020) + ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR == 2020 & ca_rec_recfin_faa$faa == "South", c("tot_mt", "land_mt")]

#Add 2020 and 2021 unallocated rockfish for FAA
update2020_21 <- utils::read.csv(file = here("data-raw", "genus_allocate_quillback_20241205.csv"), header = TRUE)
alloc_val <- update2020_21 %>% dplyr::group_by(year, mode, orig_allocated) %>% 
  dplyr::summarize(sum = sum(quillback_kg) * 0.001) #0.001 to get into MT
faa_ratio2021 <- ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR == 2021 & ca_rec_recfin_faa$faa == "North", c("tot_mt", "land_mt")] /
  colSums(ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR == 2021, c("tot_mt", "land_mt")])
#2020 north and south
ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR %in% c(2020) & ca_rec_recfin_faa$faa == "North",][,c("tot_mt", "land_mt")] <- 
  ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR %in% c(2020) & ca_rec_recfin_faa$faa == "North",][,c("tot_mt", "land_mt")] + 
  faa_ratio2020 * sum(alloc_val[alloc_val$year == 2020 & alloc_val$orig_allocated == "allocated",]$sum)
ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR %in% c(2020) & ca_rec_recfin_faa$faa == "South",][,c("tot_mt", "land_mt")] <- 
  ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR %in% c(2020) & ca_rec_recfin_faa$faa == "South",][,c("tot_mt", "land_mt")] + 
  (1 - faa_ratio2020) * sum(alloc_val[alloc_val$year == 2020 & alloc_val$orig_allocated == "allocated",]$sum)
#2021 north and south
ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR %in% c(2021) & ca_rec_recfin_faa$faa == "North",][,c("tot_mt", "land_mt")] <- 
  ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR %in% c(2021) & ca_rec_recfin_faa$faa == "North",][,c("tot_mt", "land_mt")] + 
  faa_ratio2021 * sum(alloc_val[alloc_val$year == 2021 & alloc_val$orig_allocated == "allocated",]$sum)
ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR %in% c(2021) & ca_rec_recfin_faa$faa == "South",][,c("tot_mt", "land_mt")] <- 
  ca_rec_recfin_faa[ca_rec_recfin_faa$RECFIN_YEAR %in% c(2021) & ca_rec_recfin_faa$faa == "South",][,c("tot_mt", "land_mt")] + 
  (1 - faa_ratio2021) * sum(alloc_val[alloc_val$year == 2021 & alloc_val$orig_allocated == "allocated",]$sum)



###
## GEMM discard values ----
###

# Output from discard_exploration.R (same as for regular fleet structure)
gemm_sector <- read.csv(here("data", "CAquillback_gemm_mortality_and_discard.csv"))
gemm <- gemm_sector %>%
  dplyr::group_by(grouped_sector, year) %>% 
  dplyr::summarize(Tot_Dead = round(sum(Tot_Dead),3),
                   Discard = round(sum(Discard),3),
                   Dead_Discard = round(sum(Dead_Discard),3),
                   Landings = round(sum(Landings),3)) %>%
  dplyr::mutate(., "dis_mort_prop" = round(Dead_Discard/Tot_Dead,3)) %>% 
  data.frame() 

#Add 2024 estimates as provided by the observer program
gemm2024 <- read_excel(here("data-raw", "LangsethReq_ DRAFT_2024_qbdisest.xlsx"), sheet = "DRAFT_2024_qbdisest") %>%
  dplyr::select(area_strat, qbdismort_mt_expanded) %>%
  dplyr::summarise(sum2024 = sum(qbdismort_mt_expanded))


#---------------------------------------------------------------------------------------------------------------#

# Combine data into single overall catch data frame and output ----

#---------------------------------------------------------------------------------------------------------------#

###
# Create new data frame
###

ca_catch_faa <- data.frame("Year" = 1916:2024)
ca_catch_faa$com_tot_North <- NA
ca_catch_faa$com_lan_North <- NA
ca_catch_faa$com_dis_North <- NA
ca_catch_faa$rec_tot_North <- NA
ca_catch_faa$rec_lan_North <- NA
ca_catch_faa$rec_dis_North <- NA
ca_catch_faa$com_tot_South <- NA
ca_catch_faa$com_lan_South <- NA
ca_catch_faa$com_dis_South <- NA
ca_catch_faa$rec_tot_South <- NA
ca_catch_faa$rec_lan_South <- NA
ca_catch_faa$rec_dis_South <- NA


###-----------------------###
## Commercial data ----
###-----------------------###


###
# Add historical commercial data
###

ca_catch_faa[ca_catch_faa$Year %in% ca_com_hist_faa$Year, c("com_lan_North", "com_lan_South")] <- ca_com_hist_faa[, c("North", "South")]


###
# Add pacfin data and fill in gaps
###

ca_catch_faa[ca_catch_faa$Year %in% ca_pacfin_faa$LANDING_YEAR, c("com_lan_North", "com_lan_South")] <- ca_pacfin_faa[, c("North", "South")]

## Fill in gaps

#Sampling occurred during 1981-1983, and in 1985. Therefore assume lack of samples mean 0 landings
ca_catch_faa[ca_catch_faa$Year %in% c(1981:1983, 1985), c("com_lan_North", "com_lan_South")] <- 0



###
# Add commercial discards
###

#Add discards to pacfin years and sum for total assuming same ratio as north/south as landings

ca_catch_faa[ca_catch_faa$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_dis_North"] <-
  ca_pacfin_faa[ca_pacfin_faa$LANDING_YEAR %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "ratio"] * 
  gemm[gemm$grouped_sector == "ca_comm",  "Dead_Discard"]
ca_catch_faa[ca_catch_faa$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "com_dis_South"] <-
  (1 - ca_pacfin_faa[ca_pacfin_faa$LANDING_YEAR %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], "ratio"]) * 
  gemm[gemm$grouped_sector == "ca_comm",  "Dead_Discard"]
ca_catch_faa[ca_catch_faa$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], c("com_tot_North", "com_tot_South")] <-
  ca_catch_faa[ca_catch_faa$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], c("com_lan_North", "com_lan_South")] +
  ca_catch_faa[ca_catch_faa$Year %in% gemm[gemm$grouped_sector == "ca_comm",  "year"], c("com_dis_North", "com_dis_South")]


#Add to historical years

#First need historical proportion of dead discards. Chose proportional to landings
#Calculate from years 2002-2021. This assumes same proportion for north/south
hist_dis_prop <- sum(gemm_sector[gemm_sector$sector == "Nearshore" & 
                                   gemm_sector$year %in% c(2002:2021), "Dead_Discard"]) /
  sum(gemm_sector[gemm_sector$sector == "Nearshore" & 
                    gemm_sector$year %in% c(2002:2021), "Landings"])

#Then calculate dead discards...
ca_catch_faa[!ca_catch_faa$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), c("com_dis_North", "com_dis_South")] <-
  (ca_catch_faa[!ca_catch_faa$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), c("com_lan_North", "com_lan_South")]) *
  hist_dis_prop
#...and total mortality
ca_catch_faa[!ca_catch_faa$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), c("com_tot_North", "com_tot_South")] <-
  ca_catch_faa[!ca_catch_faa$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), c("com_dis_North", "com_dis_South")] +
  ca_catch_faa[!ca_catch_faa$Year %in% c(gemm[gemm$grouped_sector == "ca_comm",  "year"], 2024), c("com_lan_North", "com_lan_South")]

#Now add assumed dead discards to 2024.
#Keep assumption of zero landing for 2024 as for non-FAA setup
#Assume all are in north. Consistent with 2024 ratio, as well as other recent years
ca_catch_faa[ca_catch_faa$Year == 2024, "com_lan_North"] <- 0
ca_catch_faa[ca_catch_faa$Year == 2024, c("com_tot_North", "com_dis_North", 
                                          "com_tot_South", "com_dis_South")] <- 
  c(gemm2024, gemm2024, 0, 0)



###-----------------------###
## Recreational data ----
###-----------------------###


###
# Add historical recreational data
###

ca_catch_faa[ca_catch_faa$Year %in% ca_rec_hist_faa$Year, "rec_lan_North"] <- ca_rec_hist_faa$North
ca_catch_faa[ca_catch_faa$Year %in% ca_rec_hist_faa$Year, "rec_lan_South"] <- ca_rec_hist_faa$South



###
# Add MRFSS data and fill in gaps
###

#Do not add 1980 MRFSS value. Use the estimate from the historical reconstruction instead
ca_catch_faa[(ca_catch_faa$Year %in% ca_rec_mrfss_faa$YEAR) & (!ca_catch_faa$Year %in% 1980), 
             c("rec_tot_North", "rec_lan_North", "rec_dis_North",
               "rec_tot_South", "rec_lan_South", "rec_dis_South")] <- 
  ca_rec_mrfss_faa[!(ca_rec_mrfss_faa$YEAR %in% 1980), c("tot_mt_North", "landEst_mt_North", "disEst_mt_North",
                                                         "tot_mt_South", "landEst_mt_South", "disEst_mt_South")]

## Fill in gaps

# Add extra PC estimates for 1993-1995 as calculated in recfin_processing.R
# Assume same ratio for north/south
pc_avg = 2.460985
ca_catch_faa[ca_catch_faa$Year %in% c(1993:1995), c("rec_tot_North", "rec_lan_North")] <- (pc_avg * albin_ratio) + 
  ca_catch_faa[ca_catch_faa$Year %in% c(1993:1995), c("rec_tot_North", "rec_lan_North")]
ca_catch_faa[ca_catch_faa$Year %in% c(1993:1995), c("rec_tot_South", "rec_lan_South")] <- (pc_avg * (1 - albin_ratio)) + 
  ca_catch_faa[ca_catch_faa$Year %in% c(1993:1995), c("rec_tot_South", "rec_lan_South")]

#Fill in missing 1990-1992 years based on averages of nearby points
average_vals_North <- c(mean(ca_catch_faa$rec_tot_North[ca_catch_faa$Year %in% c(1987:1989)], na.rm = T), #3 yr average before
                        mean(ca_catch_faa$rec_tot_North[ca_catch_faa$Year %in% c(1987:1989, 1993:1995)], na.rm = T), #3 yr average before and after
                        mean(ca_catch_faa$rec_tot_North[ca_catch_faa$Year %in% c(1993:1995)], na.rm = T)) #3 yr average after
average_vals_South <- c(mean(ca_catch_faa$rec_tot_South[ca_catch_faa$Year %in% c(1987:1989)], na.rm = T), #3 yr average before
                        mean(ca_catch_faa$rec_tot_South[ca_catch_faa$Year %in% c(1987:1989, 1993:1995)], na.rm = T), #3 yr average before and after
                        mean(ca_catch_faa$rec_tot_South[ca_catch_faa$Year %in% c(1993:1995)], na.rm = T)) #3 yr average after

ca_catch_faa[ca_catch_faa$Year %in% c(1990:1992), c("rec_tot_North", "rec_tot_South")] <- 
  c(average_vals_North, average_vals_South)


###
# Add RecFIN data
###

ca_catch_faa[ca_catch_faa$Year %in% ca_rec_recfin_faa[ca_rec_recfin_faa$faa == "North",]$RECFIN_YEAR,
             c("rec_tot_North", "rec_lan_North", "rec_dis_North")] <- 
  ca_rec_recfin_faa[ca_rec_recfin_faa$faa == "North", c("tot_mt", "land_mt", "dis_mt")]
ca_catch_faa[ca_catch_faa$Year %in% ca_rec_recfin_faa[ca_rec_recfin_faa$faa == "South",]$RECFIN_YEAR,
             c("rec_tot_South", "rec_lan_South", "rec_dis_South")] <- 
  ca_rec_recfin_faa[ca_rec_recfin_faa$faa == "South", c("tot_mt", "land_mt", "dis_mt")]


###
# Add historical recreational discards
###

#Calculate average from mrfss. Weight by catch - 1.1% (this is the same whether North/South)
ca_rec_hist_discard_rate <- sum(ca_rec_mrfss_faa[,c("disEst_mt_North", "disEst_mt_South")], na.rm = TRUE) / 
  sum(ca_rec_mrfss_faa[,c("landEst_mt_North", "landEst_mt_South")], na.rm = TRUE)

#Then calculate discards mortality...
ca_catch_faa[ca_catch_faa$Year %in% ca_rec_hist_faa$Year, c("rec_dis_North", "rec_dis_South")] <- ca_rec_hist_discard_rate *
  ca_catch_faa[ca_catch_faa$Year %in% ca_rec_hist_faa$Year, c("rec_lan_North", "rec_lan_South")]
#...and sum for total mortality
ca_catch_faa[ca_catch_faa$Year %in% ca_rec_hist_faa$Year, c("rec_tot_North", "rec_tot_South")] <- 
  ca_catch_faa[ca_catch_faa$Year %in% ca_rec_hist_faa$Year, c("rec_dis_North", "rec_dis_South")] +
  ca_catch_faa[ca_catch_faa$Year %in% ca_rec_hist_faa$Year, c("rec_lan_North", "rec_lan_South")]


#---------------------------------------------------------------------------------------------------------------#

# Output final total removals file for fleets as areas ----
# Note that this is confidential because PacFIN is confidential so keep in confidential folder

#---------------------------------------------------------------------------------------------------------------#

#Full version with discards and landings
#write.csv(round(ca_catch_faa,3), file = file.path(here("data", "confidential_noShare", "CAquillback_total_removals_faa.csv")), row.names = FALSE)
