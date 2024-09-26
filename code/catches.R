##############################################################################################################
#
# 	Purpose: Compile catch streams for use in stock synthesis
#
#   Created: Sept 26, 2024
#			  by Brian Langseth 
#
#   Uses output from the following scripts, combines them, and then fills in gaps
#     pacfin_processing.R
#
#
##############################################################################################################

#---------------------------------------------------------------------------------------------------------------#

# Load the data ----

#---------------------------------------------------------------------------------------------------------------#

###
# Historical commercial landings 
###

# These files are copied from the 2021 quillback archives, and then saved in the data-raw folder
file.copy(from = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/PacFIN Catch/ca_hist_commercial_1916_1968_ej_Feb2021_CAlandingsCaughtORWA.csv",
          to = here("data-raw","ca_hist_commercial_1916_1968_ej_Feb2021_CAlandingsCaughtORWA.csv"),
          overwrite = FALSE)
file.copy(from = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/PacFIN Catch/ca_hist_commercial_1969_1980_ej.csv",
          to = here("data-raw","ca_hist_commercial_1969_1980_ej.csv"),
          overwrite = FALSE)

# Historical 1916-1968 Landings (from EJ based on Ralston reconstruction) metric tons. 
# Original 1916-1968 data have other and trawl gear, but in csv they are aggregated into a single time series.
# These include landings in CA caught in OR/WA waters, which total 0.27 MT and are no more than 0.079 in any one year 
ca_com_hist1 <- read.csv(here("data-raw", "ca_hist_commercial_1916_1968_ej_Feb2021_CAlandingsCaughtORWA.csv"))
# Historical 1968-1980 Landings lbs
# Original 1968-1980 data have hook and line and trawl gear, but in csv they are aggregated into time series by area. 
ca_com_hist2 <- read.csv(here("data-raw", "ca_hist_commercial_1969_1980_ej.csv"))


###
# PacFIN landings
###

ca_pacfin <- read.csv(here("data", "CAquillback_pacfin_landings.csv"))



###
# Historical recreational landings
###

# These files are copied from the 2021 quillback archives, and then saved in the data-raw folder
file.copy(from = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/6_non_confidential_data/RecFIN Catch/ca_hist_recreational_1928_1980_ej.csv",
          to = here("data-raw","ca_hist_recreational_1928_1980_ej.csv"),
          overwrite = FALSE)
# Historical 1928-1980 Landings metric tons
# Use the value from this data file for 1980 because Ralston considered it more accurate than MRFSS
# We also have data broken out by CPFV (charter) and shore/skiff (private) but this spreadsheet has a single time series
ca_rec_hist = read.csv(here("data-raw","ca_hist_recreational_1928_1980_ej.csv"))
ca_rec_hist$QLBKmt = ca_rec_hist$QLBKmt_North + ca_rec_hist$QLBKmt_South




                      