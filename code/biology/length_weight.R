##############################################################################################################
#
# 	Purpose: Analyze and calculate available weight-length data
#
#   Created: February 12, 2025
#			  by Brian Langseth 
#
#   Uses output from the following scripts
#     comps_workup.R
#
#
##############################################################################################################

library(here)
library(magrittr)
library(ggplot2)
library(r4ss)


##
#Read in output of summarized length data produced in comps_workup.R
##

data <- read.csv(here("data", "length_processed_NoShare", "CAquillback_ALL_bio.csv"))

#Rename variables so can do length_weight_plot
data$Source <- data$source
data$Sex <- data$sex
data$Length <- data$length_cm
data$Weight <- data$weight_kg
data[which(data$source %in% c("trawl", "triennial")), "Source"] <- "NOAA surveys"


##
#Plots of length and weight data
##

dir.create(here("data_explore_figs", "bio_figs"))


#Use dataModerate_2021 plots for exploration. Move to bio_figs folder
source("https://raw.githubusercontent.com/brianlangseth-NOAA/dataModerate_2021/refs/heads/master/R/length_weight_plot.R")
length_weight_plot(dir = here("data_explore_figs", "bio_figs"), splits = NA, nm_append = NULL, ests = NULL,
                   data = data[-which(data$wgt_flag %in% "computed" | data$lngth_flag %in% "computed"),])
file.rename(from = here("data_explore_figs", "bio_figs", "plots", "Length_Weight_by_Sex.png"),
            to = here("data_explore_figs", "bio_figs", "Length_Weight_by_Sex.png"))
file.rename(from = here("data_explore_figs", "bio_figs", "plots", "Length_Weight_by_Source.png"),
            to = here("data_explore_figs", "bio_figs", "Length_Weight_by_Source.png"))
unlink(here("data_explore_figs", "bio_figs", "plots"), recursive=TRUE)


#And now overall plot with estimates

#There are VERY few sexes so just include overall
data_lw = data[-which(data$wgt_flag %in% "computed" | data$lngth_flag %in% "computed"),]
table(data_lw$source)
table(data_lw$sex)

lw_ests <- nwfscSurvey::estimate_weight_length(data = data_lw %>% dplyr::select(!c("Sex", "Source", "Length")))
nwfscDiag::pngfun(wd = here("data_explore_figs", "bio_figs"), file = "Length_Weight_withEsts.png", 
                  w = 7, h = 7, pt = 12)
plot(data_lw$length_cm, data_lw$weight_kg, 
     xlab = "Length (cm)", ylab = "Weight (kg)", main = "", 
     ylim = c(0, max(data_lw$weight_kg, na.rm = TRUE)), xlim = c(0, max(data_lw$length_cm, na.rm = TRUE)), 
     pch = 16)
lens = 1:max(data_lw$length_cm, na.rm = TRUE)
lines(lens, lw_ests[3, "A"] * lens ^ lw_ests[3, "B"], col = "red", lwd = 2, lty = 1)
#2021 assessment relationship
#lines(lens, 1.963e-5 * lens ^ 3.02, col = "red", lwd = 2, lty = 3)
# #Include Oregon and Washington estimates from the WCGBTS (see survey_workup.R)
# lines(lens, 8.5602e-6 * lens ^ 3.22, col = "cyan", lwd = 2, lty = 3)
# leg = c("Estimate: 1.599e-5, b = 3.07",
#         "2021 assessment: 1.963e-5, b = 3.02",
#         "Coastwide WCGBTS: 8.56e-6, b = 3.22")
#leg = c("Estimate: 1.599e-5, b = 3.07",
#        "2021 assessment: 1.963e-5, b = 3.02")
legend("topleft", bty = 'n', lty = c(1,2,2), col = c("red", "red", "cyan"), lwd = 2)
dev.off()  

#write.csv(lw_ests, here("data", "lw_ests.csv"), row.names = FALSE)
file.copy(from = here("data_explore_figs", "bio_figs","Length_Weight_withEsts.png"),
          to = here('report', 'figures','bio_length_weight.png'), 
          overwrite = TRUE, recursive = FALSE)
