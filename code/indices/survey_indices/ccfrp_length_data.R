##################################################################################################
#
#	        Process CCFRP Length Data
# for use in the Quillback Rockfish Assessent in 2025
# 					
#
##################################################################################################
rm(list = ls(all = TRUE))
graphics.off()

library(here)
library(dplyr)
library(nwfscSurvey)
# From Chantel: Note of the nwfscSurvey package: I did revisions to the unexpandeLf.fn 
# to work with our data. This function is available in the unexpand_comps
# branch on github.
library(ggplot2)

dir <- file.path(here(), "data-raw", "ccfrp")

# Load in data from various sources
# CRFS 2017-2024 - all areas
len <- read.csv(file.path(dir,  "CCFRP_lengths.csv"))
# The lengths have already been filtered based on the retained records 
len$depth_m <- len$mean_start_depth_ft/3.281
len <- len %>% mutate(depth_bin_m = cut(depth_m, breaks = c(17,30,40,50)))
len$depth_bin_m <- as.factor(len$depth_bin_m)
summary(as.factor(len$depth_bin_m))
#look at depth
#10m bins
summary(len$mean_start_depth_ft)


ggplot(len) +
  geom_boxplot(aes(y = length_cm, colour = as.factor(year))) +
  facet_wrap('name')

ggplot(len) +
  geom_boxplot(aes(y = length_cm, colour = as.factor(depth_bin_m))) 

ggplot(len) +
  geom_boxplot(aes(y = length_cm, colour = depth_bin_m)) +
  facet_wrap('name')

with(len, table(year, name))

ggplot(len, aes(x = mean_start_depth_ft, y = length_cm)) +
geom_point()

ggplot(len, aes(x = release_depth, y = length_cm)) +
geom_jitter(alpha = 0.5)

#remove the Farallons from initial length comps
len.nofn <- len %>%
filter(!grepl("FN", gridCellID))


#===============================================================================
# Sample size calculation and input N
#===============================================================================

sample_by_group <- len.nofn %>%
  dplyr::group_by(year, name, site) %>%
  dplyr::summarise(
    drifts = length(unique(driftID)),
    n = length(length_cm))

colnames(sample_by_group) <- c("Year", "Location", "Site", "Drifts", "Lengths")
write.csv(sample_by_group,
          file = file.path(dir,  "ccfrp_samples_location_sites.csv"), row.names = FALSE)


#===============================================================================
# Process the Lengths
#===============================================================================
length_bins <- seq(10, 50, by = 2)

#len <- len %>% mutate(len_bins = cut(length_cm, breaks = seq(9,51, by = 2)))
#Assign all to have unknown sex since we are not using a sex specific model
len.nofn$Sex <- "U"

#len.nofn <- len.nofn %>% rename(Length_cm = length_cm, Year = year)
len_final <- len.nofn %>%
   dplyr::rename(Length_cm = length_cm, Year = year) %>%
dplyr::select(fishID, driftID, gridCellID, Length_cm, Sex, Year)
len_final$Sex <- as.factor(len_final$Sex)

 n <- len_final %>%
   dplyr::group_by(Year) %>%
   dplyr::summarise(
     drifts = length(unique(driftID)))

#This is missin one of the lengths and can't figure out why- it's in 2022 in the 38-39 bin
 lfs <- UnexpandedLFs.fn(
        dir = file.path(dir),
        sex = 0,    #Sex has to be set to 0 if you have all unsexed fish
        datL = len_final,
        lgthBins = length_bins,
        partition = 0,
        fleet = "ccfrp",
        month = 7)

lfs$comps[, "Nsamp"] <- n$drifts

write.csv(lfs$comps, file = file.path(dir, "ccfrp_noFN_length_comps_unsexed.csv"), row.names = FALSE)
