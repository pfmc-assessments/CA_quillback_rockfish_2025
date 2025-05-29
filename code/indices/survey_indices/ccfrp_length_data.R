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
plot.dir <- file.path(here(), "data_explore_figs", "survey_figs")
# Load in data from various sources
# CRFS 2017-2024 - all areas
len <- read.csv(file.path(dir,  "CCFRP_lengths.csv"))
# The lengths have already been filtered based on the retained records 
len <- len %>% mutate(depth_bin = cut(depth, breaks = c(0, 40,80,120,160,200)))
len$depth_bin <- as.factor(len$depth_bin)
summary(as.factor(len$depth_bin))
#look at depth


#convert length to meters fro comparisons with ROV data that is in meters
 len <- len %>% mutate(depth_meters = depth/3.281) %>%
         mutate(depth_bin_meters = cut(depth_meters, breaks = c(0,20,30,40,50,60,70,80,90,100)))
 len$depth_bin_meters <- as.factor(len$depth_bin_meters)
 summary(as.factor(len$depth_bin_meters))


#boxpot in meters to compare with rov
 ggplot(
   len, aes(y = length_cm, x = depth_bin_meters, fill = depth_bin_meters)) +
   geom_boxplot() +
   ylim(0,60) +
   xlab("Depth bin meters") +
   ylab("Length cm") +
   scale_fill_viridis_d(begin = 0, end = .8)
 ggsave(file = file.path(plot.dir, "ccfrp_lengths_by_depth_meters.png"), width = 7, height = 7)

ggplot(len) +
  geom_boxplot(aes(y = length_cm, colour = as.factor(year))) +
  facet_wrap('name')
ggsave(file = file.path(plot.dir, "ccfrp_lengths_by_year_area.png"), width = 7, height = 7)

ggplot(len) +
  geom_boxplot(aes(y = length_cm, colour = as.factor(site))) +
  facet_wrap('name')
ggsave(file = file.path(plot.dir, "ccfrp_lengths_by_site_area.png"), width = 7, height = 7)



ggplot(len) +
  geom_boxplot(aes(y = length_cm, colour = depth_bin)) +
  facet_wrap('name')
ggsave(file = file.path(plot.dir, "ccfrp_lengths_by_depth_area.png"), width = 7, height = 7)

ggplot(len) +
  geom_boxplot(aes(y = length_cm, colour = depth_bin))
ggsave(file = file.path(plot.dir, "ccfrp_lengths_by_depth.png"), width = 7, height = 7)



#remove the Farallons from initial length comps
#len.nofn <- len %>%
#filter(!grepl("FN", gridCellID))


#===============================================================================
# Sample size calculation and input N
#===============================================================================

sample_by_group <- len %>%
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
len$Sex <- "U"

#len.nofn <- len.nofn %>% rename(Length_cm = length_cm, Year = year)
len_final <- len %>%
   dplyr::rename(Length_cm = length_cm, Year = year) %>%
dplyr::select(fishID, driftID, gridCellID, Length_cm, Sex, Year, site, name)
len_final$Sex <- as.factor(len_final$Sex)

 n <- len_final %>%
   dplyr::group_by(Year) %>%
   dplyr::summarise(
     drifts = length(unique(driftID)))

#This is missin one of the lengths and can't figure out why- it's in 2022 in the 38-3 bin9
 lfs <- UnexpandedLFs.fn(
        dir = file.path(dir),
        sex = 0,    #Sex has to be set to 0 if you have all unsexed fish
        datL = len_final,
        lgthBins = length_bins,
        partition = 0,
        fleet = "ccfrp",
        month = 7)
lfs <- as.data.frame(lfs)

lfs[, "comps.Nsamp"] <- n$drifts


#Now process and weight the lengths
n <- len_final %>%
  dplyr::group_by(Year, site) %>%
  dplyr::summarise(
    drifts = length(unique(driftID)))

lfs_mpa <-  UnexpandedLFs.fn(
  dir = file.path(dir),
  sex = 0,
  datL = len_final[len_final$site == "MPA", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = "ccfrp", 
  month = 7)
  lfs_mpa <- as.data.frame(lfs_mpa)
lfs_mpa[,"InputN"] <- n$drifts

lfs_ref <-  UnexpandedLFs.fn(
  dir = file.path(dir),
  sex = 0,
  datL = len_final[len_final$site == "REF", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 4, 
  month = 7)
  lfs_ref <- as.data.frame(lfs_ref)
lfs_ref[,"InputN"] <- n$drifts

protect <- 0.2; open <- 1 - protect
ind <- 7:ncol(lfs_mpa)
tmp <- lfs_mpa[, ind] * protect + lfs_ref[, ind] * open

first <- 1:(length(length_bins))
#second <- (length(length_bins) + 2):ncol(tmp)
# This is for unsexed composition data only 
lfs <- round(tmp[, first] /  apply(tmp[, first], 1, sum), 4)
      #      round(100 * tmp[, second] / apply(tmp[, second], 1, sum), 4))
out <- cbind(lfs_ref[,1:5], "InputN" = n, lfs)




## Create table of sample sizes and trips lengths
dataN <- len_final %>%
  dplyr::mutate(source = "CCFRP") %>%
  dplyr::group_by(source, Year) %>% 
  dplyr::summarize(Nfish = length(Length_cm),
                   Ndrift = length(unique(driftID))) %>%
  tidyr::pivot_wider(names_from = source,
                     values_from = c(Nfish, Ndrift),
                     names_glue = "{source}_{.value}") %>%
  dplyr::arrange(Year) %>%
  data.frame()
dataN[is.na(dataN)] <- 0
write.csv(dataN, here("data", "SampleSize_length_CCFRP.csv"), row.names = FALSE)


## Create table of sample sizes and trips ages for non-growth fleet sources
dataN_age <- data %>% dplyr::filter(!is.na(age)) %>%
  dplyr::group_by(source, Year) %>% 
  dplyr::summarize(Nfish = length(age),
                   Ntrip = length(unique(tripID))) %>%
  tidyr::pivot_wider(names_from = source,
                     values_from = c(Nfish, Ntrip),
                     names_glue = "{source}_{.value}") %>%
  dplyr::arrange(Year) %>%
  data.frame()
dataN_age[is.na(dataN_age)] <- 0
#write.csv(dataN_age, here("data", "SampleSize_age.csv"), row.names = FALSE)




write.csv(out, file = file.path(here("data","forSS3", "Lcomps_ccfrp_withFN_weighted_length_comps_unsexed.csv")), row.names = FALSE)


