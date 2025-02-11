################################################################################
### Process the CDFW CPFV onbard data for an index of abundance
### Depending on the species and the area you may need to modify the filters
###  CA Quillback assessment 2025
### Melissa Monk 1/30/25
################################################################################
rm(list = ls(all = TRUE))
graphics.off()
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)
library(ggridges)
# species and area identifiers - eventually put in function
pacfinSpecies <- "QLBK"
speciesName <- "quillback"
modelArea <- "north"
#model <- "start2004"

# set working directory
dir <- file.path(here(), "data-raw", "rec_indices", "crfs_cpfv_onboard")
setwd(dir)
out.dir <- file.path(here(), "data_explore_figs","rec_indices","crfs_cpfv_onboard")
# load data for processing
load(file.path(dir,  "onboard.RData"))



# Data filter dataframe
filter.num <- 1
dataFilters <- data.frame(matrix(vector(), 20, 4,
  dimnames = list(c(), c(
    "Filter", "Description", "Samples",
    "Positive_Samples"
  ))
), stringsAsFactors = F)

#-------------------------------------------------------------------------------
onboard <- onboard_data %>%
  mutate(area = ifelse(district %in% c(1, 2), "south", "north")) %>%
  filter(area == modelArea) %>%
  filter(effort > 0) %>% droplevels

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("All data")
dataFilters$Description[filter.num] <- c("All data")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#Look at district 3 and quillback presence
pos <- onboard %>% filter(number.fish > 0 )
with(pos, table(county))

#no catches in county 13, 41
#1 quillback in 75 and 87

#filter out san luis obispo 
onboard <- onboard %>% filter(county != 79)

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Southern trips")
dataFilters$Description[filter.num] <- c("Remove San Luis Obispo")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------



# Get the number of available samples by year
samples_year_district <- onboard %>%
  group_by(year, district) %>%
  tally() %>%
  tidyr::pivot_wider(names_from = district, values_from = n)
# View(samples_year_district)
write.csv(samples_year_district, "samples_year_district.csv")

# exploratory plots
ggplot(onboard, aes(x = as.factor(year), y = cpue)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("CPUE")
ggsave(file.path(out.dir, "cpue_by_year.png"), height = 7, width = 7)
# depth by district or area fished
ggplot(
  onboard %>% filter(number.fish > 0, depth1ft < 250),
  aes(
    x = depth1ft / 6, y = as.factor(year),
    fill = as.factor(year)
  )
) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Depth (fm)") +
  ylab("Year") +
  # geom_density_ridges(onboard, aes(x = depth, y = year, colour = year)) +
  scale_fill_viridis_d()
ggsave(file.path(out.dir, "quillback_depths_nofilter.png"))


# Proportion discarded by year
keep_discard <- onboard %>%
  group_by(year) %>%
  summarise(
    kept = sum(kept),
    discard = sum(discd)
  ) %>%
  mutate(`Proportion discarded` = discard / (kept + discard))
write.csv(keep_discard, file.path(dir, "keep_discard_prop.csv"), row.names = FALSE)



# remove 1999-2002 ----
#not much data at all 17 total drifts with quillback
  onboard <- onboard %>%
    filter(year > 2003)

  #-------------------------------------------------------------------------------
  # Add to filter dataframe
  dataFilters$Filter[filter.num] <- c("Years")
  dataFilters$Description[filter.num] <- c("Start time series in 2003 due to sparse data")
  dataFilters$Samples[filter.num] <- onboard %>% tally()
  dataFilters$Positive_Samples[filter.num] <- onboard %>%
    filter(number.fish > 0) %>%
    tally()
  filter.num <- filter.num + 1
  #-------------------------------------------------------------------------------

# IF Depth isn't available and GIS depth is - add that in
# ONLY using starting depth which is depth1ft, and the gis depths are based on
# start locations
# remove any remaining drifts with no depth information
onboard <- onboard %>%
  mutate(depth = ifelse(is.na(depth1ft) & gis90depthft > 0, gis90depthft, depth1ft)) %>%
  filter(!is.na(depth))

# how many positive drifts is depth missing from
pos_data <- onboard %>% filter(number.fish > 0)
summary(pos_data$depth)
summary(as.factor(pos_data$LocationTableError))

# check fish time and observed anglers
summary(onboard$fishtime)
summary(onboard$obsang)

# remove any drifts with a location table error, missing fish time and missing
# number of anglers

onboard <- onboard %>%
  filter(is.na(LocationTableError)) %>%
  filter(!is.na(fishtime)) %>%
  filter(!is.na(obsang)) %>%
  filter(!is.na(reef)) %>%
  filter(fishtime > 0)

# missing reef info
summary(onboard$reef)

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Errors and Missing Data")
dataFilters$Description[filter.num] <- c("Remove drifts with missing data and identified errors")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

# Keep ocean waters only
onboard <- onboard %>%
  filter(waterarea %in% c("N", "O"))

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Area fished")
dataFilters$Description[filter.num] <- c("Remove drifts in bays")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------



# remove Jan-March in the north - no rockfishing these months
  onboard <- onboard %>%
    filter(month %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12))


#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Months fished")
  dataFilters$Description[filter.num] <- c("Remove Jan-March; recreational rockfish fishery closed")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

# positive data
pos_data <- onboard %>%
  filter(number.fish > 0) %>%
  filter(depth < 500)
# look at depth distribution
ggplot(pos_data, aes(x = depth, fill = district)) +
  geom_histogram() +
  xlab("Depth (ft)") +
  ylab("Number of drifts with quillback") +
  scale_fill_viridis_d()
ggsave(file = file.path(out.dir, "drifts_by_depth_district.png"), width = 7, height = 7)

depth_quantile2 <- quantile(onboard$depth, seq(0, 1, .01))
depth_quantile1 <- quantile(pos_data$depth, seq(0, 1, .01))
depth_quantile <- round(quantile(pos_data$depth, c(0.01, .99)), 0)
depth_quantile
# remove upper and lower 1% 240 ft is 40 fm 
onboard <- onboard %>%
  filter(depth <= 242,
         depth >= 50) %>%
 # filter(!region == "0") %>%
  droplevels()
# %in% (depth_quantile[1]:depth_quantile[2]))

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Depth")
  dataFilters$Description[filter.num] <- c("Remove upper and lower 1% of depth with observed coppers;
                                           Remaining drifts between 50 and 242 feet")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

# look at number of observed anglers
summary(onboard$obsang)
obsang_quantile <- quantile(pos_data$obsang, seq(0, 1, .025))
obsang_quantile

# remove upper and lower 2.5%
# CRFS manual suggests a max number of anglers to observe is 12
onboard <- onboard %>%
  filter(obsang %in% obsang_quantile[2]:obsang_quantile[40])

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Observed anglers")
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% of observed anglers;
                                           Remaining drifts with 4-12 observed anglers")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

summary(onboard$fishtime)
fishtime_quantile <- quantile(pos_data$fishtime, seq(0, 1, .025))
fishtime_quantile

# remove upper and lower 2.5%
# Drifts less than 5 minutes probably were not successful
onboard <- onboard %>%
  filter(
    fishtime > fishtime_quantile[2],
    fishtime < fishtime_quantile[40]
  )


#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Time fished")
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% time fished and
                                         time fished; Remaining drifts with 7-90 minutes time fished")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

  # subset automatically to 500m
  onboard <- onboard %>%
    filter(reef.dist < 500)

  # Look at distance from reef for the positives
  pos_data <- onboard %>% filter(number.fish > 0)

  summary(pos_data$reef.dist)

  ggplot(pos_data, aes(x = reef.dist, y = cpue, colour = district)) +
    geom_jitter(alpha = 0.5)

  # Cumulative distribution of distance from reef
  ggplot(pos_data, aes(reef.dist)) +
    stat_ecdf(geom = "step")


  reef.dist_quantile <- quantile(pos_data$reef.dist, seq(0, 1, .01))
  reef.dist_quantile
  # keep 95% of the data

  onboard <- onboard %>%
    filter(reef.dist < reef.dist_quantile[96]) %>%
    droplevels()

  onboard <- onboard %>%
    mutate(region = district)
  #-------------------------------------------------------------------------------
  # Add to filter dataframe
  dataFilters$Filter[filter.num] <- c("Distance from rocky substrate")
  dataFilters$Description[filter.num] <- c("After removing observations further
than 0.5km from rocky substrate, keep 95% of the data; drifts within 46 m of rocky substrate")
  dataFilters$Samples[filter.num] <- onboard %>% tally()
  dataFilters$Positive_Samples[filter.num] <- onboard %>%
    filter(number.fish > 0) %>%
    tally()
  filter.num <- filter.num + 1
  #-------------------------------------------------------------------------------

# depth by district or area fished
ggplot(
  onboard %>% filter(number.fish > 0),
  aes(
    x = depth / 6, y = as.factor(year),
    fill = as.factor(year)
  )
) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Depth (fm)") +
  ylab("Year") +
  # geom_density_ridges(onboard, aes(x = depth, y = year, colour = year)) +
  scale_fill_viridis_d() +
  facet_grid(.~district)
ggsave(file.path(out.dir, "quillback_depths_gisdepthadded.png"))


#look at the positive data
pos <- onboard %>% filter(number.fish > 0) %>% droplevels
with(pos, table(district, year))

#Look at the reefs and see if we can filter further
pos.reefs <-  pos %>% 
            group_by(reef) %>%
            summarise(numberfish = sum(number.fish),
                      trips = length(unique(ID))) %>%
            filter(numberfish >10)




#filter to just the reefs with quillback observations
onboard <- onboard %>%
       filter(reef %in% pos.reefs$reef)

 #-------------------------------------------------------------------------------
  # Add to filter dataframe
  dataFilters$Filter[filter.num] <- c("Reefs with quillback")
  dataFilters$Description[filter.num] <- c("Retain reefs with at least 10 quillback 
                            observed from 10 different trips")
  dataFilters$Samples[filter.num] <- onboard %>% tally()
  dataFilters$Positive_Samples[filter.num] <- onboard %>%
    filter(number.fish > 0) %>%
    tally()
  filter.num <- filter.num + 1
  #-------------------------------------------------------------------------------
View(dataFilters)
# final tables and visualizations
# Get the number of available samples by year
samples_year_district <- onboard %>%
  group_by(year, district) %>%
  tally() %>%
  tidyr::pivot_wider(names_from = district, values_from = n)
# View(samples_year_district)
write.csv(samples_year_district,
  file.path(dir, "samples_by_year_district.csv"),
  row.names = FALSE
)

# primary target species only
driftTargets <- onboard %>%
  group_by(district) %>%
  summarise(
    driftsWithTarget = sum(number.fish > 0),
    driftsWOTarget = sum(number.fish == 0)
  ) %>%
  mutate(
    totaldrifts = driftsWithTarget + driftsWOTarget,
    percentpos = driftsWithTarget / (driftsWithTarget + driftsWOTarget)
  )
write.csv(driftTargets,
  file.path(dir, "driftTargets.csv"),
  row.names = FALSE
)


# look at water area by district
# you can see in 2017 that they started fishing outside state waters
waterarea_year <- onboard %>%
  filter(number.fish > 0) %>%
  group_by(waterarea, district) %>%
  tally() %>%
  pivot_wider(names_from = waterarea, values_from = n) %>%
  mutate(percent_N = N / (N + O))
write.csv(waterarea_year, file.path(dir, "target_water_area.csv"))


# sample sizes by month
samples_month_year <- onboard %>%
  group_by(year, month) %>%
  tally() %>%
  tidyr::pivot_wider(names_from = month, values_from = n)
samples_month_year



cpue_by_district <- onboard %>%
  group_by(year, district) %>%
  summarise(average_cpue = mean(cpue))

# look average cpue by district
ggplot(cpue_by_district, aes(x = year, y = average_cpue, colour = district)) +
  geom_point(size = 3) +
  theme_bw() +
  geom_line(aes(x = year, y = average_cpue, colour = district)) +
  xlab("Year") +
  ylab("Average CPUE") +
  ylim(c(0, (max(cpue_by_district$average_cpue) * 1.1))) +
  scale_color_viridis_d()
ggsave(file = file.path(getwd(), "average_cpue_by_district.png"), width = 7, height = 7)



# save the datafile and filters for the run file
save(onboard, dataFilters,
  file = file.path(dir, "data_for_glm.RData")
)
