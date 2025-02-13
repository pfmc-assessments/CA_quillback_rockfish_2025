##############################################
# Explore the ROV length data from CDFW
# for the 2025 quillback rockfish stock 
# assessment
#
# Melissa Monk 2/7/2025
##############################################

rm(list=ls(all=TRUE))
graphics.off()

library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

dir <- file.path(here(),"data-raw")
setwd(dir)
fig.dir <- file.path(here(),"data_explore_figs", "survey_figs", "rov")

lengths <- read.csv(file.path(dir,"QuillbackRFStereoSize-11-26-2024.csv"))

dim(lengths)
#683 lengths
str(lengths)

len <- lengths %>%
        mutate_at(c('Month', 'MPA_Group', 'Type', 'LongTerm_Region','Designation', 'Location'), as.factor)
str(len)

summary(len$Type)
summary(len$MPA_Group)

#remove southern CA 
len <- len %>% filter(Lat > 35) %>%
droplevels()

summary(len$Type)
summary(len$MPA_Group)

#assign latitude and depth bins 
len = len %>%
      mutate(dbin = cut(Depth,
                       breaks = c(0,20,30,40,50,60,70,80,90,100))) %>%
    mutate(latbin = cut(Lat, breaks = c(36,37,38,39,40,41,42))) %>%
    mutate_at(vars(dbin,latbin), as.factor)

#length by depth
ggplot(len, aes(x = StereoSize, y = Depth)) + 
geom_jitter() 

ggplot(len, aes(x = StereoSize, y = Lat)) + 
geom_jitter() 


ggplot(len, aes(x = Depth, y = Lat)) + 
geom_jitter() 

#Look at the data with ggridges using the binned lat and depth
ggplot(
  len, aes(x = StereoSize, y = latbin, fill = dbin)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Latitude") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "length_by_latitude.png"), width = 7, height = 7)

#Look at the data with ggridges using the binned lat and depth
ggplot(
  len, aes(x = StereoSize, y = dbin, fill = latbin)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "length_by_depth.png"), width = 7, height = 7)

#Look at the data with ggridges by depth
#No real differences by depth - and smaller with in the mid-depths
ggplot(
  len, aes(x = StereoSize, y = dbin)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d()


with(len, table(latbin, dbin))
#with these samples, it's hard to say anything
#about length by depth

#Look at the data with ggridges using the binned lat and depth
ggplot(
  len, aes(x = StereoSize, y = dbin)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d()


  with(len, table(latbin, Location))

#Look at the data with ggridges using the binned lat and depth
#Look very similar with maybe a few larger fish inside
ggplot(
  len, aes(x = StereoSize, y = Designation)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Length") +
  ylab("Designation") +
  scale_fill_viridis_d()

#Look at depth sampled by designation
ggplot(
  len, aes(x = Depth, y = Designation)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Depth") +
  ylab("Designation") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "length_by_designation.png"), width = 7, height = 7)


############################################################
# Look at length by depth and latitude WIHTOUT the Farallons

nofarallon <-len %>%
filter(!Location %in% c('SE Farallon Islands', 'N Farallon Islands')) %>%
droplevels



#Look at the data with ggridges using the binned lat and depth
ggplot(
  nofarallon, aes(x = StereoSize, y = latbin, fill = as.factor(dbin))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Latitude") +
  scale_fill_viridis_d() #+
#  facet_grid(.~Survey_Year)
ggsave(file = file.path(fig.dir, "length_by_latitudedepth_nofarallon.png"), width = 7, height = 7)


#Look at the data with ggridges using the binned lat and depth
ggplot(
  nofarallon, aes(x = StereoSize, y = dbin, fill = as.factor(Survey_Year))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d() #+
#  facet_grid(.~Survey_Year)
ggsave(file = file.path(fig.dir, "length_by_dpeth_nofarallon.png"), width = 7, height = 7)




#Look at the depths sampled by year and latitude
ggplot(
  nofarallon, aes(x = Depth, y = latbin, fill = as.factor(Survey_Year))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Depth") +
  ylab("Latitude") +
  scale_fill_viridis_d() #+
#  facet_grid(.~Survey_Year)
ggsave(file = file.path(fig.dir, "depth_by_latitude_nofarallon.png"), width = 7, height = 7)

#Get sample sizes to try an discern what's happening
len_dep_pivot <- len %>% 
                group_by(dbin, latbin) %>%
                tally() %>%
                pivot_wider(names_from = latbin, values_from = n, values_fill = 0)
#sample sizes look ok

#look at size by month
ggplot(
  len, aes(x = StereoSize, y = dbin , fill = Month)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5, stat="identity", scale = 1) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d() #+
#  facet_grid(.~Survey_Year)
ggsave(file = file.path(fig.dir, "length_by_monthanddepth.png"), width = 7, height = 7)


#where are all the fish <30cm really from
smallfish <- len %>%
filter(StereoSize <31)

#see if a violin plot shows the same information due to the smoothing in geom_density_ridges
ggplot(len, aes(x = dbin, y = StereoSize, fill = dbin)) +
geom_violin() + 
facet_grid(.~latbin)
ggsave(file = file.path(fig.dir, "length_by_lat_depth_violin.png"), width = 15, height = 7)


#Look at length by specific sites
#Look at the depths sampled by year and latitude
#constrain to 40 to 70 for depth
ggplot(
  len %>% filter(Depth>39, Depth <70),
   aes(x = StereoSize, y = Location, fill = as.factor(Survey_Year))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  scale_fill_viridis_d() #+
##  facet_grid(.~Survey_Year)
ggsave(file = file.path(fig.dir, "length_by_location_year_coredepths.png"), width = 7, height = 7)


ggplot(
  len %>% filter(Depth>39, Depth <70),
   aes(x = StereoSize, y = Location, fill = as.factor(Survey_Year))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  scale_fill_viridis_d() +
 facet_grid(.~dbin)
ggsave(file = file.path(fig.dir, "length_by_location_year_coredepths_bydepth.png"), width = 7, height = 7)


##############################################################################################################
##############################################################################################################
#read in the file from 2023 that contains all quillback up to that point
#look at course size vs stereo size
#look at where stereo size isn't available
#how many quillback by area/year don't have lengths

alllengths <- read.csv(file.path(dir,"Quillback_ROV_2005-2021_11012023.csv"))
alllengths <- alllengths %>% 
filter(SurveyYear > 2013)

#get a sum of all quillback observed
counts <- alllengths %>%
group_by(SurveyYear, Location) %>%
summarise(qlbk_cnt = sum(Count)) %>%
pivot_wider(names_from = SurveyYear, values_from = qlbk_cnt, values_fill = 0)
with(alllengths, table(Location, SurveyYear))

summary(alllengths$CoarseSize)
summary(alllengths$SteroSize)

#look at where the NAs are
nolength <- alllengths %>%
filter(is.na(CoarseSize))
with(nolength, table(Location, SurveyYear))

with(nolength, table(SurveyYear))/with(alllengths %>% filter(SurveyYear != 2016), table(SurveyYear))

#Fraction of fish with NO lengths by year (stereo and coarse)
#SurveyYear
#      2014       2015       2019       2020       2021
#0.09558824 0.11832061 0.03947368 0.11409396 0.06772908


ggplot(alllengths, aes(x = CoarseSize, y = SteroSize)) +
geom_jitter() + geom_abline()


#remove the data with no lengths
havelen <- alllengths %>%
filter(!is.na(CoarseSize))

summary(havelen$CoarseSize)
summary(havelen$SteroSize)


ggplot(havelen, aes(x = CoarseSize)) + geom_histogram() 
ggplot(havelen %>% filter(is.na(SteroSize)), aes(x = CoarseSize)) + geom_histogram()

havelen <- havelen %>%
mutate(lbin = cut(CoarseSize, breaks = c(seq(0,60,5)))) %>%
mutate(slbin = cut(SteroSize, breaks = c(seq(0,60,5)))) %>%
droplevels()


 with(havelen %>% filter(!is.na(SteroSize)), table(lbin)) / with(havelen %>% filter(is.na(SteroSize)), table(lbin))
 with(havelen %>% filter(!is.na(SteroSize)), table(lbin)) / with(havelen, table(lbin))

with(havelen, table(slbin))

########################################################################################
#How many quillback total are at each location in each year
#How many quillback have stereo lengths at each location and year
#How many quillback have only coarse lengths at each location and year

#all data lengths
loc_year <- alllengths %>%
group_by(SurveyYear, Location) %>%
summarise(qlbk_cnt = sum(Count)) %>%
pivot_wider(names_from = SurveyYear, values_from = qlbk_cnt, values_fill = 0)

#at least a coarse length
loc_year_coarse_lengths <- alllengths  %>%
filter(!is.na(CoarseSize)) %>%
group_by(SurveyYear, Location) %>%
summarise(qlbk_cnt = sum(Count)) %>%
pivot_wider(names_from = SurveyYear, values_from = qlbk_cnt, values_fill = 0)


#stereo lengths only
loc_year_stereo_lengths <- alllengths  %>%
filter(!is.na(SteroSize)) %>%
group_by(SurveyYear, Location) %>%
summarise(qlbk_cnt = sum(Count)) %>%
pivot_wider(names_from = SurveyYear, values_from = qlbk_cnt, values_fill = 0)


View(loc_year) 
View(loc_year_coarse_lengths)
View(loc_year_stereo_lengths) 


with(lengths, table(Location, Survey_Year))
with(lengths, table(Survey_Year))
with(havelen, table(SurveyYear))

#all data len - need to change the name of this table
havelen = havelen %>%
      mutate(dbin = cut(Depth,
                       breaks = c(0,20,30,40,50,60,70,80,90,100))) %>%
    mutate(latbin = cut(Lat, breaks = c(36,37,38,39,40,41,42))) %>%
    mutate_at(vars(dbin,latbin), as.factor)

#Look at the data with ggridges using the binned lat and depth
ggplot(
  havelen %>% filter(), aes(x = CoarseSize, y = latbin, fill = as.factor(SurveyYear))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Location") +
  scale_fill_viridis_d() #+
#  facet_grid(.~Survey_Year)


ggplot(
  lengths, aes(x = StereoSize, y = latbin, fill = as.factor(SurveyYear))) + 
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Location") +
  scale_fill_viridis_d()


