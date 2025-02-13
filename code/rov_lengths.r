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

###########################################################################################################
# This section is just the newest stereo size length file from CDFW 
lengthsNewStereo <- read.csv(file.path(dir,"QuillbackRFStereoSize-11-26-2024.csv"))

dim(lengthsNewStereo)
#683 lengths
str(lengthsNewStereo)

lenNewStereo <- lengthsNewStereo  %>%
        mutate_at(c('Month', 'MPA_Group', 'Type', 'LongTerm_Region','Designation', 'Location'), as.factor)
str(lenNewStereo)

summary(lenNewStereo$Type)
summary(lenNewStereo$MPA_Group)

#remove southern CA  - those are coppers
lenNewStereo <- lenNewStereo %>% filter(Lat > 35) %>%
droplevels()

summary(lenNewStereo$Type)
summary(lenNewStereo$MPA_Group)

#assign latitude and depth bins 
lenNewStereo = lenNewStereo %>%
      mutate(dbin = cut(Depth,
                       breaks = c(0,20,30,40,50,60,70,80,90,100))) %>%
    mutate(latbin = cut(Lat, breaks = c(36,37,38,39,40,41,42))) %>%
    mutate_at(vars(dbin,latbin), as.factor)

#length by depth
ggplot(lenNewStereo, aes(x = StereoSize, y = Depth)) + 
geom_jitter() 

#lengths by latitude
ggplot(lenNewStereo, aes(x = StereoSize, y = Lat)) + 
geom_jitter() 


ggplot(lenNewStereo, aes(x = Depth, y = Lat)) + 
geom_jitter() 

#Look at the data with ggridges using the binned lat and depth
ggplot(
  lenNewStereo, aes(x = StereoSize, y = latbin, fill = dbin)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Latitude") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "newstereolength_by_latitude_ggridges.png"), width = 7, height = 7)

#Look at the data with ggridges using the binned lat and depth
ggplot(
  lenNewStereo, aes(x = StereoSize, y = dbin, fill = latbin)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "newstereolength_by_depth_latitude_ggridge.png"), width = 7, height = 7)

#Look at the data with ggridges by depth
#No real differences by depth - and smaller with in the mid-depths
ggplot(
  lenNewStereo, aes(x = StereoSize, y = dbin)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "newstereolength_by_depth_ggridge.png"), width = 7, height = 7)

with(lenNewStereo, table(latbin, dbin))
#with these samples, it's hard to say anything
#about length by depth

#Look at the data with ggridges using the binned lat and depth
ggplot(
  lenNewStereo, aes(x = StereoSize, y = dbin)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d()


  with(len, table(latbin, Location))

#Look at the data with ggridges using the binned lat and depth
#Look very similar with maybe a few larger fish inside
ggplot(
  lenNewStereo, aes(x = StereoSize, y = Designation)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Length") +
  ylab("Designation") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "newstereolength_by_designation_ggridge.png"), width = 7, height = 7)

#Look at depth sampled by designation
ggplot(
  lenNewStereo, aes(x = Depth, y = Designation)) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Depth") +
  ylab("Designation") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "newstereolength_by_designation_ggridges.png"), width = 7, height = 7)


############################################################
# # Look at length by depth and latitude WIHTOUT the Farallons

# nofarallon <-lenNewStereo %>%
# filter(!Location %in% c('SE Farallon Islands', 'N Farallon Islands')) %>%
# droplevels



# #Look at the data with ggridges using the binned lat and depth
# ggplot(
#   nofarallon, aes(x = StereoSize, y = latbin, fill = as.factor(dbin))) +
#   geom_density_ridges(show.legend = TRUE, alpha = .5) +
#   xlab("Length") +
#   ylab("Latitude") +
#   scale_fill_viridis_d() #+
# #  facet_grid(.~Survey_Year)
# ggsave(file = file.path(fig.dir, "length_by_latitudedepth_nofarallon.png"), width = 7, height = 7)


# #Look at the data with ggridges using the binned lat and depth
# ggplot(
#   nofarallon, aes(x = StereoSize, y = dbin, fill = as.factor(Survey_Year))) +
#   geom_density_ridges(show.legend = TRUE, alpha = .5) +
#   xlab("Length") +
#   ylab("Depth") +
#   scale_fill_viridis_d() #+
# #  facet_grid(.~Survey_Year)
# ggsave(file = file.path(fig.dir, "length_by_dpeth_nofarallon.png"), width = 7, height = 7)




# #Look at the depths sampled by year and latitude
# ggplot(
#   nofarallon, aes(x = Depth, y = latbin, fill = as.factor(Survey_Year))) +
#   geom_density_ridges(show.legend = TRUE, alpha = .5) +
#   xlab("Depth") +
#   ylab("Latitude") +
#   scale_fill_viridis_d() #+
# #  facet_grid(.~Survey_Year)
# ggsave(file = file.path(fig.dir, "depth_by_latitude_nofarallon.png"), width = 7, height = 7)

#Get sample sizes to try an discern what's happening
len_dep_pivot <- lenNewStereo %>% 
                group_by(dbin, latbin) %>%
                tally() %>%
                pivot_wider(names_from = latbin, values_from = n, values_fill = 0)
#sample sizes look ok

#look at size by month
ggplot(
  lenNewStereo, aes(x = StereoSize, y = dbin , fill = Month)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d() #+
#  facet_grid(.~Survey_Year)
ggsave(file = file.path(fig.dir, "newstereolength_by_monthdepth_ggridges.png"), width = 7, height = 7)


#where are all the fish <30cm really from
smallfish <- len %>%
filter(StereoSize < 31)

#see if a violin plot shows the same information due to the smoothing in geom_density_ridges
ggplot(lenNewStereo, aes(x = dbin, y = StereoSize, fill = dbin)) +
geom_violin() + 
facet_grid(. ~ latbin)


#Look at length by specific sites
#Look at the depths sampled by year and latitude
#constrain to 40 to 70 for depth
ggplot(
  lenNewStereo %>% filter(Depth>39, Depth <70),
   aes(x = StereoSize, y = Location, fill = as.factor(Survey_Year))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  scale_fill_viridis_d() #+
##  facet_grid(.~Survey_Year)
ggsave(file = file.path(fig.dir, "newstereolength_by_location_year_coredepths_ggridges.png"), width = 7, height = 7)


ggplot(
  lenNewStereo %>% filter(Depth>39, Depth <70),
   aes(x = StereoSize, y = Location, fill = as.factor(Survey_Year))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  scale_fill_viridis_d() +
 facet_grid(.~dbin)
ggsave(file = file.path(fig.dir, "newstereolength_by_location_year_coredepths_bydepth_ggridges.png"), width = 7, height = 7)


##############################################################################################################
##############################################################################################################
#NOW LOOK AT STEREO AND COARSE (LASER) LENGTHS
#read in the file from 2023 that contains all quillback up to that point
#look at course size vs stereo size
#look at where stereo size isn't available
#how many quillback by area/year don't have lengths

#Need to get an updated file from CDFW - the newer file doesn't have all of the new stereo lengths
alllengths <- read.csv(file.path(dir,"Quillback_ROV_2005-2021_11012023.csv"))
#1566 rows
alllengths <- alllengths %>% 
filter(SurveyYear > 2013) %>%
#only look at years 2014-2021 and latitude greater than 35
filter(Lat > 35) %>% droplevels()
#1537 total rows

#For now  - ignoring the few fish where the count is 2 - very minimal
#this is still all quillback and not just those with at least one length
with(alllengths, table(Location, SurveyYear))
#This indicates that we could possibly look at a relative index for 2014, 2015, 2020, 2021
summary(alllengths$CoarseSize)
#Missing 148 coarse lengths
summary(alllengths$SteroSize)
#missing 1008 stereo lengths


#get a sum of all quillback observed
counts <- alllengths %>%
group_by(SurveyYear, Location) %>%
summarise(qlbk_cnt = sum(Count)) %>%
pivot_wider(names_from = SurveyYear, values_from = qlbk_cnt, values_fill = 0)

#look at where the NAs are
nolength <- alllengths %>%
filter(is.na(CoarseSize))
with(nolength, table(Location, SurveyYear))
#Mostly in 2020  this might change with updated data?

with(nolength, table(SurveyYear))/with(alllengths %>% filter(SurveyYear != 2016), table(SurveyYear))

#Fraction of fish with NO lengths by year (stereo and coarse)
#SurveyYear
#      2014       2015       2019       2020       2021
#  0.09629630   0.11832061 0.03947368 0.11409396 0.06772908
#2015 and 2020 have about 11% of all fish with no length data

#look at stereo vs coarse length
ggplot(alllengths, aes(x = CoarseSize, y = SteroSize)) +
geom_jitter() + geom_abline()
ggsave(file = file.path(fig.dir, "stereo_vs_coarse_lengths.png"), width = 7, height = 7)

#get the differences in stereo vs coarse
alllengths$lendiff <- alllengths$CoarseSize - alllengths$SteroSize
ggplot(alllengths, aes(x = lendiff)) +
geom_histogram() 


#remove the data with no lengths
havelen <- alllengths %>%
filter(!is.na(CoarseSize))


summary(havelen$CoarseSize)
 #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
 #  7.00   26.00   31.00   30.58   35.00   54.00

summary(havelen$SteroSize)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   8.00   24.50   32.00   30.61   37.00   53.00     862

ggplot(havelen, aes(x = CoarseSize)) + geom_histogram() 
ggplot(havelen %>% filter(is.na(SteroSize)), aes(x = CoarseSize)) + geom_histogram()


#add length bins by 5 cm - makes assumptions, but look to see
havelen <- havelen %>%
mutate(lbin = cut(CoarseSize, breaks = c(seq(0,60,5)))) %>%
mutate(slbin = cut(SteroSize, breaks = c(seq(0,60,5)))) %>%
droplevels()

#get the fraction of fish with stereo size by length bin vs no stereo size?
 #with(havelen %>% filter(!is.na(SteroSize)), table(lbin)) / with(havelen %>% filter(is.na(SteroSize)), table(lbin))
# with(havelen %>% filter(!is.na(SteroSize)), table(lbin)) / with(havelen, table(lbin))

#Look at the proportion of fish in each length bin by stereo or coarse size
with(havelen, table(slbin))/dim(havelen %>% filter(!is.na(SteroSize)))[1]
with(havelen, table(lbin))/dim(havelen)[1]
#more discrepency with large fish - makes sense

########################################################################################
#How many quillback total are at each location in each year
#How many quillback have stereo lengths at each location and year
#How many quillback have only coarse lengths at each location and year

#How many stero lengths are we missing from old data
dim(lenNewStereo)
#676 should be the total number of available stereo lengths

dim(havelen %>% filter(!is.na(SteroSize)))
#527 - so missing 149 stereo lengths in the older data

#are these all from a year?
#new data stereo only
with(lenNewStereo, table(Survey_Year))
#2014 2015 2016 2019 2020 2021
# 114   53    3   94  293  119

#older data
with(havelen %>% filter(!is.na(SteroSize)), table(SurveyYear))
#2014 2015 2019 2020 2021
#   9    8   94  296  120
# Mostly in 2014 and 2015; why are there more fish in the older 
#data in 2020-2021 than the new data?


#############################################################
# Look at sample sizes by year and location
#all quillback
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


#stereo lengths only from new data
loc_year_stereo_lengths <- lenNewStereo %>%
group_by(Survey_Year, Location) %>%
summarise(qlbk_cnt = n()) %>%
pivot_wider(names_from = Survey_Year, values_from = qlbk_cnt, values_fill = 0)

#compare the available length samples
View(loc_year) 
View(loc_year_coarse_lengths)
View(loc_year_stereo_lengths)

#add the same depth and latitude breaks to the older data with coarse length
havelen = havelen %>%
      mutate(dbin = cut(Depth,
                       breaks = c(0,20,30,40,50,60,70,80,90,100))) %>%
    mutate(latbin = cut(Lat, breaks = c(36,37,38,39,40,41,42))) %>%
    mutate_at(vars(dbin,latbin), as.factor)



#Look at the coarse lengths data with ggridges using the binned lat survey year
ggplot(
  havelen %>% filter(!SurveyYear %in% c(2016, 2019), Depth <70, Depth >20), 
  aes(x = CoarseSize, y = latbin, fill = as.factor(SurveyYear))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Latitude Bin") +
  scale_fill_viridis_d() 
ggsave(file = file.path(fig.dir, "coarselength_filtered_by_location_year_ggridges.png"), width = 7, height = 7)

#look at stereo sizes
ggplot(
   lenNewStereo %>% filter(!Survey_Year %in% c(2016, 2019), Depth <70, Depth >20), 
    aes(x = StereoSize, y = latbin, fill = as.factor(Survey_Year))) + 
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Latitude Bin") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "newstereolength_filtered_by_location_year_ggridges.png"), width = 7, height = 7)


###############################################################################################
# Look at the stereo size distibution vs course size distribution
#create a dataframe
both_sizes <- havelen %>%
filter(!is.na(SteroSize)) %>%
dplyr::select(ID, latbin, Location, SurveyYear, CoarseSize, SteroSize) %>%
pivot_longer(cols = c("CoarseSize", "SteroSize"), names_to = "LenType", values_to = "length") %>%
droplevels()


#For the data I have - compare the distribution of stereo size and coarse size
#plot as histogram and coarse size
 ggplot(both_sizes, aes(x = length)) +
 geom_histogram(data = subset(both_sizes, LenType == "CoarseSize"), fill = "red", alpha = .2) +
 geom_histogram(data = subset(both_sizes, LenType == "SteroSize"), fill = "blue", alpha = .2)
 #geom_density(alpha = .5) 


#now look at by year
ggplot(
  both_sizes, aes(x = length, y = latbin, fill = as.factor(LenType))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Location") +
  scale_fill_viridis_d()
with(both_sizes %>% filter(LenType == "CoarseSize"), table(latbin))




#####
#Latitude notes
# 36 - 37 Big Sur to Davenport; Lobos and Point Sur
# 37 - 38 Davenport to Pt. Reyes; Farallons
# 38 - 39 Pt. Reyes to Pt. Arena; Bodega Bay, Point Arena, Stewarts Point
# 39 - 40 Pt. Arena to Shelter Cove ALbion, MacKerricher, Noyo, Ten Mile, Tolo Bank
# 40 - 41 Shelter Cove to Trinidad; Big Flat, Mattole Canyon, Sea Lion Gulch, South Cape M
# 41 - 42 Trinidad to Oregon; Crescent Cirty, Point St. George, Reading Rock