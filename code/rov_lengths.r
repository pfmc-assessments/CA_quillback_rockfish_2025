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
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Depth") +
  scale_fill_viridis_d() #+
#  facet_grid(.~Survey_Year)
ggsave(file = file.path(fig.dir, "length_by_monthanddepth.png"), width = 7, height = 7)


#where are all the fish <30cm really from
smallfish <- len %>%
filter(StereoSize <31)
