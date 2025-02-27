###################################################
# This file is for exploration only - not SS input
# length exploration for quillback 2025
# 
# Uses output from the length comp processing file
#     comps_workup.R
#
# Melissa Monk 2/11/2025
###################################################

rm(list=ls(all=TRUE))
graphics.off()

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)

biodata <- read.csv(here("data", "length_processed_noShare", "CAquillback_ALL_bio.csv"))
str(biodata)
fig.dir<- (here("data_explore_figs"))
with(biodata, table(source))
with(biodata, table(area))
with(biodata, table(area, source))

#remove trawl and triennial
rm.source <- c("trawl", "triennial")
biodata <- biodata %>%
filter(!source %in% rm.source,
area != "South")

#create a region column
biodata <- biodata %>%
    mutate(region = 
     case_when(
        area %in% c('CCA', 'ERA', 'Redwood') ~ "1Redwood",
        area %in% c('Wine', 'BGA') ~ "2Wine",
        area %in% c('Central', 'Bay', 'BDA', 'SFA','MNA', 'MRA') ~ "3CentralBay"))


with(biodata, table(as.factor(region)))
with(biodata, table(area,region))

with(biodata, table(region, source))
with(biodata, table(mode))
summary(as.factor(biodata$mode))

#replace NAs with COM for commercial
biodata <- biodata %>%
mutate_at(vars(mode), ~replace_na(., "COM"))

with(biodata, table(mode, region))

ggplot(biodata, aes(length_cm)) +
geom_histogram()

#remove any length over 60 cm
biodata <- biodata %>%
filter(length_cm < 60)
#removes 1 fish


#start looking at length data over time and by each region
ggplot(biodata, aes(x = length_cm, fill = mode)) +
geom_histogram()


ggplot(biodata, aes(x = length_cm, fill = region)) +
geom_density(alpha = .5) +
facet_grid(row = vars(mode))



bio <- biodata #%>%
#filter(Year > 1995)

#Look at lengths as ggridges to see if there's patterns over time
ggplot(
  bio, aes(x = length_cm, y = as.factor(Year), fill = as.factor(mode))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Year") +
  scale_fill_viridis_d() #+
#  facet_grid(.~Survey_Year)
ggsave(file = file.path(fig.dir, "rec_length_by_mode_year_wcom_ggridges.png"), width = 7, height = 7)


year_mode <- bio %>%
group_by(Year, mode) %>%
tally() %>%
pivot_wider(names_from  = mode, values_from = n, values_fill = 0)
View(year_mode)
summary(biodata$Year)

#remove 2020 and 2002 - so few samples
bio <- bio %>% filter(!Year %in% c(2002, 2020))
#Just rec by
recbio <- bio %>% filter(!mode == "COM")
recbio <- recbio %>% mutate_at(vars(Year, region, mode), as.factor)

#Look at lengths as ggridges to see if there's patterns over time
ggplot(
  recbio, aes(x = length_cm, y = as.factor(Year), fill = as.factor(mode))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Year") +
  scale_fill_viridis_d()
#Plots these by region
ggplot(
  recbio, aes(x = length_cm, y = as.factor(Year), fill = as.factor(mode))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Year") +
  facet_grid(.~region) +
  scale_fill_viridis_d()

#Look at lengths by individual fleet and time
ggplot(
  bio, aes(x = length_cm, y = as.factor(Year), fill = as.factor(region))) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Year") +
  facet_grid(.~ mode) +
  scale_fill_viridis_d()

#by mode overall
ggplot(bio %>% filter(source=="recfin"), aes(x = length_cm, fill = mode)) +
geom_density(alpha = .5) +
facet_grid(rows = vars(region))




#length by year mode
ggplot(
  recbio, aes(x = length_cm, y = Year, fill = mode)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Year") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "rec_length_by_year_mode_ggridges.png"), width = 7, height = 7)

#length by year mode and region
ggplot(
  recbio, aes(x = length_cm, y = Year, fill = mode)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Year") +
  facet_grid(~region) +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "rec_length_by_year_mode_region_ggridges.png"), width = 7, height = 7)


#length by year and region
ggplot(
  recbio, aes(x = length_cm, y = Year, fill = region)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Year") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "rec_length_by_year_region_ggridges.png"), width = 7, height = 7)

#length by year and region
ggplot(
  recbio, aes(x = length_cm, y = region, fill = Year)) +
  geom_density_ridges(show.legend = TRUE, alpha = .5) +
  xlab("Length") +
  ylab("Region") +
  scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "rec_length_by_region_year_ggridges.png"), width = 7, height = 7)


with(recbio, table(Year, region))

#summaries
recbio <- bio %>% filter(!mode == "COM")
length_summary <- recbio %>%
group_by(Year, region) %>%
summarise(mean_lngth = mean(length_cm), 
          median_lngth = median(length_cm), 
          n = n())
View(length_summary)

test <- length_summary %>% filter(n >29)
View(test)

#plot average lenght through time
ggplot(length_summary, aes(x = Year, y = mean_lngth, fill = region, size = n)) +
geom_point(shape = 21, , colour = "black", ) +  
scale_fill_viridis_d() + geom_vline(xintercept = c(1999, 2002, 2017))
 
#plot average lenght through time
ggplot(length_summary %>% filter(Year >1998), aes(x = Year, y = mean_lngth, fill = region, size = n)) +
geom_point(shape = 21, , colour = "black", ) +  
scale_fill_viridis_d() + geom_vline(xintercept = c(1999, 2002, 2017))
