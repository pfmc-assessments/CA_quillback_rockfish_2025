#############################################################################
# This file is for exploration of the genetically ID'd rockfish from 
# Diana Baetscher's dissertation - SMURF caught fish
# 
#
# Melissa Monk 2/20/2025
##############################################################################

rm(list=ls(all=TRUE))
graphics.off()

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(lubridate)
library(geomtextpath)

dat <- read.csv(here("data-raw","Baetscher_juvenile_rockfish_NSF_dispersal_proj_genetic_ids.csv"))
str(dat)

summary(as.factor(dat$GENETIC_ID))
summary(as.factor(dat$MORPHOLOGY_ID))


mal <- dat %>%
filter(GENETIC_ID == "Smaliger",
       !is.na(LENGTH))

#201 quillback
summary(mal$LATITUDE_M)
#36.52 to 36.63 and 19 NA's for latitude

summary(as.factor(mal$COLLECTION_DATE))

#Turn character date to acutal date and then get the day within the year
mal$collect_date <- mdy(mal$COLLECTION_DATE)
mal$cal_day <- yday(mal$collect_date)
mal$year <- year(mal$collect_date)
mal$length_cm <- mal$LENGTH/10

ggplot(mal, aes(x = cal_day, y = length_cm)) +
geom_point(size = 3, shape = 21, stroke = 1.5, fill = "white") +
annotate("text", x = 175, y = 5.5, label = "July 1", angle = 90, size = 12) +
geom_vline(xintercept = 180) +
labs(x = "Calendar Day",
     y = "Length (cm)")

with(mal, table(year))
#2010 2015 2016
#  19   11  162

nearjuly1 <- mal %>% filter( cal_day >166, cal_day < 200)
mean(nearjuly1$length_cm)
#3.8 cm

with(mal, table(year))
