###############################################################
# Script to look at the available length distribution of samples 
# and length of aged fish for quillback
# rockfish for the 2025 California stock assessment
#Author: Melissa Monk SWFSC
#      4/28/2025
###############################################################
rm(list = ls(all = TRUE))
graphics.off()

library(ggplot2)
library(dplyr)
library(here)
library(nwfscSurvey)

setwd(here())
fig.dir<- (here("data_explore_figs"))

#read in the lengths at age and also the lengths
laa <- read.csv(here("data-raw", "ages", "QLBK_faa_age_length.csv"))
lengths <- read.csv(here("data", "length_processed_noShare", "CAquillback_ALL_bio.csv"))

names(laa) # "year"      "length_cm" "age"       "sex"       "source"    "faa_area"
names(lengths)
# [1] "Year"       "length_cm"  "weight_kg"  "age"        "sex"
# [6] "depth_m"    "lat"        "lon"        "area"       "mode"
#[11] "disp"       "wgt_flag"   "lngth_flag" "source"     "tripID"
lengths <- lengths %>% mutate(aged = case_when(is.na(age) ~ "No", !is.na(age) ~ "Yes"))
lengths$aged <- as.factor(lengths$aged)

summary(as.factor(laa$source))
summary(as.factor(lengths$source))
pacfin_only <- lengths %>% filter(source == "pacfin")
#pacfin lengths for aged and unaged fish - unweighted comps
ggplot(lengths %>% filter(source == "pacfin"), aes(x = as.factor(Year), y = length_cm, fill = aged)) +
geom_boxplot() +
scale_fill_viridis_d()
ggsave(file = file.path(fig.dir, "pacfin_length_and_laa_unweighted_boxplots.png"), width = 7, height = 7)

#number of aged fish by year
with(lengths %>% filter(source == "pacfin", aged == "Yes"), table(Year))
# 2007 2011 2012 2019 2020 2021 2022 2023 2024
#  27    1    4   75   73   32   71   16    3

#Look at the length distribution between the cdfw rec collected fish and coop fish and the rec lengths
rec_lengths <- lengths %>% filter(source == "recfin", Year %in% c(2010, 2011, 2021:2024)) %>% dplyr::select(c(Year, length_cm, source)) %>% rename(year = Year)
laa_rec <- laa %>% filter(source %in% c("Cooperative", "CRFS", "RBG", "SWFSCResearch", "Abrams")) %>% dplyr::select(c(year, length_cm, source))

all_rec <- rbind(rec_lengths, laa_rec)

with(all_rec %>% filter(source != "recfin"), table(year, source))
#year   Abrams Cooperative CRFS RBG SWFSCResearch
#  2010     37           0    0   0             0
#  2011     79           0    0   0             0
#  2021      0           0   35   3             0
#  2022      0         132   43  43             0
#  2023      0           2   17   9            23
#  2024      0           0   16   0            73
#  2025      0           0    0   0            40


ggplot(all_rec) +
geom_boxplot(aes(x = as.factor(year), y = length_cm, fill = source)) +
scale_fill_viridis_d() + 
labs(x = "Year", y = "Length (cm)")
ggsave(file = file.path(fig.dir, "rec_length_and_laa_boxplots.png"), width = 7, height = 7)
#with only CRFS, Cooperative
#Look at the length distribution between the cdfw rec collected fish and coop fish and the rec lengths
rec_lengths2 <- lengths %>% filter(source == "recfin", Year %in% c(2021:2024)) %>% dplyr::select(c(Year, length_cm, source)) %>% rename(year = Year)
laa_rec2 <- laa %>% filter(source %in% c("Cooperative", "CRFS")) %>% dplyr::select(c(year, length_cm, source))

all_rec2 <- rbind(rec_lengths2, laa_rec2)

with(all_rec2, table(year, source))
#year   Cooperative CRFS recfin
#  2021           0   35    256
#  2022         132   43    409
#  2023           2   17    134
#  2024           0   16     37

ggplot(all_rec2) +
geom_boxplot(aes(x = as.factor(year), y = length_cm, fill = source)) +
scale_fill_viridis_d() + 
theme_bw(base_size = 18) +
labs(x = "Year", y = "Length (cm)")


with(laa %>% filter(!source %in% c("calcom","pacfin","trawl","comm85","Comm2019","IPHC","SMURFS")), table(source, year))
with(laa, table(source))


###all data, not just rec
test_lengths <- lengths %>% dplyr::select(c(Year, length_cm, source, aged)) %>% rename(year = Year)
laa_test <- laa %>% mutate(aged = "Yes") %>% dplyr::select(c(year, length_cm, source, aged))

all_dat <- rbind(test_lengths, laa_test)



com_fish <- c( "pacfin" , "trawl", "Comm2019" , "calcom" , "comm85")
rec_fish <- c("recfin", "mrfss", "deb", "GeiCol","MilGot","MilGei", "Cooperative" , "CRFS" )
surveys <- c("triennial", "Abrams" , "CCFRPNotFarallons", "CCFRPFarallons", "SWFSCResearch","SMURFS", "IPHC", "RBG")


all_dat <- all_dat %>% mutate(simple_source = case_when(source %in% com_fish ~ "com",
                                                        source %in% rec_fish ~ "rec",
                                                        source %in% surveys ~ "survey"))

all_dat <- all_dat %>% filter(simple_source != "com")
ggplot(all_dat) +
geom_boxplot(aes(x = as.factor(source), y = length_cm, fill = aged)) +
scale_fill_viridis_d() + 
theme_bw(base_size = 18) + 
theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
labs(x = "Source", y = "Length (cm)")
ggsave(file = file.path(fig.dir, "noncomm_length_and_laa_unweighted_boxplots.png"), width = 7, height = 7)