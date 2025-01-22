###############################################################
#Script look at the available age at length data for quillback
#rockfish for the 2025 California stock assessment
#
#Author: Melissa Monk SWFSC
#      1/17/2025
###############################################################
rm(list = ls(all = TRUE))
graphics.off()

library(ggplot2)
library(dplyr)
library(here)
library(nwfscSurvey)

setwd(here())
#fig.dir <- file.path(here(),"data_explore_figs")
#read in the data
#This is a data dump from Patrick McDonald (NWFSC CAP lab) of all the 
#quillback his lab has aged

qlbk <- read.csv(file.path(here(), "data-raw", "QLBK_Data_Dump_4_Monk.csv"))

#get the wcgbts locations to filter out OR/WA
load(file.path(here(),"data-raw", "bio_quillback rockfish_NWFSC.Combo_2024-09-04.rdata"))

ca_bottomtrawl <- x %>%
filter(Latitude_dd <42, !is.na(Otosag_id))

#remove Oregon and northern data from the survey
ca <- qlbk %>%
filter(case_when (source == "OR" ~ F,
       (source == "NWFSC" & specimen_id %in% ca_bottomtrawl$Otosag_id) ~ T,
       source == "CA" ~ T,
       T ~ F))

#Recategorize CCFRP to all the same
ca <- ca %>%
filter(!is.na(length_cm)) %>%
mutate(project = sample_type) 
ca$project[grepl("CCFRP", ca$project)] <- "CCFRP"
ca$project[grepl("Recreational", ca$project)] <- "CDFW"
ca$project[grepl("Rec.", ca$project)] <- "CDFW"
ca$project[grepl("Research", ca$project) & ca$year %in% c(2010,2011)] <- "Abrams"
ca$project[grepl("Research", ca$project)] <- "CDFW" #remaining research go to CDFW
ca$project[grepl("Unknown", ca$project) & ca$year == 2007] <- "SWFSC boxed"
ca$project[grepl("Unknown", ca$project) & ca$year < 2007] <- "CDFW"


#rename columns
ca <- ca %>%
rename(age = age_best, 
       Sex = sex)
ca$Sex <- as.factor(ca$Sex)
#tally by sample type - renamed project to group
#using this to check things
ca %>%
 #filter(project == "Combined Survey") %>%
    group_by(project) %>%
    tally() 
 
#how many by sex
ca %>%
    group_by(Sex) %>%
    tally() 


age_df <- ca
age_df$Age <- age_df$age
age_df$Length_cm <- age_df$length_cm

ages_all <- est_growth(
  dir = NULL, 
  dat = age_df, 
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
  sdFactor = 3)

#check to see if any outside the 3 sds
remove <- which(ages_all[,'length_cm'] > ages_all[,'Lhat_high'] | ages_all[,'length_cm'] < ages_all[,'Lhat_low'])
ages_all[remove, ]

#none
plot(ages_all[remove, 'Age'], ages_all[remove, "Length_cm"], xlim = c(0,40), ylim = c(0, 50)) 
clean_ages <- age_df

length_age_ests_all <- est_growth(
  dat = clean_ages, #age_df, 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

#save(length_age_ests_all, file = file.path(dir,"biology", 'length_at_age_ests_all.rdata'))

length_age_ests_north <- est_growth(
  dat = age_df[age_df$area == "north", ], 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

#jplot
ggplot(age_df, aes(y = length_cm, x = age, color = Sex)) +
	geom_point(alpha = 0.1) + 
  theme_bw() + 
  geom_jitter() + 
  xlim(1, 60) + ylim(1, 60) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
	facet_grid(project~.) + 
	xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
#ggsave(filename = file.path(dir, "biology", "plots", "age_at_length.png"),
#       width = 10, height = 8)


