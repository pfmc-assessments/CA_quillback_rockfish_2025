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
#Sex = 1 is male, Sex = 2 is female, Sex = 3 is unsexed
ca %>%
    group_by(Sex) %>%
    tally() 

######Data plots
#plot by faceted project
ggplot(ca, aes(y = length_cm, x = age, color = Sex)) +
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
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_at_length_by_project.png"),
       width = 10, height = 8)

#plot by faceted sex
ggplot(ca, aes(y = length_cm, x = age, color = project)) +
	geom_point(alpha = 0.1) + 
  theme_bw() + 
  geom_jitter() + 
  xlim(1, 60) + ylim(1, 60) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text.y = element_text(size = 16),
         legend.text = element_text(size = 20),
        panel.grid.minor = element_blank()) + 
	facet_grid(rows = vars(Sex)) + 
	xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_at_length_bysex.png"),
       width = 10, height = 8)


#Plot just carcass and ccfrp sampling
ca_1 <- ca %>% filter(project %in% c("Carcass Sampling", "CCFRP"))
ggplot(ca_1, aes(y = length_cm, x = age, color = project)) +
	geom_point(alpha = 0.1) + 
  theme_bw() + 
  geom_jitter() + 
  xlim(1, 60) + ylim(1, 60) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
	xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
#ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_at_length_by_project.png"),
#       width = 10, height = 8)

#############################################
#estimate growth
#2021 estimate Linf = 43.04 k = .199 to = -0.067
#2025 CA only est: 

age_df <- ca
age_df$Age <- age_df$age
age_df$Length_cm <- age_df$length_cm
age_df <- age_df %>% 
filter(project != "Carcass Sampling")

vb_est_all<- est_vbgrowth(
  dir = NULL, 
  dat = age_df, 
  col_length = "length_cm",
  col_age = "age",
  init_params = data.frame(K = 0.12, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))
vb_est_all$all_growth
 #        K       Linf         L0        CV0        CV1
 #0.1782938 40.9971124  3.9225748  0.2262690  0.0649841

#males only
length_age_males <- est_vbgrowth(
 dir = file.path(here(),"data-raw"),
  dat = subset(age_df, Sex == 1), 
  col_length = "length_cm",
  col_age = "age",
   init_params = data.frame(K = 0.12, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))
length_age_males$all_growth
#males
#           K         Linf           L0          CV0          CV1
# 0.131052985 41.724881311 13.105886995  0.192873891  0.006705831

#females only
length_age_females <- est_vbgrowth(
 dir = file.path(here(),"data-raw"),
  dat = subset(age_df, Sex == 2), 
  col_length = "length_cm",
  col_age = "age",
   init_params = data.frame(K = 0.12, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))
length_age_females$all_growth
#females
#           K         Linf           L0          CV0          CV1
# 0.093386826 44.362969045 16.842822351  0.168755023  0.001117803

#unsexed only
length_age_unsexed <- est_vbgrowth(
  dir = file.path(here(),"data-raw"),
  dat = subset(age_df, Sex == 3), 
  col_length = "length_cm",
  col_age = "age",
  init_params = data.frame(K = 0.12, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))
length_age_unsexed$all_growth
#unsexed
#        K         Linf           L0          CV0          CV1
# 0.14501755 41.62102296 12.58916457  0.11829281  0.07596045

##################################################
#Get the predictions
ages <- seq(0, 60, by = 0.2)
vb_fn <- function(age, Linf, L0, k) {
    #vec <- Linf * (1 - exp( -k * (age - t0)))
    vec <- Linf - (Linf - L0) * exp(-age * k)
    return(vec)
}
vb_fn2 <- function(age, Linf, t0, k) {
    vec <- Linf * (1 - exp( -k * (age - t0)))
   # vec <- Linf - (Linf - L0) * exp(-age * k)
    return(vec)
}
preds1 <- data.frame(ages,
                 fit = vb_fn(ages, Linf = vb_est_all[[3]][2], L0 = vb_est_all[[3]][3], k = vb_est_all[[3]][1]))

preds2 <- data.frame(ages,
                 fit = vb_fn2(ages,  Linf = 43.02, t0 = -0.067, k = 0.199))


#Plot data with fit 
ggplot() +
	geom_jitter(data = age_df, aes(y = length_cm, x = age), alpha = .8) + 
  geom_line(data = preds1, aes(y = fit, x = ages, colour = "CA data"), linewidth = 2) +
  geom_line(data = preds2, aes(y = fit, x = ages, colour = "2021 est"), linewidth = 2) +
  theme_bw() + 
  xlim(0, 60) + ylim(0, 50) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text.y = element_text(size = 16),
       legend.text = element_text(size = 20),
        panel.grid.minor = element_blank()) + 
	xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d() 

# ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_at_length_bysex.png"),
#       width = 10, height = 8)


<<<<<<< HEAD
=======
#Plot data with fits by sex
ggplot(ca, aes(y = length_cm, x = age, col = Sex)) +
  geom_point(alpha = 0.1) + 
  theme_bw() + 
  xlim(1, 60) + ylim(1, 60) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.text = element_text(size = 20),
        panel.grid.minor = element_blank()) + 
  facet_grid(rows = vars(Sex)) +
  geom_function(data = data.frame(age = 0, length_cm = 0, Sex = "1"),
                fun = vb_fn,
                args = list(Linf = length_age_males$all_growth["Linf"], 
                            L0 = length_age_males$all_growth["L0"],
                            k = length_age_males$all_growth["K"])) +
  # geom_text(data = data.frame(age = 40, length_cm = 20, Sex = "1"), 
  #           label = paste0("Linf = ", length_age_males$all_growth["Linf"])) +
  geom_function(data = data.frame(age = 0, length_cm = 0, Sex = "2"),
                fun = vb_fn,
                args = list(Linf = length_age_females$all_growth["Linf"], 
                            L0 = length_age_females$all_growth["L0"],
                            k = length_age_females$all_growth["K"])) +
  geom_function(data = data.frame(age = 0, length_cm = 0, Sex = "3"),
                fun = vb_fn,
                args = list(Linf = length_age_unsexed$all_growth["Linf"], 
                            L0 = length_age_unsexed$all_growth["L0"],
                            k = length_age_unsexed$all_growth["K"])) +
  geom_function(data = data.frame(age = 0, length_cm = 0), aes(col = "All"),
                fun = vb_fn,
                args = list(Linf = vb_est_all$all_growth["Linf"], 
                            L0 = vb_est_all$all_growth["L0"],
                            k = vb_est_all$all_growth["K"])) +
  xlab("Age") + ylab("Length (cm)")
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_at_length_bysex_withFits.png"),
       width = 6, height = 8)


>>>>>>> origin/main

