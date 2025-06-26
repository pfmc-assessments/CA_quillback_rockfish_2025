##############################################################################################################
#
# 	Purpose: Compile length data and generate comps for use in stock synthesis
#            Also useful as a single one stop shop for calculating length and age based biological relationships
#            Do so for recreational, commercial, growth, and ROV fleets. 
#            For CCFRP length comps see ccfrp_length_data.R
#
#   Created: Jauary 3, 2025
#			  by Brian Langseth 
#
#   Uses output from the following scripts, combines them, and then generate comps
#     pacfin_processing.R
#     recfin_processing.R
#     catches.R
#
#   For age comps, non-commercial data were accessed from 
#     age_length_cleanup_by_area.R
#
##############################################################################################################

library(here)
library(magrittr)
#devtools::install_github("pfmc-assessments/nwfscDiag")
library(nwfscDiag)
library(ggplot2)
#devtools::install_github("pfmc-assessments/nwfscSurvey")
library(nwfscSurvey)
#devtools::install_github("pfmc-assessments/pacfintools")
#pak::pkg_install("pfmc-assessments/pacfintools")
devtools::load_all("U:/Stock assessments/PacFIN.Utilities")
#library(pacfintools)


#---------------------------------------------------------------------------------------------------------------#

# Pull together all the length data ----

#---------------------------------------------------------------------------------------------------------------#

##
#Load all biological data
##

## PacFIN bds
bio_pacfin <- read.csv(here("data","length_processed_noShare","CAquillback_com_bio.csv"), header = TRUE)

## RecFIN bds
bio_recfin <- read.csv(here("data","length_processed_noShare","CAquillback_rec_bio.csv"), header = TRUE)

## MRFSS bds
bio_mrfss <- read.csv(here("data","length_processed_noShare","CAquillback_mrfss_bio.csv"), header = TRUE)
#Remove 1997-1998 PC data because these are duplicated in Deb's data
bio_mrfss <- bio_mrfss[-which(bio_mrfss$Year %in% c(1997,1998) & bio_mrfss$mode == "PC"),]
table(bio_mrfss$Year, bio_mrfss$mode)

## Deb Wilson Vandenberg bds
bio_deb <- read.csv(here("data","length_processed_noShare","CAquillback_deb_bio.csv"), header = TRUE)

## Geibel and Collier bds
bio_gc <- read.csv(here("data","length_processed_noShare","CAquillback_historical_bio_skiff.csv"), header = TRUE)

## Miller and G* Historical bds
bio_mg <- read.csv(here("data","length_processed_noShare","CAquillback_historical_bio.csv"), header = TRUE)

## Survey bds
bio_survey <- read.csv(here("data","length_processed_noShare","CAquillback_wcgbts_triennial_bio.csv"), header = TRUE)

## ROV bds
bio_rov <- read.csv(here("data", "length_processed_noShare", "CAquillback_rov_bio.csv"), header = TRUE)


##
#Combine into one list
##

input = list()
input[[1]] = bio_pacfin
input[[2]] = bio_recfin
input[[3]] = bio_mrfss
input[[4]] = bio_deb
input[[5]] = bio_gc 
input[[6]] = bio_mg
input[[7]] = bio_survey
input[[8]] = bio_rov

##
#Use 'create_data_frame' function to combine all data elements into a data frame
#Modified from the function in the dataModerate_2021 repo
##
 
#' Create function to take data from multiple sources in a list and create 
#' a single data frame that can be used for biological comparisons
#'
#' @param list of data sets
#' @param names the names of the column variables from each data set
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel (modified by Brian Langseth on January 7, 2025)
#' @export
#'
create_data_frame <- function(data_list, names = c("Year",
                                                   "length_cm",
                                                   "weight_kg",
                                                   "age",
                                                   "sex",
                                                   "depth_m",
                                                   "lat",
                                                   "lon",
                                                   "area",
                                                   "mode",
                                                   "disp",
                                                   "wgt_flag",
                                                   "lngth_flag",
                                                   "source",
                                                   "tripID")){
  
  all_data = NA
  for (a in 1:length(data_list)){
    
    #Are any variables in names not in the dataframe? If so add them
    names_test <- names %in% colnames(data_list[[a]])
    
    if(any(!names_test)) {
      
      data_list[[a]][names[!names_test]] <- NA
      
    }
    
    all_data = rbind(all_data, data_list[[a]][,names])			
  }
  
  all_data = all_data[!is.na(all_data$Year), ] #Remove the first line of NAs
  return (all_data)
}

#Combine into a single data frame with all desired variables included
data <- create_data_frame(input)
#write.csv(data, here("data","length_processed_noShare", "CAquillback_ALL_bio.csv"), row.names = FALSE)


#---------------------------------------------------------------------------------------------------------------#

## Tables ----

#---------------------------------------------------------------------------------------------------------------#

## Create table of sample sizes and trips lengths
dataN <- data %>%
  dplyr::group_by(source, Year) %>% 
  dplyr::summarize(Nfish = length(length_cm),
                   Ntrip = length(unique(tripID))) %>%
  tidyr::pivot_wider(names_from = source,
                     values_from = c(Nfish, Ntrip),
                     names_glue = "{source}_{.value}") %>%
  dplyr::arrange(Year) %>%
  data.frame()
dataN[is.na(dataN)] <- 0
#write.csv(dataN, here("data", "SampleSize_length.csv"), row.names = FALSE)

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


## 
#Create figure of sample sizes and trips for length and age fleets (for STAR panel presentation)
#Pull from the summary tables
##

#Lengths
len_ccfrp <- read.csv(here("data", "SampleSize_length_CCFRP.csv")) %>%
  dplyr::mutate(CCFRP_Ntrip = CCFRP_Ndrift) %>% dplyr::select(-CCFRP_Ndrift)
len_rov <- read.csv(here("data", "SampleSize_length_ROV.csv"))
len_other <- read.csv(here("data", "SampleSize_length.csv")) %>%
  dplyr::select(-c(ROV_Nfish, ROV_Ntrip, 
                   trawl_Nfish, trawl_Ntrip,
                   triennial_Nfish, triennial_Ntrip)) #remove the ROV here because output in superyears

len_temp1 <- merge(len_other, len_ccfrp, by = "Year", all.x = TRUE)
len_temp2 <- merge(len_temp1, len_rov, by = "Year", "all.x" = TRUE)
len_ALL <- len_temp2 %>% tidyr::pivot_longer(cols = -Year,
                                             names_to = c("fleet", "type"),
                                             names_sep = "_",
                                             values_to = "N") %>% dplyr::mutate(mod_fleet = fleet)
len_ALL$mod_fleet <- dplyr::case_when(len_ALL$mod_fleet %in% c("pacfin") ~ "Com",
                                      len_ALL$mod_fleet %in% c("GeiCol", "MilGei", "MilGot", "deb", "mrfss", "recfin") ~ "Rec",
                                      len_ALL$mod_fleet %in% c("rov") ~ "ROV",
                                      len_ALL$mod_fleet %in% c("CCFRP") ~ "CCFRP")

ggplot(len_ALL %>% dplyr::filter(type == "Nfish"), aes(y = N, x = Year, fill = mod_fleet)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Number of samples") + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = c(0.1, 0.6)) +
  scale_fill_manual("legend", values = c("CCFRP" = "#F8766D", "Com" = "#A3A500", "Rec" = "#00BF7D", "ROV" = "#00B0F6"))
#ggsave(here('data_explore_figs',"ALL_lengths_N.png"), width = 6, height = 4)


#Ages
age_com <- read.csv(here("data", "SampleSize_age.csv")) %>%
  dplyr::select(-c(trawl_Nfish, trawl_Ntrip))
age_growth <- read.csv(here("data", "SampleSize_ageGrowth.csv"))
colnames(age_growth)[1] = "Year"

age_temp <- merge(age_growth, age_com, by = "Year", all = TRUE)
age_ALL <- age_temp %>% tidyr::pivot_longer(cols = -Year,
                                             names_to = c("fleet", "type"),
                                             names_sep = "_",
                                             values_to = "N") %>% dplyr::mutate(mod_fleet = fleet)
age_ALL$mod_fleet <- dplyr::case_when(age_ALL$mod_fleet %in% c("pacfin") ~ "Com",
                                      TRUE ~ "growth")

ggplot(age_ALL %>% dplyr::filter(type == "Nfish"), aes(y = N, x = Year, fill = mod_fleet)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Year") +
  ylab("Number of samples") +
  xlim(1960,2025) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = c(0.1, 0.6)) +
  scale_fill_manual("legend", values = c("Com" = "#A3A500", "growth" = "#E76BF3"))
#ggsave(here('data_explore_figs',"ALL_ages_N.png"), width = 6, height = 4)



#---------------------------------------------------------------------------------------------------------------#

## Plots ----

#---------------------------------------------------------------------------------------------------------------#

dir.create(here("data_explore_figs", "bio_figs"))

##
#Overall recreational lengths
##

ggplot(data %>% dplyr::filter(!source %in% c("pacfin", "trawl", "triennial")),
       aes(y = length_cm, x = Year, color = source)) +
  geom_point() +
  ylab("Length (cm)")
ggsave(here('data_explore_figs',"rec_length_allSources.png"), 
       width = 6, height = 4)


##
#Plot for deb data
##

ggplot(bio_deb, aes(x = Year, y = length_cm, color = area)) +
  geom_point() + 
  facet_wrap(~area)
ggsave(here('data_explore_figs',"deb_length_district.png"), 
       width = 6, height = 4)


##
#Plot for Geibel and Collier overlayed on MRFSS
##

ggplot(bio_gc, aes(x = length_cm, color = source)) +
  geom_density() +
  geom_density(aes(x = length_cm, color = source), 
               data = bio_mrfss %>% dplyr::filter(area %in% "Redwood",
                                                  mode %in% "PR",
                                                  Year %in% c(1992:1998))) +
  ggtitle("Length comparison of PR in Redwood: 1992-1998")
ggsave(here('data_explore_figs',"GeiCol_length_comparison_mrfss_density.png"), 
       width = 6, height = 4)


##
#Compare lengths by sex and source
##

ggplot(data, aes(x = length_cm, fill = sex)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2) +
  ylab("Count") +
  xlab("Length (cm)") +
  xlim(0,65) + 
  facet_wrap(~source, scales = "free") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#Plot these in style of 2021 assessment

source("https://raw.githubusercontent.com/brianlangseth-NOAA/dataModerate_2021/refs/heads/quillback/R/len_freq_plot.R")
source("https://raw.githubusercontent.com/brianlangseth-NOAA/dataModerate_2021/refs/heads/quillback/R/data_hist_plot.R")

#Rename variables so can do length_freq_plot
data$Source <- data$source
data$Sex <- data$sex
data$Length <- data$length_cm

#Source 
#Using length_freq_plot
#Note Im combining the triennial and trawl surveys here
data[which(data$source %in% c("trawl", "triennial")), "Source"] <- "NOAA surveys"
length_freq_plot(dir = here("data_explore_figs", "bio_figs"), 
                 data = data, xlim = NULL, ylim = NULL)
#The length_freq_plot function creates its own plots file so I remove that directory
file.rename(from = here("data_explore_figs", "bio_figs", "plots", "Length_by_Source.png"),
            to = here("data_explore_figs", "bio_figs", "Length_by_Source.png"))
unlink(here("data_explore_figs", "bio_figs", "plots"), recursive=TRUE)

#Source
#Using data_hist
data$test <- "all"
data_hist(dir = here("data_explore_figs", "bio_figs"), 
          data = data, 
          data_type = "Length", 
          group_column = "test", 
          fleet_column = "Source", 
          ymax = NULL, 
          do_abline = TRUE)
#Now without the survey because we dont plan to apply those data as length comps
data$test <- "all_noSurvey"
data_hist(dir = here("data_explore_figs", "bio_figs"), 
          data = data[which(!data$source %in% c("trawl", "triennial")), ], 
          data_type = "Length", 
          group_column = "test", 
          fleet_column = "Source", 
          ymax = NULL, 
          do_abline = TRUE)


##
#Compare lengths of aged and unaged fish
##

png(filename = here("data_explore_figs", "bio_figs", "Compare_Lengths_for_Aged_Unaged_Fish.png"), 
    w = 7, h = 7, units = "in", pointsize = 12, res = 300)
par(mfrow = c(2,2))
for(Sex in c("F", "M")){
  find = which(is.na(data$age) & data$sex == Sex)
  hist(data[find, "length_cm"], xlim = c(0, 65),  xlab = "Length (cm)", 
       col = ifelse(Sex == "F", alpha('red', 0.65), alpha('blue', 0.5)), main = paste0("Unaged Fish Lengths: ", Sex))
  abline(v = median(data[find, "length_cm"], na.rm = TRUE), lty = 2, lwd = 3, col = 1)
  mtext(side = 3, line = -1, adj = 0, paste("N =", length(data[find, "length_cm"])))
}
for(Sex in c("F", "M")){
  find = which(!is.na(data$age) & data$sex == Sex)
  hist(data[find, "length_cm"], , xlim = c(0, 65), xlab = "Length (cm)", 
       col = ifelse(Sex == "F", alpha('red', 0.65), alpha('blue', 0.5)), main = paste0("Aged Fish Lengths: ", Sex))
  abline(v = median(data[find, "length_cm"], na.rm = TRUE), lty = 2, lwd = 3, col = 1)
  mtext(side = 3, line = -1, adj = 0, paste("N =", length(data[find, "length_cm"])))
}
dev.off()



#---------------------------------------------------------------------------------------------------------------#

# Generate composition data for use in SS3 ----

#---------------------------------------------------------------------------------------------------------------#

dir.create(here("data", "forSS3"))
length_bins <- seq(10, 50, 2)


###########################-
## Recreational Length comps ----
###########################-

#Read in data so dont have to process script up to this point

out <- read.csv(here("data", "length_processed_noShare", "CAquillback_ALL_bio.csv"))

rec_out <- out %>% dplyr::filter(!source %in% c("pacfin", "trawl", "triennial", "ROV"))


##
#Basic. Output with both number of samples and number of trips
##

#rec_out$common_name <- "quillback" #needed if save to dir
#rec_out$project <- "recreational" #needed if save to dir
rec_out$trawl_id <- paste0(rec_out$source, rec_out$tripID) #trawl_id needed to calculate input_n for tows (trips) option.  

#Run comps with total samples
lfs_nsamp <-  nwfscSurvey::get_raw_comps(
  data = rec_out, 
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  two_sex_comps = FALSE,
  input_n_method = c("total_samples"),
  month = 7,
  fleet = "rec",
  dir = NULL)

#Now run comps with trips
lfs <-  nwfscSurvey::get_raw_comps(
  data = rec_out, 
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  two_sex_comps = FALSE,
  input_n_method = c("tows"),
  month = 7,
  fleet = "rec",
  dir = NULL)
#add the number of samples from the comps with total samples as sample size
rec_comps <- tibble::add_column(lfs$unsexed, "Nsamp" = lfs_nsamp$unsexed$input_n, .before = "input_n")

#Output final comps in forSS3 folder
write.csv(rec_comps, here("data", "forSS3", paste0("Lcomps_recreational_unsexed_raw_", 
                                                   length_bins[1], "_", tail(length_bins,1), 
                                                   ".csv")), row.names = FALSE)


##
#Fleets as areas. Output with both number of samples and number of trips for fleets as areas
##

#rec_out$common_name <- "quillback" #needed if save to dir
#rec_out$project <- "recreational" #needed if save to dir
rec_out$trawl_id <- paste0(rec_out$source, rec_out$tripID) #trawl_id needed to calculate input_n for tows (trips) option.  

fleets <- list("north" = c("Redwood", "Wine"),
               "south" = c("Bay", "Central", "South"))
rec_comps <- list()

for(s in 1:length(fleets)){
  
  #Run comps with total samples
  lfs_nsamp <-  nwfscSurvey::get_raw_comps(
    data = rec_out %>% dplyr::filter(area %in% fleets[[s]]), 
    comp_bins = length_bins,
    comp_column_name = "length_cm",
    two_sex_comps = FALSE,
    input_n_method = c("total_samples"),
    month = 7,
    fleet = paste0("rec_", names(fleets)[s]),
    dir = NULL)
  
  #Now run comps with trips
  lfs <-  nwfscSurvey::get_raw_comps(
    data = rec_out %>% dplyr::filter(area %in% fleets[[s]]), 
    comp_bins = length_bins,
    comp_column_name = "length_cm",
    two_sex_comps = FALSE,
    input_n_method = c("tows"),
    month = 7,
    fleet = paste0("rec_", names(fleets)[s]),
    dir = NULL)
  #add the number of samples from the comps with total samples as sample size
  rec_comps[[s]] <- tibble::add_column(lfs$unsexed, "Nsamp" = lfs_nsamp$unsexed$input_n, .before = "input_n")

}

#Output final comps in forSS3 folder
write.csv(dplyr::bind_rows(rec_comps), 
          here("data", "forSS3", 
               paste0("Lcomps_recreational_FAA_unsexed_raw_", length_bins[1], "_", tail(length_bins,1), 
                      ".csv")), row.names = FALSE)


###########################-
## Recreational (Growth fleet) Age comps ----
###########################-

#Pull "ca" from age_length_cleanup_by_area.R

ca <- read.csv(here("data-raw", "QLBK_faa_age_length.csv"))
age_ca <- ca %>% dplyr::filter(year < 2025, source != "SMURFS") #remove the 2025 samples and Diana's samples

age_bins <- seq(1,60,1)

#These are a mix of various rec projects and commercial and survey. For these entries
#pull all non-commercial non-expanded comps for use as a "growth" fleet.
#For CCFRP, keep Farallon fish in with nonFarallon fish which as of April 9 are
#now in the CCFRP index, and thus should be included in the CCFRP comps.
#Do as marginals and CAAL, and do for CCFRP fleet separate, and included

age_ca$fleet1 <- dplyr::case_when(age_ca$source %in% c("pacfin") ~ "Com",
                                  TRUE ~ "Growth")
age_ca$fleet2 <- dplyr::case_when(age_ca$source %in% c("CCFRPNotFarallons", "CCFRPFarallons") ~ "CCFRP",
                                  age_ca$source %in% c("pacfin") ~ "Com",
                                  TRUE ~ "other")


#######-
### Marginals ----
#######-

#For all non-commercial. Only use number of fish 

afs_nsamp <-  nwfscSurvey::get_raw_comps(
  data = age_ca %>% dplyr::filter(fleet1 == "Growth"), 
  comp_bins = age_bins,
  comp_column_name = "age",
  two_sex_comps = FALSE,
  input_n_method = c("total_samples"),
  month = 7,
  fleet = "growth",
  dir = NULL)

write.csv(afs_nsamp$unsexed, here("data", "forSS3", paste0("Acomps_noncommercial_all_unsexed_raw_", 
                                                   age_bins[1], "_", tail(age_bins,1), 
                                                   ".csv")), row.names = FALSE)


#For non-commercial split by CCFRP (non-farallons and farallons) and non-CCFRP. 
#Only use number of fish

##First for CCFRP
afs_nsamp_ccfrp <-  nwfscSurvey::get_raw_comps(
  data = age_ca %>% dplyr::filter(fleet2 == "CCFRP"), 
  comp_bins = age_bins,
  comp_column_name = "age",
  two_sex_comps = FALSE,
  input_n_method = c("total_samples"),
  month = 7,
  fleet = "ccfrp",
  dir = NULL)

write.csv(afs_nsamp_ccfrp$unsexed, here("data", "forSS3", paste0("Acomps_noncommercial_ccfrp_unsexed_raw_", 
                                                   age_bins[1], "_", tail(age_bins,1), 
                                                   ".csv")), row.names = FALSE)

##Now for non-CCFRP (which no longer includes Farallon CCFRP fish)
afs_nsamp_nonccfrp <-  nwfscSurvey::get_raw_comps(
  data = age_ca %>% dplyr::filter(fleet2 == "other"), 
  comp_bins = age_bins,
  comp_column_name = "age",
  two_sex_comps = FALSE,
  input_n_method = c("total_samples"),
  month = 7,
  fleet = "growth non-ccfrp",
  dir = NULL)

write.csv(afs_nsamp_nonccfrp$unsexed, 
          here("data", "forSS3", paste0("Acomps_noncommercial_nonccfrp_unsexed_raw_", 
                                        age_bins[1], "_", tail(age_bins,1), 
                                        ".csv")), row.names = FALSE)

#Dont need FAA for the growth fleet so no faa marginal age comps

##
#For STAR panel request 11 - redoing request 7 as marginals
##

afs_nsamp_otherRec <-  nwfscSurvey::get_raw_comps(
  data = age_ca %>% dplyr::filter(source %in% c("Abrams", "Cooperative", "CRFS")), 
  comp_bins = age_bins,
  comp_column_name = "age",
  two_sex_comps = FALSE,
  input_n_method = c("total_samples"),
  month = 7,
  fleet = "rec",
  dir = NULL)

write.csv(afs_nsamp_otherRec$unsexed, 
          here("data", "forSS3", paste0("Acomps_possibleRec_unsexed_raw_", 
                                        age_bins[1], "_", tail(age_bins,1), 
                                        ".csv")), row.names = FALSE)


#######-
### Conditionals ----
#######-

#Since the model is one sex, set to U before doing CAAL
age_ca$sex <- "U"


#For all non-commercial

caal <-  nwfscSurvey::get_raw_caal(
  data = age_ca %>% dplyr::filter(fleet1 == "Growth"), 
  len_bins = length_bins,
  age_bins = age_bins,
  length_column_name = "length_cm",
  age_column_name = "age",
  month = 7,
  fleet = "growthCAAL",
  dir = NULL)

write.csv(caal, here("data", "forSS3", paste0("CAAL_noncommercial_all_unsexed_",
                                              length_bins[1], "_", tail(length_bins,1),
                                              "_", age_bins[1], "_", tail(age_bins,1),
                                              ".csv")), row.names = FALSE)


#For non-commercial split by CCFRP (non-farallons and farallons) and non-CCFRP 

##First for CCFRP
caal_ccfrp <-  nwfscSurvey::get_raw_caal(
  data = age_ca %>% dplyr::filter(fleet2 == "CCFRP"), 
  len_bins = length_bins,
  age_bins = age_bins,
  length_column_name = "length_cm",
  age_column_name = "age",
  month = 7,
  fleet = "ccfrp_CAAL",
  dir = NULL)

write.csv(caal_ccfrp, here("data", "forSS3", paste0("CAAL_noncommercial_ccfrp_unsexed_",
                                              length_bins[1], "_", tail(length_bins,1),
                                              "_", age_bins[1], "_", tail(age_bins,1),
                                              ".csv")), row.names = FALSE)

##Now for non-CCFRP (which no longer includes Farallon CCFRP fish)
caal_nonccfrp <-  nwfscSurvey::get_raw_caal(
  data = age_ca %>% dplyr::filter(fleet2 == "other"), 
  len_bins = length_bins,
  age_bins = age_bins,
  length_column_name = "length_cm",
  age_column_name = "age",
  month = 7,
  fleet = "growthCAAL_noccfrp",
  dir = NULL)

write.csv(caal_nonccfrp, here("data", "forSS3", paste0("CAAL_noncommercial_nonccfrp_unsexed_",
                                                    length_bins[1], "_", tail(length_bins,1),
                                                    "_", age_bins[1], "_", tail(age_bins,1),
                                                    ".csv")), row.names = FALSE)


#Dont need FAA for the growth fleet so no faa marginal age comps


##
#For STAR panel request 7 - splitting out select rec fleet samples
##

caal_otherRec <-  nwfscSurvey::get_raw_caal(
  data = age_ca %>% dplyr::filter(source %in% c("Abrams", "Cooperative", "CRFS")), 
  len_bins = length_bins,
  age_bins = age_bins,
  length_column_name = "length_cm",
  age_column_name = "age",
  month = 7,
  fleet = "rec_CAAL",
  dir = NULL)

write.csv(caal_otherRec, here("data", "forSS3", paste0("CAAL_noncommercial_possibleRec_unsexed_",
                                                    length_bins[1], "_", tail(length_bins,1),
                                                    "_", age_bins[1], "_", tail(age_bins,1),
                                                    ".csv")), row.names = FALSE)

##
#For STAR panel request 11 - redoing request 7 - splitting out select rec fleet samples
##

caal_otherRec <-  nwfscSurvey::get_raw_caal(
  data = age_ca %>% dplyr::filter(source %in% c("Abrams", "Cooperative", "CRFS", "RBG")), 
  len_bins = length_bins,
  age_bins = age_bins,
  length_column_name = "length_cm",
  age_column_name = "age",
  month = 7,
  fleet = "rec_CAAL",
  dir = NULL)

write.csv(caal_otherRec, here("data", "forSS3", paste0("CAAL_noncommercial_possibleRec_withRBG_unsexed_",
                                                       length_bins[1], "_", tail(length_bins,1),
                                                       "_", age_bins[1], "_", tail(age_bins,1),
                                                       ".csv")), row.names = FALSE)


######- 
####  Growth fleet N table -----
######-

#Create table of sample sizes of growth fleet caal sources. Cant above because 
#haven't pulled in the data
dataN_caal <- age_ca %>% dplyr::filter(fleet1 == "Growth") %>%
  dplyr::group_by(source, year) %>% 
  dplyr::summarize(Nfish = length(age)) %>%
  tidyr::pivot_wider(names_from = source,
                     values_from = c(Nfish),
                     names_glue = "{source}_{.value}") %>%
  dplyr::arrange(year) %>%
  data.frame()
dataN_caal[is.na(dataN_caal)] <- 0
#write.csv(dataN_caal, here("data", "SampleSize_ageGrowth.csv"), row.names = FALSE)


###########################-
## Commercial comps ----
###########################-

##
#Load in and setup the pacfin bio data and catch data
##

#Because want expanded comps need to set up via PacFIN.Utililties/pacfintools approach
#as opposed to through the bio data file

# PacFIN Commercial - 1978-2024
load(here("data-raw", "PacFIN.QLBK.bds.17.Mar.2025.RData"))
bio = bds.pacfin %>% dplyr::filter(AGENCY_CODE == "C")

bio$disp <- "dead"
bio[which(bio$PACFIN_CONDITION_CODE == "A"), "disp"] <- "alive"

#Reorganized port group codes from North to South
bio$group_port_NS <-  dplyr::case_when(bio$PACFIN_GROUP_PORT_CODE == "BDA" ~ "4BDA",
                                       bio$PACFIN_GROUP_PORT_CODE == "BGA" ~ "3BGA",
                                       bio$PACFIN_GROUP_PORT_CODE == "CCA" ~ "1CCA",
                                       bio$PACFIN_GROUP_PORT_CODE == "ERA" ~ "2ERA",
                                       bio$PACFIN_GROUP_PORT_CODE == "MNA" ~ "6MNA",
                                       bio$PACFIN_GROUP_PORT_CODE == "MRA" ~ "7MRA",
                                       bio$PACFIN_GROUP_PORT_CODE == "SFA" ~ "5SFA")

#Remove the fish without lengths
bio <- bio[which(!is.na(bio$FISH_LENGTH)),]

#Clean the data to get in a format useful for comps
bio_clean <- pacfintools::cleanPacFIN(Pdata = bio, CLEAN=TRUE, verbose=TRUE)
bio_clean$fleet <- "com_lan" #Limited catch so combine HKL and TWL gears. Needs to make column in catch file
# #Although there are some male and female records, most are U, and other datasets are unsexed. Set to U
bio_clean$SEX <- "U"

#Load in the current weight-at-length estimates by sex
lwests <- read.csv(here("data", "lw_ests.csv"))
ua <- lwests[lwests$sex == "all", "A"]
fa <- ma <- ua
ub = lwests[lwests$sex == "all", "B"]
fb <- mb <- ub

#Read in the catch file to base expansion on 
#Because have lengths in years where pacfin catch does not exist, 
#use full commercial catch time series 
catch.file <- read.csv(here("data", "CAquillback_total_removals.csv")) %>%
  dplyr::select(c("Year", "com_lan"))


#Set up fleets by areas field and catch file 

#There are no unknown areas so dont need further fields other than port group
bio_clean$faa <- dplyr::case_when(bio_clean$PACFIN_GROUP_PORT_CODE %in% c("CCA", "ERA", "BGA") ~ "North", 
                                  bio_clean$PACFIN_GROUP_PORT_CODE %in% c("BDA", "SFA", "MNA", "MRA") ~ "South")

#Read in the catch file to base expansion on
catch.file.faa <- read.csv(here("data", "confidential_noShare", "CAquillback_total_removals_faa.csv")) %>%
  dplyr::select(c("Year", "com_lan_North", "com_lan_South")) %>%
  dplyr::rename("LANDING_YEAR" = Year,
                "North" = com_lan_North, 
                "South" = com_lan_South)


####
### Basic expansions ----
### Output only with choice for input_n 
####

## Length comps

Pdata_exp <- pacfintools::getExpansion_1(Pdata = bio_clean,
                            fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub)
plot(Pdata_exp$Expansion_Factor_1_L)

Pdata_exp <- pacfintools::getExpansion_2(Pdata = Pdata_exp, 
                            Catch = catch.file, 
                            Units = "MT",
                            maxExp = 0.95,
                            stratification.cols = "fleet")
plot(Pdata_exp$Expansion_Factor_2)

# Pdata_exp$Final_Sample_Size <- pacfintools::capValues(Pdata_exp$Expansion_Factor_1_L * Pdata_exp$Expansion_Factor_2, maxVal = 0.80)
# plot(Pdata_exp$Final_Sample_Size)

Lcomps = pacfintools::getComps(Pdata_exp, 
                               Comps = "LEN",
                               weightid = "Final_Sample_Size_L")

pacfintools::writeComps(inComps = Lcomps, 
           fname = file.path(here("data", "forSS3",
                                  paste0("Lcomps_PacFIN_unsexed_expanded_",
                                         length_bins[1], "_", tail(length_bins,1),".csv"))),
           comp_bins = length_bins,
           column_with_input_n = "n_stewart",
           partition = 0, 
           digits = 4,
           verbose = TRUE)


## Marginal age comps

age_bins <- seq(1,60,1)

#Run the Pdata_exp lines from length comps. They are the same for age comps
Acomps = pacfintools::getComps(Pdata_exp, 
                               Comps = "AGE",
                               weightid = "Final_Sample_Size_A")

pacfintools::writeComps(inComps = Acomps, 
                        fname = file.path(here("data", "forSS3",
                                               paste0("Acomps_PacFIN_unsexed_expanded_",
                                                      age_bins[1], "_", tail(age_bins,1),".csv"))),
                        comp_bins = age_bins,
                        column_with_input_n = "n_stewart",
                        partition = 0, 
                        digits = 4,
                        verbose = TRUE)


## Conditional age comps

#CAAL for commercial data shouldnt apply expansions, the conditionals account 
#for the sampling process. As such use nwfscSurvey::get_raw_caal

pacfin_caal <-  nwfscSurvey::get_raw_caal(
  data = bio_clean, 
  len_bins = length_bins,
  age_bins = age_bins,
  length_column_name = "lengthcm", #can use this even though it floors to the nearest cm because doesn't affect binning
  age_column_name = "Age",
  month = 7,
  fleet = "com",
  dir = NULL)

write.csv(pacfin_caal, here("data", "forSS3", paste0("CAAL_PacFIN_unsexed_",
                                              length_bins[1], "_", tail(length_bins,1),
                                              "_", age_bins[1], "_", tail(age_bins,1),
                                              ".csv")), row.names = FALSE)



####
### Fleets as areas expansions ----
### Output only with choice for input_n 
####

## Length comps

Pdata_exp_faa <- pacfintools::getExpansion_1(Pdata = bio_clean %>% 
                                               dplyr::filter(year %in% catch.file.faa$LANDING_YEAR),
                                         fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub)
plot(Pdata_exp_faa$Expansion_Factor_1_L)

Pdata_exp_faa <- pacfintools::getExpansion_2(Pdata = Pdata_exp_faa, 
                                             Catch = catch.file.faa, 
                                             Units = "MT",
                                             maxExp = 0.95,
                                             stratification.cols = "faa")
plot(Pdata_exp_faa$Expansion_Factor_2)

# Pdata_exp_faa$Final_Sample_Size <- pacfintools::capValues(Pdata_exp_faa$Expansion_Factor_1_L * Pdata_exp_faa$Expansion_Factor_2, maxVal = 0.80)
# plot(Pdata_exp_faa$Final_Sample_Size)

Lcomps_faa = pacfintools::getComps(Pdata_exp_faa,
                                   strat = "faa",
                                   Comps = "LEN",
                                   weightid = "Final_Sample_Size_L")

Lcomps_faa$fleet <- Lcomps_faa$faa

pacfintools::writeComps(inComps = Lcomps_faa[,names(Lcomps_faa) != "faa"], 
                        fname = file.path(here("data", "forSS3", 
                                               paste0("Lcomps_PacFIN_FAA_unsexed_expanded_", 
                                                      length_bins[1], "_", tail(length_bins,1),".csv"))),
                        comp_bins = length_bins,
                        column_with_input_n = "n_stewart",
                        partition = 0, 
                        digits = 4,
                        verbose = TRUE)


## Marginal age comps

#Run the Pdata_exp_faa lines from length comps. They are the same for age comps
Acomps_faa = pacfintools::getComps(Pdata_exp_faa, 
                                   strat = "faa",
                                   Comps = "AGE",
                                   weightid = "Final_Sample_Size_A")

Acomps_faa$fleet <- Acomps_faa$faa

pacfintools::writeComps(inComps = Acomps_faa[,names(Acomps_faa) != "faa"], 
                        fname = file.path(here("data", "forSS3", 
                                               paste0("Acomps_PacFIN_FAA_unsexed_expanded_", 
                                                      age_bins[1], "_", tail(age_bins,1),".csv"))),
                        comp_bins = age_bins,
                        column_with_input_n = "n_stewart",
                        partition = 0, 
                        digits = 4,
                        verbose = TRUE)


## Conditional age comps

#CAAL for commercial data shouldnt apply expansions, the conditionals account 
#for the sampling process. As such use nwfscSurvey::get_raw_caal

#There are no ages in the south so assign CAAL to the northern fleet
table(bio_clean$year, bio_clean$faa, is.na(bio_clean$Age))

com_comps_faa <- list()

for(s in unique(bio_clean$faa)){
  
  pacfin_caal_faa <- NA
  
  pacfin_caal_faa <- nwfscSurvey::get_raw_caal(
    data = bio_clean %>% dplyr::filter(faa %in% s), 
    len_bins = length_bins,
    age_bins = age_bins,
    length_column_name = "lengthcm", #can use this even though it floors to the nearest cm because doesn't affect binning
    age_column_name = "Age",
    month = 7,
    fleet = s,
    dir = NULL)
  
  com_comps_faa[[which(unique(bio_clean$faa) == s)]] <- pacfin_caal_faa
}

write.csv(com_comps_faa, here("data", "forSS3", paste0("CAAL_PacFIN_FAA_unsexed_",
                                                     length_bins[1], "_", tail(length_bins,1),
                                                     "_", age_bins[1], "_", tail(age_bins,1),
                                                     ".csv")), row.names = FALSE)



###########################-
## ROV Length comps ----
###########################-

length_bins <- seq(10, 50, by = 2)

#Read in data so dont have to process script up to this point

out <- read.csv(here("data", "length_processed_noShare", "CAquillback_ALL_bio.csv"))

rov_out <- out %>% dplyr::filter(source %in% c("ROV"))


##
#Basic. Output with both number of samples and number of transects
##

#rov_out$common_name <- "quillback" #needed if save to dir
#rov_out$project <- "recreational" #needed if save to dir
rov_out$trawl_id <- rov_out$tripID #trawl_id needed to calculate input_n for tows (trips) option.  

#Run comps with total samples
lfs_nsamp <-  nwfscSurvey::get_raw_comps(
  data = rov_out, 
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  two_sex_comps = FALSE,
  input_n_method = c("total_samples"),
  month = 7,
  fleet = "rov",
  dir = NULL)

#Now run comps with trips
lfs <-  nwfscSurvey::get_raw_comps(
  data = rov_out, 
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  two_sex_comps = FALSE,
  input_n_method = c("tows"),
  month = 7,
  fleet = "rov",
  dir = NULL)
#add the number of samples from the comps with total samples as sample size
rov_comps <- tibble::add_column(lfs$unsexed, "Nsamp" = lfs_nsamp$unsexed$input_n, .before = "input_n")

#Output final comps in forSS3 folder
write.csv(rov_comps, here("data", "forSS3", paste0("Lcomps_rov_unsexed_raw_", 
                                                   length_bins[1], "_", tail(length_bins,1), 
                                                   ".csv")), row.names = FALSE)

##
#Weighted - need designation so pull directly from the ROV length file. Do for number of trips only. 
##

#Weight the ROV comps; 20% inside MPAs and 80% outside MPAs
len_final <- read.csv(here("data", "length_processed_noShare", "CAquillback_rov_bio.csv")) 
len_final <- len_final %>% mutate(site = case_when(Designation == "MPA" ~ "MPA", 
                                        .default = "REF"))
len_final$trawl_id <- len_final$tripID #trawl_id needed to calculate input_n for tows (trips) option.  

#Sample size check
n <- len_final %>%
  dplyr::group_by(Year, site) %>%
  dplyr::summarise(
    drifts = length(unique(tripID)))

lfs_mpa <- nwfscSurvey::get_raw_comps(
  data = len_final[len_final$site == "MPA", ], 
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  two_sex_comps = FALSE,
  input_n_method = c("tows"),
  month = 7,
  fleet = "rov")

lfs_ref <- nwfscSurvey::get_raw_comps(
  data = len_final[len_final$site == "REF", ], 
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  two_sex_comps = FALSE,
  input_n_method = c("tows"),
  month = 7,
  fleet = "rov")

#Now apply the weighting
protect <- 0.2; open <- 1 - protect
ind <- 7:ncol(lfs_mpa$unsexed)
tmp <- lfs_mpa$unsexed[, ind] * protect + lfs_ref$unsexed[, ind] * open

first <- 1:(length(length_bins))

# This is for unsexed composition data only 
lfs <- round(tmp[, first] /  apply(tmp[, first], 1, sum), 4)
samp <- data.frame("input_n" = lfs_ref$unsexed[,6] + lfs_mpa$unsexed[,"input_n"])
out <- cbind(lfs_ref$unsexed[,1:5], samp, lfs)

#Output final weighted comps in forSS3 folder
write.csv(out, here("data", "forSS3", paste0("Lcomps_rov_unsexed_weighted_", 
                                                   length_bins[1], "_", tail(length_bins,1), 
                                                   ".csv")), row.names = FALSE)


##
#Weighted and by ACTUAL year - need designation so pull directly from the ROV length file. Do for number of trips only. 
##

#Weight the ROV comps; 20% inside MPAs and 80% outside MPAs
len_final <- read.csv(here("data", "length_processed_noShare", "CAquillback_rov_bio.csv")) 
len_final <- len_final %>% 
  dplyr::mutate(site = dplyr::case_when(Designation == "MPA" ~ "MPA", .default = "REF")) %>%
  dplyr::mutate(Year = Actual_Year) 
len_final$trawl_id <- len_final$tripID #trawl_id needed to calculate input_n for tows (trips) option.  

#Output sample size for use in report (SampleSize_length compbined across super years)
rovN <- len_final %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(
    rov_Ntrip = length(unique(tripID)),
    rov_Nfish = length(Year)) %>%
  data.frame()
#write.csv(rovN, here("data", "SampleSize_length_ROV.csv"), row.names = FALSE)

#Sample size check
n <- len_final %>%
  dplyr::group_by(Year, site) %>%
  dplyr::summarise(
    drifts = length(unique(tripID)))

lfs_mpa <- nwfscSurvey::get_raw_comps(
  data = len_final[len_final$site == "MPA", ],
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  two_sex_comps = FALSE,
  input_n_method = c("tows"),
  month = 7,
  fleet = "rov")

lfs_ref <- nwfscSurvey::get_raw_comps(
  data = len_final[len_final$site == "REF", ], 
  comp_bins = length_bins,
  comp_column_name = "length_cm",
  two_sex_comps = FALSE,
  input_n_method = c("tows"),
  month = 7,
  fleet = "rov")

#Now apply the weighting. For 2016, which only occurs in mpa samples, remove
#when calculating the average but add back in after done
protect <- 0.2; open <- 1 - protect
ind <- 7:ncol(lfs_mpa$unsexed)
tmp <- lfs_mpa$unsexed[lfs_mpa$unsexed$year != 2016, ind] * protect + lfs_ref$unsexed[, ind] * open
#add back in 2016 data (which is only)
tmp2016 <- lfs_mpa$unsexed[lfs_mpa$unsexed$year == 2016, ind]
tmp <- dplyr::bind_rows(tmp[which(lfs_ref$unsexed$year < 2016),], 
                        tmp2016, 
                        tmp[which(lfs_ref$unsexed$year > 2016),])


first <- 1:(length(length_bins))

# This is for unsexed composition data only 
lfs <- round(tmp[, first] /  apply(tmp[, first], 1, sum), 4)
samp <- data.frame("input_n" = lfs_ref$unsexed[,6] + lfs_mpa$unsexed[lfs_mpa$unsexed$year != 2016, "input_n"])
samp <- c(samp[1:2, ],
          lfs_mpa$unsexed[lfs_mpa$unsexed$year == 2016, "input_n"], #2016 sample size
          samp[3:5, ])
                         
out <- cbind(lfs_mpa$unsexed[,1:5], samp, lfs)

#Output final weighted comps in forSS3 folder
write.csv(out, here("data", "forSS3", paste0("Lcomps_rov_unsexed_weighted_ACTUAL_YEAR_", 
                                             length_bins[1], "_", tail(length_bins,1), 
                                             ".csv")), row.names = FALSE)

#Dont need FAA for ROV fleet so no faa length comps
