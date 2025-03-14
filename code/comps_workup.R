##############################################################################################################
#
# 	Purpose: Compile length data and generate comps for use in stock synthesis
#            Also useful as a single one stop shop for calculating length and age based biological relationships
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
#     age_length_cleanup.R
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
bio_rov <- data.frame(NA)


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

## Create table of sample sizes and trips for non-commercial sources
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

rec_out <- out %>% dplyr::filter(!source %in% c("pacfin", "trawl", "triennial"))


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
## Recreational Age comps ----
###########################-

#Pull "ca" from age_length_cleanup.R

load(here("data-raw","all_ages_labeled.RData"))
age_ca <- ca

age_bins <- seq(1,60,1)

#These are a mix of various rec projects and commercial and survey. For these entries
#pull all non-commercial non-expanded comps for use as a "growth" fleet.
#Do as marginals and CAAL, and do for CCFRP fleet separate, and included

age_ca$fleet1 <- dplyr::case_when(age_ca$sample_type %in% c("Commercial") ~ "Com",
                                  TRUE ~ "Growth")
age_ca$fleet2 <- dplyr::case_when(grepl("CCFRP", age_ca$sample_type) ~ "CCFRP",
                                  age_ca$sample_type %in% c("Commercial") ~ "Com",
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


#For non-commercial split by CCFRP and non-CCFRP. 
#Only use number of fish

##First for CCFRP
afs_nsamp_ccfrp <-  nwfscSurvey::get_raw_comps(
  data = age_ca %>% dplyr::filter(fleet2 == "CCFRP"), 
  comp_bins = age_bins,
  comp_column_name = "age",
  two_sex_comps = FALSE,
  input_n_method = c("total_samples"),
  month = 7,
  fleet = "growth ccfrp",
  dir = NULL)

write.csv(afs_nsamp_ccfrp$unsexed, here("data", "forSS3", paste0("Acomps_noncommercial_ccfrp_unsexed_raw_", 
                                                   age_bins[1], "_", tail(age_bins,1), 
                                                   ".csv")), row.names = FALSE)

##Now for non-CCFRP
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


#######-
### Conditionals ----
#######-

#Since the model is one sex, set to U before doing CAAL
age_ca$Sex <- "U"


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


#For non-commercial split by CCFRP and non-CCFRP 

##First for CCFRP
caal_ccfrp <-  nwfscSurvey::get_raw_caal(
  data = age_ca %>% dplyr::filter(fleet2 == "CCFRP"), 
  len_bins = length_bins,
  age_bins = age_bins,
  length_column_name = "length_cm",
  age_column_name = "age",
  month = 7,
  fleet = "growthCAAL_ccfrp",
  dir = NULL)

write.csv(caal_ccfrp, here("data", "forSS3", paste0("CAAL_noncommercial_ccfrp_unsexed_",
                                              length_bins[1], "_", tail(length_bins,1),
                                              "_", age_bins[1], "_", tail(age_bins,1),
                                              ".csv")), row.names = FALSE)

##Now for non-CCFRP
caal_nonccfrp <-  nwfscSurvey::get_raw_caal(
  data = age_ca %>% dplyr::filter(fleet2 == "other"), 
  len_bins = length_bins,
  age_bins = age_bins,
  length_column_name = "length_cm",
  age_column_name = "age",
  month = 7,
  fleet = "growthCAAL_ccfrp",
  dir = NULL)

write.csv(caal_nonccfrp, here("data", "forSS3", paste0("CAAL_noncommercial_nonccfrp_unsexed_",
                                                    length_bins[1], "_", tail(length_bins,1),
                                                    "_", age_bins[1], "_", tail(age_bins,1),
                                                    ".csv")), row.names = FALSE)



###########################-
## Commercial comps ----
###########################-

##
#Load in and setup the pacfin bio data and catch data
##

#Because want expanded comps need to set up via PacFIN.Utililties/pacfintools approach
#as opposed to through the bio data file

# PacFIN Commercial - 1978-2024
load(here("data-raw", "PacFIN.QLBK.bds.14.Mar.2025.RData"))
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
ua <- 1.599251e-5
fa <- ma <- ua
ub = 3.076563
fb <- mb <- ub

#Read in the catch file to base expansion on. 
#Because have lengths in years where pacfin catch does not exist, 
#use full commercial catch time seires 
catch.file <- read.csv(here("data", "CAquillback_total_removals.csv")) %>%
  dplyr::select(c("Year", "com_lan"))


#Set up fleets by areas field and catch file 

#There are no unknown areas so dont need further fields other than port group
bio_clean$faa <- dplyr::case_when(bio_clean$PACFIN_GROUP_PORT_CODE %in% c("CCA", "ERA", "BGA") ~ "North", 
                                  bio_clean$PACFIN_GROUP_PORT_CODE %in% c("BDA", "SFA", "MNA", "MRA") ~ "South")

#Note that this only covers years in pacfin catch file. 
#Right now I resolve this by only expanding lengths over this time period 
catch.file.faa <- read.csv(here("data", "confidential_noShare", "CAquillback_pacfin_FAA_landings.csv"))
catch.file.faa[is.na(catch.file.faa)] <- 0 #set NAs to 0


##
# Basic expansions. Output only with choice for input_n
##

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

Pdata_exp$Final_Sample_Size <- pacfintools::capValues(Pdata_exp$Expansion_Factor_1_L * Pdata_exp$Expansion_Factor_2, maxVal = 0.80)
plot(Pdata_exp$Final_Sample_Size)

Lcomps = pacfintools::getComps(Pdata_exp, Comps = "LEN")

pacfintools::writeComps(inComps = Lcomps, 
           fname = file.path(here("data", "forSS3",
                                  paste0("Lcomps_PacFIN_unsexed_expanded_",
                                         length_bins[1], "_", tail(length_bins,1),".csv"))),
           comp_bins = length_bins,
           column_with_input_n = "n_stewart",
           partition = 0, 
           digits = 4,
           verbose = TRUE)


## Age comps


##
# Fleets as areas expansions.  Output only with choice for input_n
##

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

Pdata_exp_faa$Final_Sample_Size <- pacfintools::capValues(Pdata_exp_faa$Expansion_Factor_1_L * Pdata_exp_faa$Expansion_Factor_2, maxVal = 0.80)
plot(Pdata_exp_faa$Final_Sample_Size)

Lcomps_faa = pacfintools::getComps(Pdata_exp_faa, Comps = "LEN")

pacfintools::writeComps(inComps = Lcomps_faa, 
                        fname = file.path(here("data", "forSS3", 
                                               paste0("Lcomps_PacFIN_FAA_unsexed_expanded_", 
                                                      length_bins[1], "_", tail(length_bins,1),".csv"))),
                        comp_bins = length_bins,
                        column_with_input_n = "n_stewart",
                        partition = 0, 
                        digits = 4,
                        verbose = TRUE)

## Age comps
