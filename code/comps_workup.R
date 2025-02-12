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
##############################################################################################################

library(here)
library(magrittr)
#devtools::install_github("pfmc-assessments/nwfscDiag")
library(nwfscDiag)
library(ggplot2)


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

#Combine into a single data frame will all desired variables included
data <- create_data_frame(input)
#write.csv(data, here("data","length_processed_noShare", "CAquillback_ALL_bio.csv"), row.names = FALSE)


#---------------------------------------------------------------------------------------------------------------#

# Tables ----

#---------------------------------------------------------------------------------------------------------------#

## Create table of sample sizes and trips
dataN <- data %>% dplyr::filter(source != "pacfin") %>%
  dplyr::group_by(source, Year) %>% 
  dplyr::summarize(Nfish = length(length_cm),
                   Ntrip = length(unique(tripID))) %>%
  tidyr::pivot_wider(names_from = source,
                     values_from = c(Nfish, Ntrip),
                     names_glue = "{source}_{.value}") %>%
  dplyr::arrange(Year) %>%
  data.frame()
dataN[is.na(dataN)] <- 0
#write.csv(dataN, here("data", "SampleSize_length_noPacFIN.csv"), row.names = FALSE)



#---------------------------------------------------------------------------------------------------------------#

# Plots ----

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



