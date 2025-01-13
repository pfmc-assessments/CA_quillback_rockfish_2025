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


#---------------------------------------------------------------------------------------------------------------#

# Pull together all the length data ----

#---------------------------------------------------------------------------------------------------------------#

##
#Load all biological data
##

## PacFIN bds
bio_pacfin <- read.csv(here("data_bio_process","CAquillback_com_bio.csv"), header = TRUE)

## RecFIN bds
bio_recfin <- read.csv(here("data_bio_process","CAquillback_rec_bio.csv"), header = TRUE)

## MRFSS bds
bio_mrfss <- read.csv(here("data_bio_process","CAquillback_mrfss_bio.csv"), header = TRUE)
#Remove 1997-1998 PC data because these are duplicated in Deb's data
bio_mrfss <- bio_mrfss[-which(bio_mrfss$Year %in% c(1997,1998) & bio_mrfss$mode == "PC"),]
table(bio_mrfss$Year, bio_mrfss$mode)

## Deb Wilson Vandenberg bds
bio_deb <- read.csv(here("data_bio_process","CAquillback_deb_bio.csv"), header = TRUE)

## Geibel and Collier bds
bio_gc <- read.csv(here("data_bio_process","CAquillback_historical_bio_skiff.csv"), header = TRUE)

## Miller and G* Historical bds
bio_mg <- read.csv(here("data_bio_process","CAquillback_historical_bio.csv"), header = TRUE)

## Survey bds
bio_survey <- read.csv(here("data_bio_process","CAquillback_wcgbts_triennial_bio.csv"), header = TRUE)

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
                                                   "source")){
  
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
#write.csv(data, here("data_bio_process", "CAquillback_ALL_bio.csv"), row.names = FALSE)




#---------------------------------------------------------------------------------------------------------------#

# Plots ----

#---------------------------------------------------------------------------------------------------------------#

dir.create(here("data_explore_figs", "bio_figs"))


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
