# Pull survey catches and biological data for California quillback
# 
# Outputs R workspace of each surveys info saved into one of two objects
#  catch - list of catches
#  bio - list of available biological data
# Created by Brian Langseth

library(here)
library(ggplot2)
library(magrittr)
#devtools::install_github("pfmc-assessments/nwfscSurvey")

# Pull all surveys, but remove (comment-out) those that have no catches
survey_names <- c("Triennial", #"AFSC.Slope", 
                  "NWFSC.Combo", #"NWFSC.Slope", 
                  "NWFSC.Shelf", #"NWFSC.Hypoxia", #"NWFSC.Santa.Barb.Basin", 
                  "NWFSC.Video", 
                  "Triennial.Canada")

catch <- bio <- list()
for(i in 1:length(survey_names)) {
  
  #Catches
  catch[[i]] <- nwfscSurvey::pull_catch(common_name = 'quillback rockfish',
                                        survey = survey_names[i],
                                        dir = here('data-raw')) %>%
    dplyr::mutate(Date = as.character(Date),
                  State = dplyr::case_when(Latitude_dd < 42 ~ 'CA',
                                           Latitude_dd < 46.25 ~ 'OR',
                                           TRUE ~ 'WA'))

  #Biological data - samples exist for the Tri, WCGBTS and Tri Canada (only 5 of these)
  #Triennial outputs a list so handle differently. Also there are no quillback ages, 
  #so only apply to lengths
  if(survey_names[i] %in% c("Triennial")){
    
    bio[[i]] <- nwfscSurvey::pull_bio(common_name = 'quillback rockfish',
                                      survey = survey_names[i],
                                      dir = here('data-raw')) 
    bio[[i]]$length_data <- bio[[i]]$length_data %>% 
      dplyr::mutate(Date = as.character(Date),
                    State = dplyr::case_when(Latitude_dd < 42 ~ 'CA',
                                             Latitude_dd < 46.25 ~ 'OR',
                                             TRUE ~ 'WA'))
  }
  
  if(survey_names[i] %in% c("NWFSC.Combo", "Triennial.Canada")){
    
    bio[[i]] <- nwfscSurvey::pull_bio(common_name = 'quillback rockfish',
                                      survey = survey_names[i],
                                      dir = here('data-raw')) %>%
      dplyr::mutate(Date = as.character(Date),
                    State = dplyr::case_when(Latitude_dd < 42 ~ 'CA',
                                             Latitude_dd < 46.25 ~ 'OR',
                                             TRUE ~ 'WA'))
  }

  print(paste(survey_names[i], sum(catch[[i]]$total_catch_wt_kg)))
}
# Triennial 72.697
# NWFSC.Combo 231.31
# NWFSC.Shelf 1.89
# NWFSC.Video 2.1
# Triennial.Canada 97.131

#Extent of WCGBTS sampling in CA
#There are 6 total tows
catch[[2]] %>% dplyr::filter(State == "CA", total_catch_numbers>0)

save.image(file = file.path(here('data-raw'),"survey_pulls_Sept4.RData"))



