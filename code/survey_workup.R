# Workup pulled survey catches and biological data for California quillback
# 
# Pulls R workspace saved from survey_pulls.R
#  catch - list of catches
#  bio - list of available biological data
#
# Created by Brian Langseth

library(here)
library(ggplot2)
library(magrittr)

load(file = file.path(here('data-raw'),"survey_pulls_Aug21.RData"))

# Calculate total research catches and plot
# For CA only numbers for Triennial and Combo
research_catch <- list(NWFSC.Combo = dplyr::select(catch[[which(survey_names=="NWFSC.Combo")]], 
                                                   Year, total_catch_numbers,
                                                   total_catch_wt_kg, cpue_kg_km2, State),
                       Triennial = dplyr::select(catch[[which(survey_names=="Triennial")]], 
                                                 Year, total_catch_numbers,
                                                 total_catch_wt_kg, cpue_kg_km2, State)) %>%
  dplyr::bind_rows(.id = 'Survey') %>%
  dplyr::group_by(Year, State, Survey) %>%
  dplyr::summarise(total_catch_numbers = sum(total_catch_numbers),
                   total_catch_wt_kg = sum(total_catch_wt_kg)) %>%
  dplyr::filter(State == "CA")

ggplot(data = research_catch, aes(x = Year, y = total_catch_numbers, fill = Survey)) +
  geom_bar(stat = "identity") + 
  labs(y = "Total catch (N)")

research_catch <- research_catch %>% dplyr::filter(total_catch_numbers>0)
# This is the extent of CA research catch
# Year State Survey    total_catch_numbers total_catch_wt_kg
# 1  2001 CA    Triennial                   1              0.35
# 2  2007 CA    NWFSC.Co…                  19             20.4 
# 3  2013 CA    NWFSC.Co…                   1              0.08
# 4  2014 CA    NWFSC.Co…                   4              4.1 
# 5  2017 CA    NWFSC.Co…                   2              1.62

#Plot areas were caught (based on CPUE)
nwfscSurvey::PlotMap.fn(dir = here("data_explore_figs"), dat = catch[[1]], main = survey_names[1]) #Tri
nwfscSurvey::PlotMap.fn(dir = here("data_explore_figs"), dat = catch[[2]], main = survey_names[2]) #WCGBTS




# Combine biological data

research_bio <- dplyr::select(bio[[which(survey_names=="NWFSC.Combo")]], 
                              Year, Length_cm, Sex, Age, Depth_m, Latitude_dd, Longitude_dd, State) %>%
  dplyr::filter(State == "CA")

nwfscSurvey::plot_bio_patterns(dir = here("data_explore_figs"), bio = bio[[2]])
