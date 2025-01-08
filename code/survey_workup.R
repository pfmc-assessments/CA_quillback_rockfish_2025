# Workup pulled catches and biological data from NOAA surveys
# 
# Pulls R workspace saved from survey_pulls.R
#  catch - list of catches indexed by "survey_names"
#  bio - list of available biological data indexed by "survey_names"
#
# Created by Brian Langseth

library(here)
library(ggplot2)
library(magrittr)

load(file = file.path(here('data-raw'),"survey_pulls_Sept4.RData"))

## Calculate total research catches and plot

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

#Plot areas where caught (based on CPUE)
#Commented out because already uploaded
# nwfscSurvey::PlotMap.fn(dir = here("data_explore_figs"), dat = subset(catch[[1]],State=="CA"), 
#                         main = survey_names[1], plot = 1) #Tri
# nwfscSurvey::PlotMap.fn(dir = here("data_explore_figs"), dat = subset(catch[[2]],State=="CA"), 
#                         main = survey_names[2], plot = 1) #WCGBTS



## Combine biological data for only CA (occurs in WCGBTS and 1 in Triennial)

research_bio <- dplyr::select(bio[[which(survey_names=="NWFSC.Combo")]], 
                              Year, Length_cm, Weight_kg, Sex, Age, Depth_m, Latitude_dd, Longitude_dd, Tow, State) %>% 
  dplyr::bind_rows(trawl = ., 
                   triennial = dplyr::select(bio[[which(survey_names == "Triennial")]]$length_data, 
                                             Year, Length_cm, Weight_kg, Sex, Depth_m, Latitude_dd, Longitude_dd, Tow, State),
                   .id = 'survey') %>%
  dplyr::filter(State == "CA")

#Plot biological data by depth and latitude
#Commented out because already uploaded
# nwfscSurvey::plot_bio_patterns(dir = here("data_explore_figs"), bio = research_bio, plot = 1)


##Rename "plots" folder to something more understandable

file.rename(here("data_explore_figs","plots"),here("data_explore_figs", "survey_figs"))


## Output file for biological analysis

research_bio$area = NA
out_surveys <- research_bio %>% dplyr::select(Year, 
                                              "length_cm" = Length_cm,
                                              "weight_kg" = Weight_kg,
                                              "sex" = Sex,
                                              "age" = Age,
                                              "depth_m" = Depth_m,
                                              "lat" = Latitude_dd,
                                              "lon" = Longitude_dd,
                                              "source" = survey,
                                              tripID = Tow)
#write.csv(out_surveys, here("data","CAquillback_wcgbts_triennial_bio.csv"), row.names = FALSE)

