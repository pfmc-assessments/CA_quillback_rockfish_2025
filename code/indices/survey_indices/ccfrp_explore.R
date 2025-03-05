#########################################################################
### CCFRP data import and clean up
### CA quillback rockfish assessment 2025
### Melissa Monk
### 3/3/25
#########################################################################
rm(list = ls(all = TRUE))
graphics.off()

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RODBC)
library(here)


#species and area identifiers - eventually put in function
pacfinSpecies <- 'QLBK'
speciesName <- "quillback"
#CCFRP has their own species codes
ccfrpSpeciesCode <- "QBK"

#setwd(glue::glue(here(),"/data/survey_indices/ccfrp/"))
dir <- here("data-raw","ccfrp")
setwd(dir)
plot.dir <- here("data_explore_figs","survey_figs")
#-------------------------------------------------------------------------------

#Read in data and basic cleanup
#odbcDriver does not like the spaces or hyphens in the database name
#does not want to read in a dynamic file name
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             DBQ=C:/users/melissa.monk/Documents/GitHub/CA_quillback_rockfish_2025/data-raw/ccfrp/CCFRPDbaseFinal.accdb")

#trips
trips <- sqlQuery(channel, "SELECT * FROM [1-Trip Information]")
#drifts
drifts <- sqlQuery(channel, "SELECT * FROM [3-Drift Information]")
#catches
catches <- sqlQuery(channel, "SELECT * FROM [4-Caught Fishes]")
#species 
specieslu <- sqlQuery(channel, "SELECT * FROM [Fish Species]")
#monitoring
areas <- sqlQuery(channel, "SELECT * FROM [Monitoring Areas]")
#returned tags
tagReturns <- sqlQuery(channel, "SELECT * FROM [Returned Tags Data]")
#cell locations
cellLocation <- sqlQuery(channel, "SELECT * FROM [Grid Cell Locations]")
#close the channel
odbcClose(channel)

#columns have spaces - remove all spaces here
trips <- trips %>% rename_all(make.names)
catches <- catches %>% rename_all(make.names)
drifts <- drifts %>% rename_all(make.names)
specieslu <- specieslu %>% rename_all(make.names)
areas <- areas %>% rename_all(make.names)
tagReturns <- tagReturns %>% rename_all(make.names)
cellLocation <- cellLocation %>% rename_all(make.names)

#Fixed two instances of a space after month by hand in access database
#clean up trip table
trips <- trips %>%
  rename(tripID = Trip.ID,
         area = Area,
         site = Site..MPA..REF.,
         year = Year.Automatic,
         month = Month,
         day = Day,
         vessel = Vessel,
         captain = Captain,
         deckhand = Deckhand,
         boatAnglers = X.Volunteer.Anglers,
         comments = Comments)

#clean up drift table
drifts <- drifts %>%
  rename(driftID = Drift.ID, 
         tripID = Trip.ID,
         tripCellID = ID.Cell.per.Trip, 
         gridCellID = Grid.Cell.ID,
         site = Site..MPA..REF., 
         driftTime = Drift.Time..hrs., 
         anglers = Total...Anglers.Fishing,
         startDepthft = Start.Depth..ft., 
         endDepthft = End.Depth..ft., 
         excludeDrift = Excluded.Drift.Comment,
         startLat = ST_LatDD, 
         startLong = ST_LonDD, 
         anglerHours = Total.Angler.Hrs,
         relief = Relief..1.3.)

#clean up catches table
catches <- catches %>%
  rename(fishID = Fish.ID,
         driftID = Drift.ID,
         speciesCode = Species.Code,
         tagID = Tag.ID,
         lengthcm = Length..cm.,
         gear = Gear.Type,
         station = Station..,
         anglerID = Angler.ID,
         sex = Sex,
         retained = Retained,
         recapture = Recapture,
         comments = Comments)

#clean up species look up table
specieslu <- specieslu %>%
  rename(speciesCode = Species.Code,
         commonName = Common.Name)

#clean up cell location
cellLocation <- cellLocation %>%
  rename(gridCellID = Grid.Cell.ID,
         area = Area,
         site = Site..MPA..REF.)

#clean up tag returns
tagReturns <- tagReturns %>%
  rename(tagReturnID = Tag.Return.ID,
         tagID = Tag.ID,
         speciesCode = Species.Code,
         monitoringGroup = Monitoring.Group)

#clean up monitoring areas
areas <- areas %>%
  rename(area = Area.code,
         name = Name,
         mpaArea = Area.of.MPA..km.2.,
         region = Region,
         monitoringGroup = Monitoring.Group)


#Join trip to areas
trips_areas <- left_join(trips, areas)
#join trips_areas to drifts
drifts_trip_area <- left_join(drifts, trips_areas, by = "tripID")

#-------------------------------------------------------------------------------
# Collapse catches to drift level
#each line in the catch table is a single fish
Target_catches <- subset(catches, speciesCode == ccfrpSpeciesCode)
Target_catches <- Target_catches %>%
  group_by(driftID) %>%
  tally()
#Target is the number of fish by drift for the species you're interested in
colnames(Target_catches)[2] <- "Target"  #number of fish

#join drifts and catch info and make NA 0 where target species not observed
dat <- left_join(drifts_trip_area, Target_catches)
dat <- dat %>%
  mutate(
    Target = replace_na(Target, 0),
    area = substring(driftID, 1, 2) #pulls out the area sampled
  ) %>%
  mutate(effort = anglers * driftTime) %>%
  mutate(cpue = Target / effort) %>%
  rename(site = site.x)


#DO NOT use these length cart blanche if your species has a forked tail!
lengths <- catches %>%
  filter(!is.na(lengthcm),
         speciesCode == ccfrpSpeciesCode)
lengths <- inner_join(lengths, drifts_trip_area, by = "driftID")
#-------------------------------------------------------------------------------
#look at depth data
summary(dat$startDepthft)
#NAs for 823 sites
summary(dat$endDepthft)
#NAs for 1995 sites

#where are the depth NA's
aa <- subset(dat, is.na(startDepthft))
summary(as.factor(aa$monitoringGroup))
#mostly humboldt that doesn't have depth
#cell could be included and accounts for depth likely



#how many drifts with target species by MPA
total_effort <- dat %>% group_by(name) %>%
  summarise(total_effort = sum(effort, na.rm=TRUE))

#total of the target species 
total_target <- dat %>% group_by(name) %>%
  summarise(total_target = sum(Target))
totals <- inner_join(total_effort, total_target) %>%
  mutate(total_cpue = total_target/total_effort)

#how often sites (500 m x 500m cell) sampled
sites_sampled <- dat %>%
  group_by(name, year) %>%
  tally() %>%
  pivot_wider(names_from = name, values_from = n)

#-------------------------------------------------------------------------------


ggplot(dat %>% filter(cpue>0, cpue <20), aes(cpue, fill = name)) +
  geom_boxplot() +
    scale_color_viridis_d()
ggsave(file = file.path(plot.dir, "ccfrp_cpue_name.png"), width = 7, height = 7)

#see how much depth changes within a drift when available
ggplot(dat, aes(x = startDepthft , y = endDepthft, color = name)) +
  geom_point(alpha = .5) +
   xlab("Start depth (ft)") + ylab(" End depth (ft)") +
    scale_color_viridis_d()
ggsave(file = file.path(plot.dir, "ccfrp_start_end_depth.png"), width = 7, height = 7)



#keep only samples north of conception
dat <- dat %>% filter(startLat > 35) %>% droplevels()


#areas to remove - only sampled 1-2 years or saw 1-2 quillback
#removed from the data in the process file where it's documented
#remove here for plotting purposes
name.remove = c( "Point Lobos", "Año Nuevo","Piedras Blancas", "Trinidad", 
                  "Point Buchon")


dat <- dat %>% filter(!name %in% name.remove)



#average cpue
cpue_summary <- dat %>% 
group_by(year,name,site) %>%
 mutate(avg_cpue = mean(cpue))
#plot
ggplot(cpue_summary %>% filter(!name %in% name.remove), aes(x = year , y = avg_cpue, color = site)) +
geom_point(size = 1) +
geom_line() +
facet_wrap(~name) +
 xlab("Year") + ylab("Average CPUE") +
scale_color_viridis_d(begin = .5, end = .8)
ggsave(file = file.path(plot.dir, "ccfrp_cpue_site_name.png"), width = 7, height = 7)


ggplot(lengths %>% filter(!name %in% name.remove), aes(lengthcm, fill = site.x)) +
  geom_density(alpha = .5) +
  facet_wrap(~name) +
  xlab("Length cm") + ylab("Density") +
  scale_fill_viridis_d("Site")
ggsave(file = file.path(plot.dir, "ccfrp_lengths_by_mpa.png"), width = 7, height = 7)


avg_start_depth <- dat %>%
filter(cpue > 0) %>%
group_by(gridCellID, name) %>%
summarise(meansdepth = mean(startDepthft, na.rm = T))

avg_end_depth <- dat %>%
group_by(gridCellID, name) %>%
summarise(meansdepth = mean(startDepthft, na.rm = T))


#save the data or save it and just open the process_ccfrp R script
save(dat, areas, catches, lengths, cellLocation, drifts, specieslu,
     tagReturns, dat, file = file.path(dir,"ccfrp.RData"))
