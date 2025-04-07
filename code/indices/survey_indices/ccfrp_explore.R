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


#DO NOT use these lengths carte blanche if your species has a forked tail!
lengths <- catches %>%
  filter(speciesCode == ccfrpSpeciesCode)
lengths <- inner_join(lengths, drifts_trip_area, by = "driftID")

summary(lengths$Fork.Length..mm.)
summary(lengths$Total.Length..mm.)


lengths <- lengths %>%
   mutate(Forklengthcm = Fork.Length..mm./10,
       length_cm = coalesce(lengthcm, Forklengthcm)) %>%
       filter(!is.na(length_cm))
  


summary(lengths$Fork.Length..mm.)
summary(lengths$Total.Length..mm.)

#-------------------------------------------------------------------------------
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


#-------------------------------------------------------------------------------
#look at depth data
summary(dat$startDepthft)
#NAs for 646
summary(dat$endDepthft)
#NAs for 842 sites

#see how much depth changes within a drift when available
ggplot(dat, aes(x = startDepthft , y = endDepthft, color = name)) +
  geom_point(alpha = .5) +
   xlab("Start depth (ft)") + ylab(" End depth (ft)") +
    scale_color_viridis_d()
ggsave(file = file.path(plot.dir, "ccfrp_start_end_depth.png"), width = 7, height = 7)

#where are the depth NA's
aa <- subset(dat, is.na(startDepthft))
summary(as.factor(aa$monitoringGroup))
bb <- subset(dat, is.na(endDepthft))
summary(as.factor(bb$monitoringGroup))
#mostly humboldt that doesn't have depth
#which cells missing any depth info
cc <- dat %>% 
      group_by(gridCellID) %>%
       summarise(SdepthMean = mean(, na.rm = T))
View(cc)
#read in the GIS interpreted depths
#Rebecca Miller took all of the recorded start and end lat/long inforamtion from
# each drift and used the 2m resolution bathymetry layer and the 90m  resolution 
#bathymetry layer to interpret depths
gis.start.depth1 <- readxl::read_excel(here("data-raw", "ccfrp","ccfrp_for_arc_Start_copy.xlsx"))
gis.end.depth1 <- readxl::read_excel(here("data-raw", "ccfrp","ccfrp_for_arc_End_copy.xlsx")) 

#convert the negative depth in meters to positive values in feet
gis.start.depth <- gis.start.depth1 %>%
  dplyr::select(Drift_ID, Depth_2m, Depth90m, Grid_Cell_ID) %>%
  mutate_at(vars(Depth_2m), as.numeric) %>%
  mutate(Depth_2mft = -Depth_2m * 3.281,
         Depth90mft = -Depth90m * 3.281) %>%
  rename(gis.start.2mtoft = Depth_2mft,
         gis.start.90mtoft = Depth90mft) 

#do the same for the end locations
gis.end.depth <- gis.end.depth1 %>%
  dplyr::select(Drift_ID, Depth2m, Depth90m, Grid_Cell_ID) %>%
  mutate(Depth2mft = -Depth2m * 3.281,
         Depth90mft = -Depth90m * 3.281) %>%
  rename(gis.end.2mtoft = Depth2mft,
         gis.end.90mtoft = Depth90mft)  

#join the data to get an idea of what we have to gis depths
gis.depth <- left_join(gis.start.depth, gis.end.depth) %>%
  rename(driftID = Drift_ID) %>%
  filter(Grid_Cell_ID %in% dat$gridCellID)
#how different are the 2m and 90 depths
ggplot(gis.depth, aes(gis.start.2mtoft,gis.start.90mtoft)) + geom_point()
#look at 2m vs 90m - these both lok reasonable and won't change the binning we do for depth
gis.depth.cell <- gis.depth %>%
group_by(Grid_Cell_ID) %>%
summarise(meanSGisDepth = mean(gis.start.2mtoft, na.rm = T), 
          meanEGisDepth = mean(gis.end.2mtoft, na.rm = T))
View(gis.depth.cell)

#merge the mean SGisDepth to the main data
StartGisDepth <- gis.depth.cell %>%
                dplyr::select(Grid_Cell_ID, meanSGisDepth) %>%
                rename(gridCellID = Grid_Cell_ID)

#merge the new depths in
dat <- left_join(dat, StartGisDepth)

#assign depths
#rules - take recorded depth when possible, if not take the average start depth for the cell
dat <- dat %>%
  mutate(depth = ifelse(is.na(startDepthft) & meanSGisDepth> 0, meanSGisDepth, startDepthft))# %>%
  #filter(!is.na(depth))


ggplot(dat, aes(x = depth, y = cpue)) + geom_point()

#-----------------------------------------------------------                        -------------------
#save the data or save it and just open the process_ccfrp R script
save(dat, areas, catches, lengths, cellLocation, drifts, specieslu,
     tagReturns, dat, file = file.path(dir,"ccfrp.RData"))
