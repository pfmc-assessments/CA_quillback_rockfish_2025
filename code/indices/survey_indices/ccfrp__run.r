################################################################################
### CCFRP index
### CA quillback rockfish assessment 2025
### Melissa Monk
################################################################################
rm(list = ls(all = TRUE))
graphics.off()

library(sdmTMB)
library(tmbstan)
library(ggeffects)
library(MuMIn)
library(here)
library(glue)
library(tidyr)
library(dplyr)
#library(rstanarm)
#options(mc.cores = parallel::detectCores())
library(ggplot2)
#library(bayesplot)
library(grid)
library(devtools)
library(ggeffects)
#library(tidybayes)
library(gridExtra)
#library(fitdistrplus)


#species and area identifiers - eventually put in function
pacfinSpecies <- 'QLBK'
speciesName <- "quillback"
#CCFRP has their own species codes
ccfrpSpeciesCode <- "QBK"

#setwd(glue::glue(here(),"/data/survey_indices/ccfrp/"))
dir <- here("data-raw","ccfrp")
setwd(dir)
plot.dir <- here("data_explore_figs","survey_figs")

#load helper functions
all <- list.files(here("code", "indices", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(here("code", "indices", "sdmTMB", all[a])))}
#Assuming 20% of the habitat is in MPAs in both areas
#Also assign the areas to a district to then use the habiat weights
load("Filtered_data_CCFRP.RData")
orginal_dat <- dat


#grid cell summary for mapping
grid_summary <- dat %>%
  group_by(gridCellID) %>%
  summarise(sum_copp = sum(Target),
            avg_lat = mean(startLat),
            avg_long = mean(startLong))

grid_zeroes <- dat %>%
  filter(Target==0) %>%
  group_by(gridCellID) %>%
 summarise(avg_lat = mean(startLat),
avg_long = mean(startLong))

#


#-------------------------------------------------------------------------------
#Ensure columns named appropriately and covariates are factors
covars <- c("year", "month", "siteName", "MPAorREF", "gridCellID")


dat <- dat %>%
  dplyr::select(year, month, name, site, effort, gridCellID, Full.Name, 
                Nearest.Port, monitoringGroup,
                anglers, driftTime, Target) %>%
  mutate(Effort = anglers * (driftTime * 60)) %>% #need cpue > 1 to take log
  rename(siteName = name,
         MPAorREF = site,
         fullName = Full.Name,
         nearestPort = Nearest.Port) %>%
  mutate(logEffort = log(Effort),
         cpue = Target/Effort) %>%
  mutate_at(covars, as.factor) # %>% # make sure covariates are factors
 ## mutate(depth = depth/6,
  #       depth_2 = depth^2)




#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# models to explore
# inside/outside MPAs the same in northern and southern CA 80/20
# region here represents the location of the MPAs - can likely use district
# region weights will be the interpreted habitat by district or finer if needed
#
#-------------------------------------------------------------------------------
average_cpue <- dat %>% group_by(year) %>% summarise(avg_cpue = mean(cpue))

#All data combined
ggplot(average_cpue, aes(x = year, y = avg_cpue, group = 1)) +
  geom_point() +
  geom_line() 
ggsave(file = file.path(dir, "all_cpue.png"), width = 7, height = 7)


#more visualizations
year_site_mpa <- dat %>% 
  group_by(year, siteName, MPAorREF) %>% 
  summarise(avg_cpue = mean(cpue))


#cpue by mpa and site
ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = MPAorREF, group = MPAorREF)) +
  geom_point() +
  geom_path() +
  facet_wrap(~siteName) +
  scale_color_viridis_d()
ggsave(file = file.path(dir, "mpa_site_cpue.png"), width = 7, height = 7)


#same as above but flip the facet
ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = siteName, group = siteName)) +
  geom_point() +
  geom_line() +
  facet_wrap(~MPAorREF) +
  scale_color_viridis_d()
ggsave(file = file.path(dir, "site_mpa_cpue.png"), width = 7, height = 7)

#all 6 trends
ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = MPAorREF, 
                          linetype = year_site_mpa$siteName,
                          group = interaction(siteName:MPAorREF))) +
  geom_line(linewidth = 1.5) +
  scale_color_viridis_d(begin = .1, end = .5)
ggsave(file = file.path(dir, "mpa_site_3way_cpue.png"), width = 7, height = 7)

#percent pos and county by gridcell
dat.nofn <- dat %>% filter(!grepl("FN", gridCellID))

#-------------------------------------------------------------------------------
#Models EXCLUDING the Farallon Islands
#full model with main effect
model.full <- MASS::glm.nb(
  Target ~  year + siteName + MPAorREF + offset(logEffort),
  data = dat.nofn,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
main_effects <- ggpredict(model.full, terms = "year")


#see if the year:region interaction is significant
#not significant in the south
model.full <- MASS::glm.nb(
  Target ~  year*siteName + MPAorREF +  offset(logEffort),
  data = dat.nofn,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
year_region_interxn <- ggpredict(model.full, terms = "year")

#see if the year:MPAorREF
model.full <- MASS::glm.nb(
  Target ~  siteName +  year*MPAorREF + offset(logEffort),
  data = dat.nofn,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
year_mpa_interxn <- ggpredict(model.full, terms = "year")
#MPAorREF:year is NOT significant 
#same trends in each

#see if we can add the gridCellID as a random effect - removing the interaction first
#DOES NOT CONVERGE
#  model.full <- lme4::glmer.nb(
#    Target ~  year + MPAorREF + siteName + (1|gridCellID) + offset(logEffort),
#    data = dat.nofn,
#    na.action = "na.fail")
#  summary(model.full)
#  anova(model.full)


model.full <- MASS::glm.nb(Target ~  year + siteName + MPAorREF + year*siteName + offset(logEffort),
  data = dat.nofn,
  na.action = "na.fail")
#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "year"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
  dplyr::select(-weight)
Model_selection

#----------------------------------------------------------------------------------
#Set up the grid for sdmTMB 
#No MPA/REF interaction with year so need to weight it by the area of habitaet
#MPA and year interaction

grid <- expand.grid(
  year = unique(dat.nofn$year),
  siteName = levels(dat.nofn$siteName)[1])

grid2 <- NULL
for (a in 1:25){
  grid2 <- rbind(grid2, grid[grid$siteName == "Bodega Head", ])
}
for (a in 1:25){
  grid2 <- rbind(grid2, grid[grid$siteName == "South Cape Mendocino", ])
}
for (a in 1:25){
  grid2 <- rbind(grid2, grid[grid$siteName == "Sewarts Point", ])
}
for (a in 1:25){
  grid2 <- rbind(grid2, grid[grid$siteName == "Ten Mile", ])
}


fit.deltalogn <- sdmTMB(
  Target ~  year + siteName + MPAorREF,
  data = dat.nofn,
  offset = dat.nofn$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal(),
  control = sdmTMBcontrol(newton_loops = 1))

do_diagnostics(
  dir = file.path(dir), 
  fit = fit.nb,
  plot_resid = FALSE)

calc_index(
  dir = file.path(dir), 
  fit = fit.nb,
  grid = grid)


#########################################################################################################
# Model selection and data filter tables ---------------------------------------
#Format data filtering table and the model selection table for document
dataFilters <- data.frame(lapply(data_filters, as.character), stringsasFactors = FALSE)
write.csv(dataFilters, 
          file = file.path(dir, "dataFilters.csv"), 
          row.names = FALSE)

#View(Model_selection)
#format table for the document
out <- Model_selection %>%
  dplyr::select(-`(Intercept)`) %>%
  mutate_at(vars("region",  "MPAorREF" ,"year","offset(logEffort)"), as.character) %>%
  mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
#  replace_na(list(region = "Excluded", 
        #          MPAorREF:year = "Excluded",
#                  MPAorREF = "Excluded", 
            #      depth_2 = "Excluded", 
 #                 depth = "Excluded")) %>%
  mutate_at(c("region",  "MPAorREF" ,"year", "offset(logEffort)"), 
            funs(stringr::str_replace(.,"\\+","Included"))) %>%
  rename(`Effort offset` = `offset(logEffort)`, 
         `log-likelihood` = logLik,
         `Depth squared` = depth_2,
         `Interaction` = `MPAorREF:year`) %>%
  rename_with(stringr::str_to_title,-AICc)
#View(out)
write.csv(out, file = file.path(dir, "model_selection.csv"), 
          row.names = FALSE)




