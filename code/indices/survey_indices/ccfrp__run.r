################################################################################
### CCFRP index
### CA quillback rockfish assessment 2025
### Melissa Monk
################################################################################
rm(list = ls(all = TRUE))
graphics.off()

library(sdmTMB)
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
library(mgcv)


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

#summary of depths for positive quillback
pos <- dat %>% filter(cpue>0)
summary(pos$depth)
ggplot(pos, aes(depth, cpue)) +geom_point()
#bin depth
dat <- dat %>%
 mutate(depth_bin = cut(depth, breaks = c(0, 40, 80, 120, 160, 200)))

#-------------------------------------------------------------------------------
#Ensure columns named appropriately and covariates are factors
covars <- c("year", "month", "siteName", "MPAorREF", "gridCellID", "depth_bin")



dat <- dat %>%
  dplyr::select(year, month, name, site, effort, gridCellID, Full.Name, 
                Nearest.Port, monitoringGroup,
                anglers, driftTime, Target, depth_bin, depth) %>%
  mutate(Effort = anglers * (driftTime * 60)) %>% #need cpue > 1 to take log
  rename(siteName = name,
         MPAorREF = site,
         fullName = Full.Name,
         nearestPort = Nearest.Port) %>%
  mutate(logEffort = log(Effort),
         cpue = Target/Effort) %>%
  mutate_at(covars, as.factor) # %>% # make sure covariates are factors

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
  geom_point(size = 2) +
  geom_path() +
  facet_wrap(~siteName) +
  labs(x = 'Year', y = 'Average CPUE')+
  scale_color_viridis_d(begin = .2, end = .5)
ggsave(file = file.path(dir, "mpa_site_cpue.png"), width = 9, height = 9)


#cpue by depth
ggplot(dat %>% filter(cpue >0), aes(x = depth_bin, y = cpue)) +
  geom_boxplot() +
 # geom_path() +
  labs(x = 'Depth Bin (ft)', y = 'CPUE')+
  scale_color_viridis_d(begin = .2, end = .5)
ggsave(file = file.path(dir, "depth_cpue.png"), width = 9, height = 9)


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
  scale_color_viridis_d(begin = .2, end = .5)
ggsave(file = file.path(dir, "mpa_site_3way_cpue.png"), width = 7, height = 7)

#percent pos and county by gridcell
#use this to create a dataset without the farallons to explore
#dat.nofn <- dat %>% filter(!grepl("FN", gridCellID))

#-------------------------------------------------------------------------------
#Models INCLUDING the Farallon Islands ---- adding the Farallons back in 4/7/25
#Fallons only sampled in 2017, 2018 and 2024 so it's the year/area weighting
#won't work unless we're creative, or use a 3 year index.
#The final index without the Farallons is fairly flat

area_summary <- dat %>% filter(MPAorREF=="MPA") %>% group_by(siteName) %>% 
summarise(ngrids = n_distinct(gridCellID))
area_summary
#How many hectares within each MPA does CCFRP sample
#Bodega 325
#Farallons 300
#Cape Mendo 400
#Stewarts 225
#Ten Mile 200

#full model with main effect
model.full <- gam(
  Target ~  year + siteName + MPAorREF + depth +s(depth),
  offset = log(dat$Effort),
  family = nb(),
  data = dat)
summary(model.full)
anova(model.full)
main_effects <- ggpredict(model.full, terms = "year")


#see if the year:region interaction is significant
model.full <- gam(
  Target ~  year*MPAorREF + siteName + s(depth) ,
  offset = log(dat$Effort),
  family = nb(),
  data = dat)
summary(model.full)
anova(model.full)


#see if we can add the gridCellID as a random effect - removing the interaction first
#DOES NOT CONVERGE
  model.full <- gam( Target ~  year + MPAorREF + siteName + year*MPAorREF + s(depth) + offset(logEffort),
          #      offset = log(dat$Effort),
                family = nb(),
                data = dat)
#  summary(model.full)
#  anova(model.full)

model2 <- gam( Target ~  year + MPAorREF + siteName + year*MPAorREF + s(depth, by = siteName),
                offset = log(dat$Effort),
                family = nb(),
                data = dat)
#MuMIn will fit all models and then rank them by AICc
options(na.action=na.fail)
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("year", "offset(logEffort)"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
  dplyr::select(-weight)
View(Model_selection)




#----------------------------------------------------------------------------------
#Set up the grid for sdmTMB 
#Weight by inside/outside 20/80
#MPA and year interaction

grid <- expand.grid(
  year = unique(dat$year),
  MPAorREF = levels(dat$MPAorREF)[1],
  siteName = levels(dat$siteName)[1],
  depth = mean(dat$depth))

grid2 <- NULL
for (a in 1:20){
  grid2 <- rbind(grid2, grid[grid$MPAorREF == "MPA", ])
}
for (a in 1:80){
  grid2 <- rbind(grid2, grid[grid$MPAorREF == "REF", ])
}



fit.mod <- sdmTMB(
  Target ~  year + MPAorREF + siteName + year*MPAorREF + s(depth),
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(),
  control = sdmTMBcontrol(newton_loops = 1))

#diagnostics
diag.fit.mod <- simulate(fit.mod, nsim = 500, type = 'mle-mvn') 
#check number of zeros
sum(dat$Target == 0) / length(dat$Target)
sum(diag.fit.mod == 0)/length(diag.fit.mod)
#check residuals
simulata_residuals(diag.fit.mod, fit.mod)


#sanity check
do_diagnostics(
  dir = file.path(dir), 
  fit = fit.mod,
  plot_resid = FALSE)

#calculate the index
index <- calc_index(
  dir = file.path(dir), 
  fit = fit.mod,
  grid = grid)

#format the index
  format_index(index = index,
    dir = file.path(dir),
    month = 7,
    fleet = 4
  )



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
  mutate_at(vars("siteName",  "MPAorREF" ,"year","offset(logEffort)"), as.character) %>%
  mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
  mutate_at(c("siteName",  "MPAorREF" ,"year", "offset(logEffort)"), 
            funs(stringr::str_replace(.,"\\+","Included"))) %>%
  rename(`Effort offset` = `offset(logEffort)`, 
         `log-likelihood` = logLik,
         `Region` = siteName,
         `Depth` = `s(depth)`,
         `Interaction` = `MPAorREF:year`)# %>%
 # rename_with(stringr::str_to_title,-AICc)
#View(out)
write.csv(out, file = file.path(dir, "model_selection.csv"), 
          row.names = FALSE)
