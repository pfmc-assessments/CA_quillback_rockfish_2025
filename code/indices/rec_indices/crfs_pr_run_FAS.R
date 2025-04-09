#########################################################################
### Run the CDFW PR dockside data to get an index of abundance using data filtered with Stephens-MacCall
### Without first filtering by primary species target and after removing Avila and Morro sites
### Split the index for fleets as areas model
### Quillback assessment 2025
### Julia Coates adapted from Melissa Monk
#########################################################################

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
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(bayesplot)
library(grid)
library(tidybayes)
library(gridExtra)
library(fitdistrplus)
#species and area identifiers - eventually put in function
pacfinSpecies <- 'RFQIL'
speciesName <- "quillback"
indexName <-  "crfs_pr_dockside"

covars <- c("month", "wave3", "district", "year")
covars_weighted <- c("month", "wave3", "district", "year", "year:district")

# Load in some helper functions for processing and plotting the data
functions_dir <- here("code", "sdmTMB")

all <- list.files(file.path(functions_dir))
for (a in 1:length(all)) { source(file.path(functions_dir, all[a]))}

# Set working directories
dir <- here("data-raw", "PRData")
setwd(dir)

# load data
load("data_for_GLM.RData")
colnames(cdfwpr4)[8] <- "district"

#Ensure columns named appropriately and covariates are factors

dat <- cdfwpr4 %>%
rename(Effort = anglers) %>%
  mutate(logEffort = log(Effort)) %>%
  mutate_at(covars, as.factor) # make sure covariates are factors

# Split the data North & South

datN <- dat %>% filter(district %in% c(5,6))
datS <- dat %>% filter(district == 4)

#-------------------------------------------------------------------------------#
#  ----Separate Grids for Fleets as Areas----
#-------------------------------------------------------------------------------#

#set the grid
grid <- expand.grid(
  year = unique(dat$year),
  district = levels(dat$district),
  wave3 = levels(dat$wave3)[1])

grid_north <- grid %>% filter(district %in% c(5, 6))
grid_south <- grid %>% filter(district==4)

name <- "FAS_north_delta_lognormal"
dir.create(file.path(dir, name))

fit <- sdmTMB(
  target ~ 0 + year + district + wave3,
  data = datN,
  offset = datN$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal(),
  control = sdmTMBcontrol(newton_loops = 3)
)                                               

sanity(fit)

index <- calc_index(
  dir = file.path(dir, name), 
  fit = fit,
  grid = grid_north)       

do_diagnostics(
  dir = file.path(dir, name), 
  fit = fit, plot_resids = FALSE)

index <- read.csv(here("data-raw", "PRData", "FAS_north_delta_lognormal", "index_forSS.csv"))
index_dir <- here("data-raw", "PRData", "FAS_north_delta_lognormal")

index$lower <- index$obs - index$logse
index$upper <- index$obs + index$logse

indexp <- ggplot(aes(y = obs, x = year), data = index) + 
  geom_point() +
  geom_line(group=1) + theme_bw() +
  xlab("Year") + ylab("Index Value") + theme(axis.text.x=element_text(angle=90))
indexp <- indexp + geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)
ggsave(file = file.path(index_dir, "model_cpue_by_year_FAS_north.png"), width = 7, height = 7)

#----------------------------------------------------------------------------------------------

name <- "FAS_south_delta_lognormal"
dir.create(file.path(dir, name))

fit <- sdmTMB(
  target ~ 0 + year + wave3,
  data = datS,
  offset = datS$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal(),
  control = sdmTMBcontrol(newton_loops = 3)
)                                               

sanity(fit)

index <- calc_index(
  dir = file.path(dir, name), 
  fit = fit,
  grid = grid_south)       

do_diagnostics(
  dir = file.path(dir, name), 
  fit = fit, plot_resids = FALSE)

index <- read.csv(here("data-raw", "PRData", "FAS_north_delta_lognormal", "index_forSS.csv"))
index_dir <- here("data-raw", "PRData", "FAS_north_delta_lognormal")

index$lower <- index$obs - index$logse
index$upper <- index$obs + index$logse

indexp <- ggplot(aes(y = obs, x = year), data = index) + 
  geom_point() +
  geom_line(group=1) + theme_bw() +
  xlab("Year") + ylab("Index Value") + theme(axis.text.x=element_text(angle=90))
indexp <- indexp + geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)
ggsave(file = file.path(index_dir, "model_cpue_by_year_FAS_south.png"), width = 7, height = 7)


