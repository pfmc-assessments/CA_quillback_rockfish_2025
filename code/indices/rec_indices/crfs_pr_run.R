#########################################################################
### Run the CDFW PR dockside data to get an index of abundance using data filtered with Stephens-MacCall
### Without first filtering by primary species target and after removing Avila and Morro sites
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

#-------------------------------------------------------------------------------#
# ----Model selection----
#full model
model.full <- MASS::glm.nb(
  target ~ year + district + wave3 + month + year:district + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
#use ggpredict to get an estimate of the logEffort for sdmTMB predictions
ggpredict(model.full, terms = "year")
#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "year"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
  dplyr::select(-weight)
Model_selection
colnames(Model_selection)[6] <- "year:district"

#set the grid
grid <- expand.grid(
  year = unique(dat$year),
  district = levels(dat$district)[1],
  wave3 = levels(dat$wave)[1])

#-------------------------------------------------------------------------------#
## ----Negative Binomial----
name <- "glm_negbin"
dir.create(file.path(dir, name), showWarnings = FALSE)
  
fit.nb <- sdmTMB(
  target ~ year + district + wave3 + year:district,
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log"),
  control = sdmTMBcontrol(newton_loops = 1))  

sanity(fit.nb)  

#Get diagnostics and index for SS
do_diagnostics(
  dir = file.path(dir, name), 
  fit = fit.nb)

# It looks like the error I get here has to do with plotting residuals.  Hopefully it isn't a big deal.  

calc_index(
  dir = file.path(dir, name), 
  fit = fit.nb,
  grid = grid)

logLik(fit.nb) # 'log Lik.' -9832.045 (df=57)
AIC(fit.nb) # 19778.09
pscl::pR2(fit.nb)["McFadden"] 
#fitting null model for pseudo-r2
#McFadden 
#0.04224333  
#-------------------------------------------------------------------------------#
## ----delta-lognormal model----
name <- "delta_lognormal"
dir.create(file.path(dir, name))

fit <- sdmTMB(
  target ~ 0 + year + district + wave3,
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal(),
  control = sdmTMBcontrol(newton_loops = 3)
)      

sanity(fit) # Model with interaction does not pass diagnostics.  Removed interaction.  

index <- calc_index(
  dir = file.path(dir, name), 
  fit = fit,
  grid = grid)       

do_diagnostics(
  dir = file.path(dir, name), 
  fit = fit, plot_resids = FALSE)

dev.off()

logLik(fit) # 'log Lik.' -9897.391 (df=45)
AIC(fit) # 19884.78
pscl::pR2(fit)["McFadden"] 

#-------------------------------------------------------------------------------#
## ----tweedie----
name <- "tweedie"
dir.create(file.path(dir, name))

fit <- sdmTMB(
  target ~ 0 + year + district + wave3,
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = tweedie(),
  control = sdmTMBcontrol(newton_loops = 3)
)      

# Error in solve.default(h, g) : 
# system is computationally singular: reciprocal condition number = 7.87907e-18
# sanity(fit)

index <- calc_index(
  dir = file.path(dir, name), 
  fit = fit,
  grid = grid)       

do_diagnostics(
  dir = file.path(dir, name), 
  fit = fit, plot_resids = FALSE)

dev.off()


# ----Format data filtering tables for document----
View(dataFilters)

dataFilters <- dataFilters %>%
rowwise() %>%
filter(!all(is.na(across((everything()))))) %>%
ungroup() %>%
rename(`Positive Samples` = Positive_Samples)
dataFilters <- data.frame(lapply(dataFilters, as.character), stringsasFactors = FALSE)

write.csv(dataFilters, file = file.path(dir, "data_filters.csv"), row.names = FALSE)

#View(Model_selection)
#format table for the document

#colnames(Model_selection)[6] <- "district_year"

out <- Model_selection %>%
  dplyr::select(-`(Intercept)`) %>%
  mutate_at(vars(covars_weighted,"year","offset(logEffort)"), as.character) %>%
  mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
  replace_na(list(district = "Excluded", wave3 = "Excluded", month = "Excluded", `year:district` = "Excluded")) %>%
  mutate_at(c(covars,"year","offset(logEffort)"), 
            funs(stringr::str_replace(.,"\\+","Included"))) %>%
  rename(`Effort offset` = `offset(logEffort)`, 
         `log-likelihood` = logLik) %>%
  rename_with(stringr::str_to_title,-AICc)

write.csv(out, file = file.path(dir,  "model_selection.csv"), row.names = FALSE)

#summary of trips and  percent pos per year
summaries <- cdfwpr4 %>%
  group_by(year) %>%
  summarise(tripsWithTarget = sum(target>0),
            tripsWOTarget = sum(target==0)) %>%
  mutate(totalTrips = tripsWithTarget+tripsWOTarget,
         percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) 
View(summaries)
write.csv(summaries, 
file.path(dir,  "percent_pos.csv"),
row.names=FALSE)

#-------------------------------------------------------------------------------#
#  ----Area-weighted index---- DECIDED NOT TO USE THIS
#-------------------------------------------------------------------------------#

#fraction of rocky habitat by district in state waters only
north_district_weights <- data.frame(district = c(3,4,5,6),
                                  area_weight = c(0.3227, 0.321, 0.162, 0.1943))

#set the grid
grid <- expand.grid(
    year = unique(dat$year),
    district = levels(dat$district),
    wave3 = levels(dat$wave3)[1])

 grid$district_year <- 1

 district3 <- round(0.32 * 100, 0)
 district4 <- round(0.32 * 100, 0)
 district5 <- round(0.16 * 100, 0)
 district6 <- round(0.20 * 100, 0)
 
 grid_north <- NULL
 for (a in 1:32){
   grid_north <- rbind(grid_north, grid[grid$district == 3, ])
 }
 for (a in 1:32){
   grid_north <- rbind(grid_north, grid[grid$district == 4, ])
 }
 for (a in 1:16){
   grid_north <- rbind(grid_north, grid[grid$district == 5, ])
 }
 for (a in 1:20){
   grid_north <- rbind(grid_north, grid[grid$district == 6, ])
 }
 
name <- "area_weighted_delta_lognormal"
dir.create(file.path(dir, name))
  
 fit <- sdmTMB(
   target ~ 0 + year + district + wave3,
   data = dat,
   offset = dat$logEffort,
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
 
#-----------------------------------------------------------------------------------------------------
 
# Create a plot of the index since the sdmTMB plotting function isn't working

index <- read.csv(here("data-raw", "PRData", "delta_lognormal", "index_forSS.csv"))

index_dir <- here("data-raw", "PRData", "delta_lognormal")
 
 index$lower <- index$obs - index$logse
 index$upper <- index$obs + index$logse
 
 indexp <- ggplot(aes(y = obs, x = year), data = index) + 
   geom_point() +
   geom_line(group=1) + theme_bw() +
   xlab("Year") + ylab("Index Value") + theme(axis.text.x=element_text(angle=90))
 indexp <- indexp + geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.1)
 ggsave(file = file.path(index_dir, "model_cpue_by_year.png"), width = 7, height = 7)
 
# Look at proportion of 0s in predicted and actual data
 pred <- predict(fit, newdata = grid, return_tmb_object = TRUE)
 simulations <- simulate(fit, nsim=5, seed=5090, model=NA)    # How are simulations different from predictions?  The simulations have 0s and predictions don't.  
 colSums(simulations==0)[1]/length(simulations[,1])  # Proportion of 0s in one simulation is 0.4127471
 
 y_i<-pred[["pred_tmb_data"]][["y_i"]]  # y_i in the pred output is the original data (d$target) split into presence absence, and abundance columns.
 y_i <- as.data.frame(y_i)
 y_i[is.na(y_i)] <- 0
 colSums(y_i==0)[1]/length(y_i[,1])  # Proportion 0s in the actual data is 0.4107553   so the model is doing a good job.  
 y_i_2 <- y_i[,1] * y_i[,2] # Multiplying presence absence and abundance columns to recreate original data
 
 # Compare simulations, predictions, and original data
 sim <- sample(simulations[,1],1000)
 sim<- as.data.frame(sim)
 sim$simVdata <- "sim"
 colnames(sim) <- c("cpue", "simVdata")
 plot(sim$cpue, ylim=c(0, 20), ylab="Quillback Count")
 datasample <- as.data.frame(sample(y_i_2, 1000))
 datasample$simVdata <- "data"
 colnames(datasample) <- c("cpue", "simVdata")
 
 points(datasample$cpue, ylim=c(0, 20), col="red")
 
 legend("topright", legend=c("simulations", "data"),
        col=c("black", "red"), cex=0.8, lty=1)

 model_data_compare <- rbind(sim, datasample)

 ggplot(model_data_compare, aes(cpue, fill=simVdata, color=simVdata)) + 
   geom_histogram(alpha=0.5, position='identity') 
 
 ggplot(model_data_compare, aes(cpue, fill=simVdata)) + geom_density(alpha=0.2)
 