#############################################################################
## DebVW index of abundance for the 2025 CA quillback assessment
## Model selection and index
## Melissa Monk
## 1/28/2025
#############################################################################

rm(list = ls(all = TRUE))
graphics.off()
options(knitr.table.format = "latex")
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(devtools)
library(ggeffects)
#library(tidybayes)
library(gridExtra)
library(fitdistrplus)
library(MuMIn)
library(here)
library(glue)
#species and area identifiers - eventually put in function
pacfinSpecies <- 'QLBK'
speciesName <- "quillback"
modelArea = "California"
indexName <-  "debwv_cpfv_onboard"


# Set working directories
dir <- file.path(here(), "data-raw", "rec_indices", "debwv_cpfv_onboard")
setwd(dir)
out.dir <- file.path(here(),"data_explore_figs", "rec_indices","dwv_cpfv_onboard")


#load data
load("QLBK_filtered_data.RData")
#load(file.path(here(),"data","rec_indices",indexName, 'COPP_filtered_data.RData'))
#r_code_location <- "C:/Users/melissa.monk/Documents/Github/copper_rockfish_2023/R"
#-------------------------------------------------------------------------------
covars <- c("year", "depth" ,"reef")
#rename effort and catch columns
  dat <- dat %>%
    rename(Effort = ANGHRS) %>%
    rename(Target = NUMENC,   #MAKE SURE YOU CHANGE IF YOU WANT KEPT ONLY
           year = YEAR,
           wave = WAVE,
           depth = DEPTH_bin,
           cpue = CPUE) %>%
    mutate(reef = MegaReef) %>%
    mutate(logEffort = log(Effort)) %>%
  mutate_at(covars, as.factor)


#-------------------------------------------------------------------------------
# What's the percent positive in the raw data
Percent_pos <- as.data.frame(round(with(subset(dat, Target > 0), table(year)) /
  with(dat, table(year)), 2))
Percent_pos

with(subset(dat, Target > 0), table(year, reef)) / with(dat, table(year, reef))

#sample size of total and positive by year
with(subset(dat, Target > 0), table(year, reef))
with(dat, table(year, reef))
with(dat, table(year))
with(subset(dat, Target > 0), table(year))

#CPUE plot by reef 
ggplot(dat %>% group_by(reef, year) %>% summarise(average_cpue = mean(cpue)), 
       aes(x = year, y = average_cpue, colour = reef, group = reef)) +
  geom_line()+
  geom_point(size = 3)  + theme_bw() +
  geom_line(aes(x = year, y = average_cpue, 
                colour = reef)) +
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, .2)) + 
  scale_color_viridis_d()
ggsave(file = file.path(out.dir,"average_cpue_by_reef.png"), width = 7, height = 7)

#cpue by depth
ggplot(dat, aes(x = cpue, y = depth)) +
  geom_point(alpha = .5)

summary(dat$wave)
summary(dat$year)
summary(dat$reef)
summary(dat$depth)



#Model selection
#full model - not using wave
model.full <- MASS::glm.nb(
  Target ~ year + depth + reef  + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)

aa <- ggeffects::ggpredict(model.full, terms = "year", back.transform = TRUE)

#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "year"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
  dplyr::select(-weight)
Model_selection


#-------------------------------------------------------------------------------
#Also run as sdmtmb - looks very different - yikes!
  library(sdmTMB)
  library(tmbstan)
 #library(sdmTMBextra)

  grid <- expand.grid(
    year = unique(dat$year),
    reef = levels(dat$reef)[1],
    depth = dat$depth[1]
  )
  
  fit.nb <- sdmTMB(
    Target ~ year  + depth,
    data = dat,
    offset = dat$logEffort,
    time = "year",
    spatial="off",
    spatiotemporal = "off",
    family = nbinom2(link = "log"),
    control = sdmTMBcontrol(newton_loops = 1)) #not entirely sure what this does
  
#----------------------------------------------------------------
  # Load in some helper functions for processing and plotting the data

all <- list.files(file.path(here(), "code", "sdmTMB"))
  
  for (a in 1:length(all)) { source(file.path(here(), "code", "sdmTMB", all[a]))}
  
  #Get diagnostics and index for SS
  do_diagnostics(
    dir = file.path(getwd()), 
    fit = fit.nb)
  
  index.nb <- calc_index(
    dir = file.path(getwd()), 
    fit = fit.nb,
    grid = grid)

plot_indices(
  dir = file.path(getwd()),
  data = index.nb)



  #-------------------------------------------------------------------------------
  #Format data filtering table and the model selection table for document
  View(dataFilters)
  dataFilters <- data_filters %>%
    rowwise() %>%
    filter(!all(is.na(across((everything()))))) %>%
    ungroup() %>%
    rename(`Positive Samples` = Positive_Samples) %>%
    as.data.frame()
  dataFilters <- data.frame(lapply(dataFilters, as.character), stringsasFactors = FALSE)
  
  write.csv(dataFilters, 
            file = file.path(getwd(), "data_filters.csv"), 
            row.names = FALSE)
  
  View(Model_selection)
  #format table for the document
  out <- Model_selection %>%
    dplyr::select(-`(Intercept)`) %>%
    mutate_at(vars(covars,"year","offset(logEffort)"), as.character) %>%
    mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
    mutate_at(c(covars,"year","offset(logEffort)"), 
              funs(stringr::str_replace(.,"\\+","Included"))) %>%
    rename(`Effort offset` = `offset(logEffort)`, 
           `log-likelihood` = logLik) %>%
    rename_with(stringr::str_to_title,-AICc)
 # View(out)
  write.csv(out, file = file.path(getwd(), "model_selection.csv"), row.names = FALSE)
  
  #summary of trips and  percent pos per year
  summaries <- dat %>%
    group_by(year) %>%
    summarise(tripsWithTarget = sum(Target>0),
              tripsWOTarget = sum(Target==0)) %>%
    mutate(totalTrips = tripsWithTarget+tripsWOTarget,
           percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) 
  #View(summaries)
  write.csv(summaries, 
            file.path(getwd(),"percent_pos.csv"),
            row.names=FALSE)
  
 
  
#Delta_models ------------------------------------------------------------------- 

  

  
#   fit <- sdmTMB(
#     Target ~ year  + reef + poly(depth, 2),
#     data = dat,
#     offset = dat$logEffort,
#     time = "year",
#     spatial="off",
#     spatiotemporal = "off",
#     family = delta_lognormal(),
#     control = sdmTMBcontrol(newton_loops = 1)
#   )
  
#   index <- calc_index(
#     dir = file.path(getwd(), "deltalogn"), 
#     fit = fit,
#     grid = grid)
  
#   do_diagnostics(
#     dir = file.path(getwd(),"deltalogn"), 
#     fit = fit)
  
#   index$model <- modelName
#   indices <- rbind(indices, index)
#   loglike <- logLik(fit)
#   aic <- AIC(fit)
#   metrics <- rbind(metrics, c(name, loglike, aic))
  
#   #save(indices, file = file.path(dir, "all_indices.rdata"))  
#   #save(metrics, file = file.path(dir, "metrics.rdata"))
  
# #Delta gamma


  
#   fit <- sdmTMB(
#     Target ~ year  + reef + poly(depth, 2),
#     data = dat,
#     offset = dat$logEffort,
#     time = "year",
#     spatial="off",
#     spatiotemporal = "off",
#     family = delta_gamma(),
#     control = sdmTMBcontrol(newton_loops = 1)
#   )
  
#   index <- calc_index(
#     dir = file.path(getwd(), "deltagamma"), 
#     fit = fit,
#     grid = grid)
  
#   do_diagnostics(
#     dir = file.path(getwd(),"deltagamma"), 
#     fit = fit)
  