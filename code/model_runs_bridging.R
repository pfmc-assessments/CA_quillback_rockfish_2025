##########################################################################################-
#
# Bridging model runs for 2025 California Quillback Rockfish
# These are similar to those in model runs but a bit more detailed
# and structured
#   By: Brian Langseth, Melissa Monk, Julia Coates
#
##########################################################################################-

#Load packages
library(r4ss)
library(PEPtools)
library(here)
library(dplyr)
library(tictoc)
library(nwfscSurvey)
source(here('code/selexComp.R'))


##-------------------------------------------------------------------##
#------------------Initial Setup-------------------------------------
##-------------------------------------------------------------------##

####------------------------------------------------#
## 0_0_1_2025rename  ----
####------------------------------------------------#

# Change file names and comments to reflect a 2025 model
# Model inputs are changed later

dir.create(here("models", "_bridging_runs"))

new_name <- "0_0_1_2025rename"
old_name <- "0_0_1_2021base"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

## Starter file
#Change name for files
mod$start$datfile <- "2025_ca_quillback.dat"
mod$start$ctlfile <- "2025_ca_quillback.ctl"


## Data file
#Change comments in data file
mod$dat$Comments[1] <- "#California Quillback Rockfish 2025 assessment Brian Langseth, Melissa Monk, Julia Coates"


## Forecast file
#Reset benchmark years which were previously hard code. -999 is first model year, 0 is last 
mod$fore$Bmark_years <- c(-999,0, 0,0, 0,0, -999,0, -999,0) #start year and end year for all but selectivity (because of blocks) and relF
mod$fore$Fcast_years <- c(0,0, -3,0, -999,0) #last year for selex, last three years for relF, full time series for average recruitment (though using fcast_rec_option = 0 ignores this)


## And now change forecast set up to reflect new forecast formatting and avoid warning

# Type 10 is selectivity, currently set to equal the last year
# Type 11 is relF, currently set to be the last three years
# Type 12 is recruitment, currently set to be all years
mod$fore$Fcast_selex <- -12345
mod$fore$Fcast_years <- data.frame("MG_type" = c(10, 11, 12),
                                   "method" = c(1, 1, 1),
                                   "st_year" = c(0, -3, -999),
                                   "end_year" = c(0, 0, 0),
                                   row.names = c("#_Fcast_years1",
                                                 "#_Fcast_years2",
                                                 "#_Fcast_years3"))


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

#Because renamed files in starter, new file names are applied. Delete old ones
file.remove(here('models', "_bridging_runs", new_name, 
                 c("2021_ca_quillback.dat", 
                   "2021_ca_quillback.ctl")))

r4ss::run(dir = here('models', "_bridging_runs", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SS_plots(pp, plot = c(1:26))


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = c("//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/2_base_model/CA/10_0_0_postNov_base",
                             here("models", "_bridging_runs", new_name)))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('2021 base',
                                     '2025 exe'),
                    subplots = c(1,3), print = TRUE, plotdir = here('models', "_bridging_runs", new_name))

dev.off()


##-------------------------------------------------------------------##
#------------------Add biology---------------------------------------
##-------------------------------------------------------------------##

####------------------------------------------------#
## 0_1_1_updateM  ----
####------------------------------------------------#

# Update natural mortality prior

new_name <- "0_1_1_updateM"
old_name <- "0_0_1_2025rename"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

maxAge <- 84 #this is the maximum age among BC entries in Claires dataset
m_init <- round(5.4/maxAge, 4)
m_se <- 0.31
m_prior <- 3 #log-normal

mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.15, m_init, round(log(m_init), 2), m_se, m_prior, -2)


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)


####------------------------------------------------#
## 0_1_2_updateGrowth  ----
####------------------------------------------------#

# Update growth based on external estimate

new_name <- "0_1_2_updateGrowth"
old_name <- "0_0_1_2025rename"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

mod$ctl$Growth_Age_for_L1 <- 1

#Growth curve from all individuals. Follows from quillback_growth.R
vb_ests <- read.csv(here("data", "vonb_ests_withAge0.csv"))

vb_k <- vb_ests[vb_ests$X == "K", "ests"]
vb_linf <- vb_ests[vb_ests$X == "Linf", "ests"]
vb_l0 <- mean(c(6.548213, 7.760798)) #mean values from length at ages 0.4 and 0.6 from preds1 in quillback_growth.R
vb_cv0 <- vb_ests[vb_ests$X == "CV0", "ests"]
vb_cv1 <- vb_ests[vb_ests$X == "CV1", "ests"]
vb_prior <- 0

mod$ctl$MG_parms['L_at_Amin_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0, 20, vb_l0, vb_l0, 0, vb_prior, -3)
mod$ctl$MG_parms['L_at_Amax_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(35, 50, vb_linf, vb_linf, 0, vb_prior, -3)
mod$ctl$MG_parms['VonBert_K_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.03, 0.3, vb_k, vb_k, 0, vb_prior, -3)
mod$ctl$MG_parms['CV_young_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.5, vb_cv0, vb_cv0, 0, vb_prior, -3)
mod$ctl$MG_parms['CV_old_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.001, 0.5, vb_cv1, vb_cv1, 0, vb_prior, -3)


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)


####------------------------------------------------#
## 0_1_3_updateLW  ----
####------------------------------------------------#

# Update length-weight relationship

new_name <- "0_1_3_updateLW"
old_name <- "0_0_1_2025rename"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

lw_ests <- read.csv(here("data", "lw_ests.csv"))
lw_a <- lw_ests[lw_ests$sex == "all", "A"]
lw_b <- lw_ests[lw_ests$sex == "all", "B"]
lw_prior <- 0

mod$ctl$MG_parms['Wtlen_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0, 0.1, lw_a, lw_a, 0, lw_prior, -9)
mod$ctl$MG_parms['Wtlen_2_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(2, 4, lw_b, lw_b, 0, lw_prior, -9)


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)


####------------------------------------------------#
## 0_1_4_updateMaturity  ----
####------------------------------------------------#

# Update biological relationships

new_name <- "0_1_4_updateMaturity"
old_name <- "0_0_1_2025rename"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

l50_fxn <- 28.96 #-a/b
l50_se <- 0.599 #se of -a/b
slope_fxn <- -0.606 #b (SS3 manual says this must be negative)
slope_se <- 0.121 #se of b
mat_prior <- 6 #making it normal

mod$ctl$MG_parms['Mat50%_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(25, 32, l50_fxn, l50_fxn, l50_se, mat_prior, -9)
mod$ctl$MG_parms['Mat_slope_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(-1.0, 0, slope_fxn, slope_fxn, slope_se, mat_prior, -9)


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)


####------------------------------------------------#
## 0_1_5_updateFecundity  ----
####------------------------------------------------#

# Update biological relationships

new_name <- "0_1_5_updateFecundity"
old_name <- "0_0_1_2025rename"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

eggs_b <- 4.440
eggs_a <- 4.216e-08
fec_prior <- 0

mod$ctl$MG_parms['Eggs_alpha_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(-3, 3, eggs_a, eggs_a, 0, fec_prior, -9)
mod$ctl$MG_parms['Eggs_beta_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(1, 7, eggs_b, eggs_b, 0, fec_prior, -9)


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)


####------------------------------------------------#
## 0_1_0_updateAllBio  ----
####------------------------------------------------#

# Update ALL biological relationships

new_name <- "0_1_0_updateAllBio"
old_name <- "0_0_1_2025rename"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##


### Update M prior

maxAge <- 84 #this is the maximum age among BC entries in Claires dataset
m_init <- round(5.4/maxAge, 4)
m_se <- 0.31
m_prior <- 3 #log-normal

mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.15, m_init, round(log(m_init), 2), m_se, m_prior, -2)


### Update growth prior

mod$ctl$Growth_Age_for_L1 <- 0

#Growth curve from all individuals. Follows from quillback_growth.R
vb_ests <- read.csv(here("data", "vonb_ests_withAge0.csv"))
vb_k <- vb_ests[vb_ests$X == "K", "ests"]
vb_linf <- vb_ests[vb_ests$X == "Linf", "ests"]
vb_l0 <- mean(c(6.548213, 7.760798)) #mean values from length at ages 0.4 and 0.6 from preds1 in quillback_growth.R
vb_cv0 <- vb_ests[vb_ests$X == "CV0", "ests"]
vb_cv1 <- vb_ests[vb_ests$X == "CV1", "ests"]
vb_prior <- 0

mod$ctl$MG_parms['L_at_Amin_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0, 20, vb_l0, vb_l0, 0, vb_prior, -3)
mod$ctl$MG_parms['L_at_Amax_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(35, 50, vb_linf, vb_linf, 0, vb_prior, -3)
mod$ctl$MG_parms['VonBert_K_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.03, 0.3, vb_k, vb_k, 0, vb_prior, -3)
mod$ctl$MG_parms['CV_young_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.5, vb_cv0, vb_cv0, 0, vb_prior, -3)
mod$ctl$MG_parms['CV_old_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.001, 0.5, vb_cv1, vb_cv1, 0, vb_prior, -3)


### Update LW relationship

lw_ests <- read.csv(here("data", "lw_ests.csv"))
lw_a <- lw_ests[lw_ests$sex == "all", "A"]
lw_b <- lw_ests[lw_ests$sex == "all", "B"]
lw_prior <- 0

mod$ctl$MG_parms['Wtlen_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0, 0.1, lw_a, lw_a, 0, lw_prior, -9)
mod$ctl$MG_parms['Wtlen_2_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(2, 4, lw_b, lw_b, 0, lw_prior, -9)


### Update maturity

l50_fxn <- 28.96 #-a/b
l50_se <- 0.599 #se of -a/b
slope_fxn <- -0.606 #b (SS3 manual says this must be negative)
slope_se <- 0.121 #se of b
mat_prior <- 6 #making it normal

mod$ctl$MG_parms['Mat50%_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(25, 32, l50_fxn, l50_fxn, l50_se, mat_prior, -9)
mod$ctl$MG_parms['Mat_slope_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(-1.0, 0, slope_fxn, slope_fxn, slope_se, mat_prior, -9)


### Update fecundity

eggs_b <- 4.440
eggs_a <- 4.216e-08
fec_prior <- 0

mod$ctl$MG_parms['Eggs_alpha_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(-3, 3, eggs_a, eggs_a, 0, fec_prior, -9)
mod$ctl$MG_parms['Eggs_beta_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(1, 7, eggs_b, eggs_b, 0, fec_prior, -9)


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SS_plots(pp, plot = c(1:26))


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_0_1_2025rename",
                                                 "0_1_1_updateM",
                                                 "0_1_2_updateGrowth",
                                                 "0_1_3_updateLW",
                                                 "0_1_4_updateMaturity",
                                                 "0_1_5_updateFecundity",
                                                 "0_1_0_updateAllBio")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('2025 exe',
                                     'Natural Mortality',
                                     'Growth',
                                     'Length weight',
                                     'Maturity',
                                     'Fecundity',
                                     'All relationships'),
                    subplots = c(1,3), print = TRUE, plotdir = here('models', "_bridging_runs", new_name))

dev.off()



##-------------------------------------------------------------------##
#--------------------------Add Data----------------------------------
##-------------------------------------------------------------------##


####------------------------------------------------#
## 0_2_1_updateCatch ----
####------------------------------------------------#

# Update model inputs for catch only

new_name <- "0_2_1_updateCatch"
old_name <- "0_1_0_updateAllBio"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

mod$dat$endyr <- 2024

#Set up fleet converter to set up any com to fleet=1, and rec to fleet=2

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec")) %>%
  dplyr::mutate(fleet_num = c("1", "2")) %>%
  dplyr::select(fleetname, fleet, fleet_num)

#Update catch time series

catches <- read.csv(here("data", "CAquillback_total_removals.csv"))
catches[is.na(catches)] <- 0

updated.catch.df <- catches %>%
  dplyr::select(c(Year, com_tot, rec_tot)) %>%
  tidyr::pivot_longer(cols = -Year, names_to = 'fleet', values_to = 'catch', 
                      names_pattern = '(.*)_') %>% #keep everything before _
  dplyr::left_join(fleet.converter) %>%
  dplyr::mutate(seas = 1, 
                catch_se = 0.05) %>%
  dplyr::select(year = Year, seas, fleet = fleet_num, catch, catch_se) %>%
  dplyr::arrange(fleet, year) %>%
  as.data.frame()

mod$dat$catch <- updated.catch.df


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SSplotData(pp, print = TRUE, subplots = 1) 
SS_plots(pp, plot = c(1:26))


####------------------------------------------------#
## 0_2_2_update_Lcomps_oldFleets ----
####------------------------------------------------#

# Update model data for catch and length comp data (does not include comps for indices) but
# only do so with existing fleet structure (dont add growth fleet comps)

new_name <- "0_2_2_update_Lcomps_oldFleets"
old_name <- "0_2_1_updateCatch"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

#Set up fleet converter to set up any com to fleet=1, rec to fleet=2

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec")) %>%
  dplyr::mutate(fleet_num = c(1, 2)) %>%
  dplyr::select(fleetname, fleet, fleet_num)


## Update comps

# Length comps

mod$dat$use_lencomp <- 1 #already 1 but useful to set
mod$dat$lbin_vector <- seq(10, 50, by = 2)
mod$dat$N_lbins <- length(mod$dat$lbin_vector)

com.lengths <- read.csv(here("data", "forSS3", "Lcomps_PacFIN_unsexed_expanded_10_50.csv")) %>%
  dplyr::mutate(fleet = "com") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

rec.lengths <- read.csv(here("data", "forSS3", "Lcomps_recreational_unsexed_raw_10_50.csv")) %>%
  dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

lcomps.df <- dplyr::bind_rows(com.lengths, rec.lengths) 

mod$dat$lencomp <- lcomps.df


# Age comps not added yet


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

#Plot just the data figure (output as "data.png")
pp <- SS_output(here('models', "_bridging_runs", new_name))
SSplotData(pp, print = TRUE, subplots = 1) 


####------------------------------------------------#
## 0_2_3_update_Comps_oldFleets ----
####------------------------------------------------#

# Include age comps now for old fleets alongwith length comps. Also include
# ageing error

new_name <- "0_2_3_update_Comps_oldFleets"
old_name <- "0_2_2_update_Lcomps_oldFleets"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

#Set up fleet converter to set up any com to fleet=1, rec to fleet=2

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec")) %>%
  dplyr::mutate(fleet_num = c(1, 2)) %>%
  dplyr::select(fleetname, fleet, fleet_num)


## Update age comps

# Age comps

mod$dat$agebin_vector <- seq(1, 60, by = 1)
mod$dat$N_agebins <- length(mod$dat$agebin_vector)
#Ageing error is up to max age so dont need to reduce to number of data age bins
#mod$dat$ageerror <- mod$dat$ageerror[, 1:(max(mod$dat$agebin_vector) + 1)]

mod$dat$age_info$combine_M_F <- 0 #dont compress males with females

mod$dat$lbin_method <- 2 #this is the current value, but useful to set.
#Requires length bins to be set to the length bin index, so need to change CAAL
#to reflect bin index. Could set this to 3 and keep length bins as is (i.e. as lengths)

com.CAAL <- read.csv(here("data", "forSS3", "CAAL_PacFIN_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

mod$dat$agecomp <- com.CAAL

# Add ageing error matrix
ageerr <- read.csv(here("data", "forSS3", "Ageing_error_B01S11_forSS.csv"))

age_ind <- grep(mod$dat$Nages, colnames(ageerr))
mod$dat$ageerror <- ageerr[,1:age_ind]


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

#Plot just the data figure (output as "data.png")
pp <- SS_output(here('models', "_bridging_runs", new_name))
SSplotData(pp, print = TRUE, subplots = 1)


####------------------------------------------------#
## 0_2_3b_noAgeErr ----
####------------------------------------------------#

# Remove ageing error to see how that affects things compared to 
# model with updating length and age comps of old fleets

new_name <- "0_2_3b_noAgeErr"
old_name <- "0_2_3_update_Comps_oldFleets"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

# Remove ageing error matrix
mod$dat$ageerror[1,] <- -1
mod$dat$ageerror[2,] <- 0.01


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_2_3_update_Comps_oldFleets",
                                                 "0_2_3b_noAgeErr")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Update length/age comps of existing fleets',
                                     'Reset to old age err'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', "_bridging_runs", new_name))



####------------------------------------------------#
## 0_2_4_update_Comps_addGrowth ----
####------------------------------------------------#

# Adds the growth fleet to the old fleet structure and the old fleets have
# all data updated (length and age comps). Also includes ageing error and estimates growth.

new_name <- "0_2_4_update_Comps_addGrowth"
old_name <- "0_2_1_updateCatch"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

# We need to change fleet structure because we are adding CAAL for growth fleet
# Because the growth fleet doesn't have index values we dont change CPUEinfo

mod$dat$Nfleets <- 3
mod$dat$fleetinfo <- rbind(mod$dat$fleetinfo,
                           c("type" = 3, "surveytiming" = 1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_Growth"))
mod$dat$len_info <- rbind(mod$dat$len_info, #set to match that of the other fleets
                          "CA_Growth" = mod$dat$len_info[1,])
mod$dat$age_info <- rbind(mod$dat$age_info, #set to match that of the other fleets
                          "CA_Growth" = mod$dat$age_info[1,])
mod$dat$age_info$combine_M_F <- 0
mod$dat$CPUEinfo <- rbind(mod$dat$CPUEinfo,
                          "CA_Growth" = c("fleet" = 3, "units" = 1, "errtype" = 0, "SD_report" = 0))

#Set up fleet converter to set up any com to fleet=1, rec to fleet=2, growth = fleet=3

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3)) %>%
  dplyr::select(fleetname, fleet, fleet_num)


## Update comps

# Length comps

mod$dat$use_lencomp <- 1 #already 1 but useful to set
mod$dat$lbin_vector <- seq(10, 50, by = 2)
mod$dat$N_lbins <- length(mod$dat$lbin_vector)

com.lengths <- read.csv(here("data", "forSS3", "Lcomps_PacFIN_unsexed_expanded_10_50.csv")) %>%
  dplyr::mutate(fleet = "com") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

rec.lengths <- read.csv(here("data", "forSS3", "Lcomps_recreational_unsexed_raw_10_50.csv")) %>%
  dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

lcomps.df <- dplyr::bind_rows(com.lengths, rec.lengths) 

mod$dat$lencomp <- lcomps.df

# Age comps

mod$dat$agebin_vector <- seq(1, 60, by = 1)
mod$dat$N_agebins <- length(mod$dat$agebin_vector)
#Ageing error is up to max age so dont need to reduce to number of data age bins
#mod$dat$ageerror <- mod$dat$ageerror[, 1:(max(mod$dat$agebin_vector) + 1)]

mod$dat$lbin_method <- 2 #this is the current value, but useful to set.
#Requires length bins to be set to the length bin index, so need to change CAAL
#to reflect bin index. Could set this to 3 and keep length bins as is (i.e. as lengths)

com.CAAL <- read.csv(here("data", "forSS3", "CAAL_PacFIN_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

growth.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_all_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = "growth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
  
mod$dat$agecomp <- dplyr::bind_rows(com.CAAL, growth.CAAL)


## Add ageing error matrix

ageerr <- read.csv(here("data", "forSS3", "Ageing_error_B01S11_forSS.csv"))

age_ind <- grep(mod$dat$Nages, colnames(ageerr))
mod$dat$ageerror <- ageerr[,1:age_ind]


## Now change the selectivity tables....

mod$ctl$size_selex_types <- rbind(mod$ctl$size_selex_types, #set to match that of the other fleets
                                  "CA_Growth" = mod$ctl$size_selex_types[1,])
mod$ctl$size_selex_types["CA_Growth", "Pattern"] <- 0

mod$ctl$age_selex_types <- rbind(mod$ctl$age_selex_types, #set to match that of the other fleets
                                 "CA_Growth" = mod$ctl$age_selex_types[1,])

#But because the growth fleet as selectivity type = 0, no need to update parameterization


## Estimate growth

#Phase 2 for L1, L2 and K as well as CVs 
mod$ctl$MG_parms$PHASE[2:6] <- 2


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SSplotData(pp, print = TRUE, subplots = 1)
SS_plots(pp, plot = c(1:26))


####------------------------------------------------#
## 0_2_5_update_Indices_noGrowth ----
####------------------------------------------------#

# Update model data for indices and comps for new index fleets
# Right now exclude updating comps for non-index fleets which means growth fleet
# data is not included

new_name <- "0_2_5_update_Indices_noGrowth"
old_name <- "0_2_1_updateCatch"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

## Add fleets for indices. Keep fleet structure from 0_2_4_updateComps_newFleets
# which has the growth fleet

mod$dat$Nfleets <- 5
mod$dat$fleetinfo <- rbind(mod$dat$fleetinfo,
                           c("type" = 3, "surveytiming" = 1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_Growth"),
                           c("type" = 3, "surveytiming" = 1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_CCFRP"),
                           c("type" = 3, "surveytiming" = 1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_ROV"))
mod$dat$len_info <- rbind(mod$dat$len_info, #set to match that of the other fleets
                          "CA_Growth" = mod$dat$len_info[1,],
                          "CA_CCFRP" = mod$dat$len_info[1,],
                          "CA_ROV" = mod$dat$len_info[1,])
mod$dat$age_info <- rbind(mod$dat$age_info, #set to match that of the other fleets
                          "CA_Growth" = mod$dat$age_info[1,],
                          "CA_CCFRP" = mod$dat$age_info[1,],
                          "CA_ROV" = mod$dat$age_info[1,])
mod$dat$age_info$combine_M_F <- 0
mod$dat$CPUEinfo <- rbind(mod$dat$CPUEinfo,
                          "CA_Growth" = c("fleet" = 3, "units" = 1, "errtype" = 0, "SD_report" = 0),
                          "CA_CCFRP" = c("fleet" = 4, "units" = 0, "errtype" = 0, "SD_report" = 0),
                          "CA_ROV" = c("fleet" = 5, "units" = 0, "errtype" = 0, "SD_report" = 0))

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)


## Add index data

#CCFRP
ccfrp_index <- read.csv(here("data", "forSS3", "CCFRP_withFN_weighted_index_forSS.csv")) %>%
  dplyr::mutate(fleet = "ccfrp") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

#PR dockside
pr_index <- read.csv(here("data", "forSS3", "PR_dockside_index_forSS.csv")) %>%
  dplyr::mutate(fleet = "rec") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

#ROV
rov_index <- read.csv(here("data", "forSS3", "ROV_index_forSS.csv")) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()


mod$dat$CPUE <- dplyr::bind_rows(pr_index, ccfrp_index, rov_index)


## Add q setup for surveys with index data

#Base the number on fleetinfo and any fishery fleets with CPUE data
cpuefleets <- unique(c(unique(mod$dat$CPUE$index)))
mod$ctl$Q_options <- data.frame("fleet" = cpuefleets,
                                "link" = 1,
                                "link_info" = 0,
                                "extra_se" = 0,
                                "biasadj" = 0,
                                "float" = 0,
                                row.names = paste(cpuefleets, 
                                                  fleet.converter[cpuefleets, "fleetname"], 
                                                  sep = "_"))

mod$ctl$Q_parms <- data.frame("LO" = rep(-25, length(cpuefleets)),
                              "HI" = 25,
                              "INIT" = 0,
                              "PRIOR" = 0,
                              "PR_SD" = 1,
                              "PR_type" = 0,
                              "PHASE" = 2,
                              "env_var&link" = 0,
                              "dev_link" = 0,
                              "dev_minyr" = 0,
                              "dev_maxyr" = 0,
                              "dev_PH" = 0,
                              "Block" = 0,
                              "Block_Fxn" = 0,
                              row.names = paste("LnQ", "base", cpuefleets, 
                                                fleet.converter[cpuefleets, "fleetname"], 
                                                sep = "_"))


## Add composition data for indices

#CCFRP
#these use number of drifts as sample size. Fleet is hard coded so dont need
ccfrp.lengths <- read.csv(here("data", "forSS3", "Lcomps_ccfrp_withFN_weighted_length_comps_unsexed.csv")) %>%
  dplyr::select(-InputN.Year) %>%
  #dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(ccfrp.lengths) <- names(mod$dat$lencomp)

#ROV
#Use number of transects as sample size.
rov.lengths <- read.csv(here("data", "forSS3", "Lcomps_rov_unsexed_weighted_ACTUAL_YEAR_10_50.csv")) %>%
  #dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(rov.lengths) <- names(mod$dat$lencomp)

mod$dat$lencomp <- dplyr::bind_rows(mod$dat$lencomp, ccfrp.lengths, rov.lengths)

#Now change the selectivity tables....
mod$ctl$size_selex_types <- rbind(mod$ctl$size_selex_types, #set to match that of the other fleets
                                  "CA_Growth" = mod$ctl$size_selex_types[1,],
                                  "CA_CCFRP" = mod$ctl$size_selex_types[1,],
                                  "CA_ROV" = mod$ctl$size_selex_types[1,])
mod$ctl$size_selex_types["CA_Growth", "Pattern"] <- 0

mod$ctl$age_selex_types <- rbind(mod$ctl$age_selex_types, #set to match that of the other fleets
                                 "CA_Growth" = mod$ctl$age_selex_types[1,],
                                 "CA_CCFRP" = mod$ctl$age_selex_types[1,],
                                 "CA_ROV" = mod$ctl$age_selex_types[1,])

#...and length selectivity parameterization 
#Set the new fleets selectivity to be the same as the rec fleet for now. 
#Because the growth fleet selectivity type = 0, no need to have selectivity for that
mod$ctl$size_selex_parms <- rbind(mod$ctl$size_selex_parms,
                                  mod$ctl$size_selex_parms[7:12,],
                                  mod$ctl$size_selex_parms[7:12,])

selex_fleets <- rownames(mod$ctl$size_selex_types)[mod$ctl$size_selex_types$Pattern == 24] |>
  as.list()
selex_names <- purrr::map(selex_fleets,
                          ~ glue::glue('SizeSel_P_{par}_{fleet_name}({fleet_no})',
                                       par = 1:6,
                                       fleet_name = .x,
                                       fleet_no = fleet.converter$fleet_num[fleet.converter$fleetname == .x])) |>
  unlist()

rownames(mod$ctl$size_selex_parms) <- selex_names



##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SSplotData(pp, print = TRUE, subplots = 1)
SS_plots(pp, plot = c(1:26))


####------------------------------------------------#
## 0_2_5b_PRindexOnly ----
####------------------------------------------------#

# Only update PR index to see effect compared to index model

new_name <- "0_2_5b_PRindexOnly"
old_name <- "0_2_1_updateCatch"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec")) %>%
  dplyr::mutate(fleet_num = c(1, 2)) %>%
  dplyr::select(fleetname, fleet, fleet_num)


## Add index data

pr_index <- read.csv(here("data", "forSS3", "PR_dockside_index_forSS.csv")) %>%
  dplyr::mutate(fleet = "rec") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

mod$dat$CPUE <- pr_index


## Add q setup for surveys with index data

#Base the number on fleetinfo and any fishery fleets with CPUE data
cpuefleets <- unique(c(unique(mod$dat$CPUE$index)))
mod$ctl$Q_options <- data.frame("fleet" = cpuefleets,
                                "link" = 1,
                                "link_info" = 0,
                                "extra_se" = 0,
                                "biasadj" = 0,
                                "float" = 0,
                                row.names = paste(cpuefleets, 
                                                  fleet.converter[cpuefleets, "fleetname"], 
                                                  sep = "_"))

mod$ctl$Q_parms <- data.frame("LO" = rep(-25, length(cpuefleets)),
                              "HI" = 25,
                              "INIT" = 0,
                              "PRIOR" = 0,
                              "PR_SD" = 1,
                              "PR_type" = 0,
                              "PHASE" = 2,
                              "env_var&link" = 0,
                              "dev_link" = 0,
                              "dev_minyr" = 0,
                              "dev_maxyr" = 0,
                              "dev_PH" = 0,
                              "Block" = 0,
                              "Block_Fxn" = 0,
                              row.names = paste("LnQ", "base", cpuefleets, 
                                                fleet.converter[cpuefleets, "fleetname"], 
                                                sep = "_"))


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SSplotData(pp, print = TRUE, subplots = 1)
SS_plots(pp, plot = c(1:26))


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_2_1_updateCatch",
                                                 "0_2_5_update_Indices_noGrowth",
                                                 "0_2_5b_PRindexOnly")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Update catch',
                                     'Update indices',
                                     'Update only PR index'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', "_bridging_runs", new_name))


####------------------------------------------------#
## 0_2_6_updateAllData_nogrowth ----
####------------------------------------------------#

# Update all data, including catches and comps for existing fleets, new comps 
# and indices for index fleets, and new data for the growth fleet.
# Although growth fleet is included, do not estimate growth. 

new_name <- "0_2_6_updateAllData_nogrowth"
old_name <- "0_1_0_updateAllBio"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

mod$dat$endyr <- 2024

## Add fleets for indices. Keep fleet structure from 0_2_4_updateComps_newFleets
# which has the growth fleet

mod$dat$Nfleets <- 5
mod$dat$fleetinfo <- rbind(mod$dat$fleetinfo,
                           c("type" = 3, "surveytiming" = 1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_Growth"),
                           c("type" = 3, "surveytiming" = 1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_CCFRP"),
                           c("type" = 3, "surveytiming" = 1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_ROV"))
mod$dat$len_info <- rbind(mod$dat$len_info, #set to match that of the other fleets
                          "CA_Growth" = mod$dat$len_info[1,],
                          "CA_CCFRP" = mod$dat$len_info[1,],
                          "CA_ROV" = mod$dat$len_info[1,])
mod$dat$age_info <- rbind(mod$dat$age_info, #set to match that of the other fleets
                          "CA_Growth" = mod$dat$age_info[1,],
                          "CA_CCFRP" = mod$dat$age_info[1,],
                          "CA_ROV" = mod$dat$age_info[1,])
mod$dat$age_info$combine_M_F <- 0
mod$dat$CPUEinfo <- rbind(mod$dat$CPUEinfo,
                          "CA_Growth" = c("fleet" = 3, "units" = 1, "errtype" = 0, "SD_report" = 0),
                          "CA_CCFRP" = c("fleet" = 4, "units" = 0, "errtype" = 0, "SD_report" = 0),
                          "CA_ROV" = c("fleet" = 5, "units" = 0, "errtype" = 0, "SD_report" = 0))

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)


## Add catch data

catches <- read.csv(here("data", "CAquillback_total_removals.csv"))
catches[is.na(catches)] <- 0

updated.catch.df <- catches %>%
  dplyr::select(c(Year, com_tot, rec_tot)) %>%
  tidyr::pivot_longer(cols = -Year, names_to = 'fleet', values_to = 'catch', 
                      names_pattern = '(.*)_') %>% #keep everything before _
  dplyr::left_join(fleet.converter) %>%
  dplyr::mutate(seas = 1, 
                catch_se = 0.05) %>%
  dplyr::select(year = Year, seas, fleet = fleet_num, catch, catch_se) %>%
  dplyr::arrange(fleet, year) %>%
  as.data.frame()

mod$dat$catch <- updated.catch.df


## Add index data

#CCFRP
ccfrp_index <- read.csv(here("data", "forSS3", "CCFRP_withFN_weighted_index_forSS.csv")) %>%
  dplyr::mutate(fleet = "ccfrp") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

#PR dockside
pr_index <- read.csv(here("data", "forSS3", "PR_dockside_index_forSS.csv")) %>%
  dplyr::mutate(fleet = "rec") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

#ROV
rov_index <- read.csv(here("data", "forSS3", "ROV_index_forSS.csv")) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()


mod$dat$CPUE <- dplyr::bind_rows(pr_index, ccfrp_index, rov_index)


## Add q setup for surveys with index data

#Base the number on fleetinfo and any fishery fleets with CPUE data
cpuefleets <- unique(c(unique(mod$dat$CPUE$index)))
mod$ctl$Q_options <- data.frame("fleet" = cpuefleets,
                                "link" = 1,
                                "link_info" = 0,
                                "extra_se" = 0,
                                "biasadj" = 0,
                                "float" = 0,
                                row.names = paste(cpuefleets, 
                                                  fleet.converter[cpuefleets, "fleetname"], 
                                                  sep = "_"))

mod$ctl$Q_parms <- data.frame("LO" = rep(-25, length(cpuefleets)),
                              "HI" = 25,
                              "INIT" = 0,
                              "PRIOR" = 0,
                              "PR_SD" = 1,
                              "PR_type" = 0,
                              "PHASE" = 2,
                              "env_var&link" = 0,
                              "dev_link" = 0,
                              "dev_minyr" = 0,
                              "dev_maxyr" = 0,
                              "dev_PH" = 0,
                              "Block" = 0,
                              "Block_Fxn" = 0,
                              row.names = paste("LnQ", "base", cpuefleets, 
                                                fleet.converter[cpuefleets, "fleetname"], 
                                                sep = "_"))


## Add or update comps

# Length comps

mod$dat$use_lencomp <- 1 #already 1 but useful to set
mod$dat$lbin_vector <- seq(10, 50, by = 2)
mod$dat$N_lbins <- length(mod$dat$lbin_vector)

com.lengths <- read.csv(here("data", "forSS3", "Lcomps_PacFIN_unsexed_expanded_10_50.csv")) %>%
  dplyr::mutate(fleet = "com") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(com.lengths) <- names(mod$dat$lencomp)

rec.lengths <- read.csv(here("data", "forSS3", "Lcomps_recreational_unsexed_raw_10_50.csv")) %>%
  dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(rec.lengths) <- names(mod$dat$lencomp)

ccfrp.lengths <- read.csv(here("data", "forSS3", "Lcomps_ccfrp_withFN_weighted_length_comps_unsexed.csv")) %>%
  dplyr::select(-InputN.Year) %>%
  #dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(ccfrp.lengths) <- names(mod$dat$lencomp)

rov.lengths <- read.csv(here("data", "forSS3", "Lcomps_rov_unsexed_weighted_ACTUAL_YEAR_10_50.csv")) %>%
  #dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(rov.lengths) <- names(mod$dat$lencomp)

mod$dat$lencomp <- dplyr::bind_rows(com.lengths, rec.lengths, ccfrp.lengths, rov.lengths) 

# Age comps

mod$dat$agebin_vector <- seq(1, 60, by = 1)
mod$dat$N_agebins <- length(mod$dat$agebin_vector)

mod$dat$lbin_method <- 2 #this is the current value, but useful to set.
#Requires length bins to be set to the length bin index, so need to change CAAL
#to reflect bin index. Could set this to 3 and keep length bins as is (i.e. as lengths)

com.CAAL <- read.csv(here("data", "forSS3", "CAAL_PacFIN_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

growth.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_all_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = "growth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

mod$dat$agecomp <- dplyr::bind_rows(com.CAAL, growth.CAAL)

#Add ageing error matrix
ageerr <- read.csv(here("data", "forSS3", "Ageing_error_B01S11_forSS.csv"))
age_ind <- grep(mod$dat$Nages, colnames(ageerr))
mod$dat$ageerror <- ageerr[,1:age_ind]


## Now change the selectivity tables....

mod$ctl$size_selex_types <- rbind(mod$ctl$size_selex_types, #set to match that of the other fleets
                                  "CA_Growth" = mod$ctl$size_selex_types[1,],
                                  "CA_CCFRP" = mod$ctl$size_selex_types[1,],
                                  "CA_ROV" = mod$ctl$size_selex_types[1,])
mod$ctl$size_selex_types["CA_Growth", "Pattern"] <- 0

mod$ctl$age_selex_types <- rbind(mod$ctl$age_selex_types, #set to match that of the other fleets
                                 "CA_Growth" = mod$ctl$age_selex_types[1,],
                                 "CA_CCFRP" = mod$ctl$age_selex_types[1,],
                                 "CA_ROV" = mod$ctl$age_selex_types[1,])

#...and length selectivity parameterization 
#Set the new fleets selectivity to be the same as the rec fleet for now. 
#Because the growth fleet selectivity type = 0, no need to have selectivity for that
mod$ctl$size_selex_parms <- rbind(mod$ctl$size_selex_parms,
                                  mod$ctl$size_selex_parms[7:12,],
                                  mod$ctl$size_selex_parms[7:12,])

selex_fleets <- rownames(mod$ctl$size_selex_types)[mod$ctl$size_selex_types$Pattern == 24] |>
  as.list()
selex_names <- purrr::map(selex_fleets,
                          ~ glue::glue('SizeSel_P_{par}_{fleet_name}({fleet_no})',
                                       par = 1:6,
                                       fleet_name = .x,
                                       fleet_no = fleet.converter$fleet_num[fleet.converter$fleetname == .x])) |>
  unlist()

rownames(mod$ctl$size_selex_parms) <- selex_names


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SSplotData(pp, print = TRUE, subplots = 1)
SS_plots(pp, plot = c(1:26))


####------------------------------------------------#
## 0_2_0_updateAllData ----
####------------------------------------------------#

# Now estimate growth based on the new data

new_name <- "0_2_0_updateAllData"
old_name <- "0_2_6_updateAllData_nogrowth"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

#Phase 2 for L1, L2 and K as well as CVs 
mod$ctl$MG_parms$PHASE[2:6] <- 2


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SS_plots(pp, plot = c(1:26))


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_0_1_2025rename",
                                                 "0_1_0_updateAllBio",
                                                 "0_2_1_updateCatch",
                                                 "0_2_2_update_Lcomps_oldFleets",
                                                 "0_2_3_update_Comps_oldFleets",
                                                 "0_2_4_update_Comps_addGrowth",
                                                 "0_2_5_update_Indices_noGrowth",
                                                 "0_2_6_updateAllData_nogrowth",
                                                 "0_2_0_updateAllData")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('2021 base',
                                     'All bio relationships',
                                     'Update catch',
                                     'Update length comps of existing fleets',
                                     'Update length/age comps of existing fleets',
                                     '+ add growth fleet and estimate growth',
                                     'Add new index fleets (new indices and comps)',
                                     'All data but do not estimate growth',
                                     'All data and estimate growth'),
                    subplots = c(1,3), print = TRUE, legendloc = "topleft",
                    plotdir = here('models', "_bridging_runs", new_name))

dev.off()



##-------------------------------------------------------------------##
#--------------------Changes to model inputs-------------------------
##-------------------------------------------------------------------##


####------------------------------------------------#
## 0_3_1_selexInits_parms1234 ----
####------------------------------------------------#

# Update initial selectivity parameters based on new data. Update the initial 
# parameters for parameters 1-4 but keep those for 5-6. Given previous inits 
# for parameter 5 and 6 aren't -999 changing parameter 4 is unlikely to change 
# model results

new_name <- "0_3_1_selexInits_parms1234"
old_name <- "0_2_0_updateAllData"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)

selex_new <- mod$ctl$size_selex_parms

#since all PR_types are zero these aren't used, just set to any number
selex_new$PR_SD <- 0 
selex_new$PRIOR <- 0

# Fix three parameters of double normal
# -999 for p5 and p6 means set control to p3 and p4
selex_new$INIT[grep('P_2', rownames(selex_new))] <- -15
#selex_new$INIT[grep('P_5', rownames(selex_new))] <- -999
#selex_new$INIT[grep('P_6', rownames(selex_new))] <- -999
selex_new$PHASE[grep('P_2', rownames(selex_new))] <- -9
selex_new$PHASE[grep('P_5', rownames(selex_new))] <- -9
selex_new$PHASE[grep('P_6', rownames(selex_new))] <- -9
selex_new$LO[grep('P_2', rownames(selex_new))] <- -20
selex_new$HI[grep('P_2', rownames(selex_new))] <- 20

# calculate initial values for p1, p3, p4 for each fleet
selex_modes <- mod$dat$lencomp |>
  dplyr::arrange(fleet) |>
  dplyr::group_by(fleet) |>
  dplyr::summarise(dplyr::across(l10:l50, ~ sum(Nsamp*.x)/sum(Nsamp))) |> 
  tidyr::pivot_longer(cols = -fleet, names_to = 'len_bin', values_to = 'dens') |>
  tidyr::separate(col = len_bin, into = c('sex', 'length'), sep = 1) |> #with unsexed sex is just "l"
  dplyr::group_by(fleet, sex) |> 
  dplyr::summarise(mode = length[which.max(dens)]) |>
  dplyr::summarise(mode = mean(as.numeric(mode))) |>
  dplyr::mutate(asc.slope = log(8*(mode - 12)),
                desc.slope = log(8*(66-mode)))

selex_fleets <- rownames(mod$ctl$size_selex_types)[mod$ctl$size_selex_types$Pattern == 24] |>
  as.list()

# Parameter 1
p1.ind <- grep('P_1', rownames(selex_new))
selex_new$LO[p1.ind] <- 11 #midpoint of first bin
selex_new$HI[p1.ind] <- 51 #midpoint of last bin
selex_new$PHASE[p1.ind] <- 4
#Only update the mode for fleets we have length data for
p1.ind.2 <- intersect(
  grep('P_1', rownames(selex_new)),  
  grep(paste0(fleet.converter[fleet.converter$fleet_num %in% unique(mod$dat$lencomp$fleet), "fleetname"], collapse = "|"), 
       rownames(selex_new)))
selex_new$INIT[p1.ind.2] <- purrr::map(selex_fleets, 
                                       ~ selex_modes$mode[selex_modes$fleet == fleet.converter$fleet_num[fleet.converter$fleetname == .x]]) |> 
  unlist()


### P_3
p3.ind <- grep('P_3', rownames(selex_new))
selex_new$LO[p3.ind] <- 0 #could go negative but slope is super steep. Good to have 0 as the bound
selex_new$HI[p3.ind] <- 9
selex_new$PHASE[p3.ind] <- 5
#Only update the mode for fleets we have length data for
p3.ind.2 <- intersect(
  grep('P_3', rownames(selex_new)),  
  grep(paste0(fleet.converter[fleet.converter$fleet_num %in% unique(mod$dat$lencomp$fleet), "fleetname"], collapse = "|"), 
       rownames(selex_new)))
selex_new$INIT[p3.ind.2] <- purrr::map(selex_fleets, 
                                       ~ selex_modes$asc.slope[selex_modes$fleet == 
                                                                 fleet.converter$fleet_num[fleet.converter$fleetname == .x]]) |>
  unlist()

## P_4
p4.ind <- grep('P_4', rownames(selex_new))
selex_new$LO[p4.ind] <- 0 #could go negative but slope is super steep. Good to have 0 as the bound
selex_new$HI[p4.ind] <- 9
selex_new$PHASE[p4.ind] <- 5
#Only update the mode for fleets we have length data for
p4.ind.2 <- intersect(
  grep('P_4', rownames(selex_new)),
  grep(paste0(fleet.converter[fleet.converter$fleet_num %in% unique(mod$dat$lencomp$fleet), "fleetname"], collapse = "|"),
       rownames(selex_new)))
selex_new$INIT[p4.ind.2] <- purrr::map(selex_fleets,
                                       ~ selex_modes$desc.slope[selex_modes$fleet ==
                                                                  fleet.converter$fleet_num[fleet.converter$fleetname == .x]]) |>
  unlist()

#Set new selectivity parameters
mod$ctl$size_selex_parms <- selex_new


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_2_0_updateAllData",
                                                 "0_3_1_selexInits_parms1234")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('All data and estimate growth',
                                     'Update selex inits'),
                    subplots = c(1,3), print = TRUE, legendloc = "topleft",
                    plotdir = here('models', "_bridging_runs", new_name))

dev.off()



####------------------------------------------------#
## 0_3_2_selexInits_parm12356 ----
####------------------------------------------------#

# Update initial selectivity parameters based on new data. Update the initial 
# parameters for parameters 1-3, keeping parameter 4 at previous values and 
# setting inits of parameter 5 and 6 to -999 so now parameter 4 controls domed-ness 

new_name <- "0_3_2_selexInits_parms12356"
old_name <- "0_2_0_updateAllData"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)

selex_new <- mod$ctl$size_selex_parms

#since all PR_types are zero these aren't used, just set to any number
selex_new$PR_SD <- 0 
selex_new$PRIOR <- 0

# Fix three parameters of double normal
# -999 for p5 and p6 means set control to p3 and p4
selex_new$INIT[grep('P_2', rownames(selex_new))] <- -15
selex_new$INIT[grep('P_5', rownames(selex_new))] <- -999
selex_new$INIT[grep('P_6', rownames(selex_new))] <- -999
selex_new$PHASE[grep('P_2', rownames(selex_new))] <- -9
selex_new$PHASE[grep('P_5', rownames(selex_new))] <- -9
selex_new$PHASE[grep('P_6', rownames(selex_new))] <- -9
selex_new$LO[grep('P_2', rownames(selex_new))] <- -20
selex_new$HI[grep('P_2', rownames(selex_new))] <- 20

# calculate initial values for p1, p3, p4 for each fleet
selex_modes <- mod$dat$lencomp |>
  dplyr::arrange(fleet) |>
  dplyr::group_by(fleet) |>
  dplyr::summarise(dplyr::across(l10:l50, ~ sum(Nsamp*.x)/sum(Nsamp))) |> 
  tidyr::pivot_longer(cols = -fleet, names_to = 'len_bin', values_to = 'dens') |>
  tidyr::separate(col = len_bin, into = c('sex', 'length'), sep = 1) |> #with unsexed sex is just "l"
  dplyr::group_by(fleet, sex) |> 
  dplyr::summarise(mode = length[which.max(dens)]) |>
  dplyr::summarise(mode = mean(as.numeric(mode))) |>
  dplyr::mutate(asc.slope = log(8*(mode - 12)),
                desc.slope = log(8*(66-mode)))

selex_fleets <- rownames(mod$ctl$size_selex_types)[mod$ctl$size_selex_types$Pattern == 24] |>
  as.list()

# Parameter 1
p1.ind <- grep('P_1', rownames(selex_new))
selex_new$LO[p1.ind] <- 11 #midpoint of first bin
selex_new$HI[p1.ind] <- 51 #midpoint of last bin
selex_new$PHASE[p1.ind] <- 4
#Only update the mode for fleets we have length data for
p1.ind.2 <- intersect(
  grep('P_1', rownames(selex_new)),  
  grep(paste0(fleet.converter[fleet.converter$fleet_num %in% unique(mod$dat$lencomp$fleet), "fleetname"], collapse = "|"), 
       rownames(selex_new)))
selex_new$INIT[p1.ind.2] <- purrr::map(selex_fleets, 
                                       ~ selex_modes$mode[selex_modes$fleet == fleet.converter$fleet_num[fleet.converter$fleetname == .x]]) |> 
  unlist()


### P_3
p3.ind <- grep('P_3', rownames(selex_new))
selex_new$LO[p3.ind] <- 0 #could go negative but slope is super steep. Good to have 0 as the bound
selex_new$HI[p3.ind] <- 9
selex_new$PHASE[p3.ind] <- 5
#Only update the mode for fleets we have length data for
p3.ind.2 <- intersect(
  grep('P_3', rownames(selex_new)),  
  grep(paste0(fleet.converter[fleet.converter$fleet_num %in% unique(mod$dat$lencomp$fleet), "fleetname"], collapse = "|"), 
       rownames(selex_new)))
selex_new$INIT[p3.ind.2] <- purrr::map(selex_fleets, 
                                       ~ selex_modes$asc.slope[selex_modes$fleet == 
                                                                 fleet.converter$fleet_num[fleet.converter$fleetname == .x]]) |>
  unlist()

## P_4
# p4.ind <- grep('P_4', rownames(selex_new))
# selex_new$LO[p4.ind] <- 0 #could go negative but slope is super steep. Good to have 0 as the bound
# selex_new$HI[p4.ind] <- 9
# selex_new$PHASE[p4.ind] <- 5
# #Only update the mode for fleets we have length data for
# p4.ind.2 <- intersect(
#   grep('P_4', rownames(selex_new)),
#   grep(paste0(fleet.converter[fleet.converter$fleet_num %in% unique(mod$dat$lencomp$fleet), "fleetname"], collapse = "|"),
#        rownames(selex_new)))
# selex_new$INIT[p4.ind.2] <- purrr::map(selex_fleets,
#                                        ~ selex_modes$desc.slope[selex_modes$fleet ==
#                                                                   fleet.converter$fleet_num[fleet.converter$fleetname == .x]]) |>
#   unlist()

#Set new selectivity parameters
mod$ctl$size_selex_parms <- selex_new


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_2_0_updateAllData",
                                                 "0_3_2_selexInits_parms12356")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('All data and estimate growth',
                                     'Update selex inits'),
                    subplots = c(1,3), print = TRUE, legendloc = "topleft",
                    plotdir = here('models', "_bridging_runs", new_name))

dev.off()


####------------------------------------------------#
## 0_3_2b_estParm4 ----
####------------------------------------------------#

# Copy previous model but now allow parameter 4 to be estimated for non-survey
# fleets (i.e. not CCFRP nor ROV)

new_name <- "0_3_2b_estParm4"
old_name <- "0_3_2_selexInits_parms12356"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

ind <- intersect(grep('P_4', rownames(mod$ctl$size_selex_parms)),
                 grep('Comm|Rec', rownames(mod$ctl$size_selex_parms)))
mod$ctl$size_selex_parms[ind, "PHASE"] <- 5


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_3_2_selexInits_parms12356",
                                                 "0_3_2b_estParm4")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Update selex inits for 12356',
                                     'Estimate parameter 4'),
                    subplots = c(1,3), print = TRUE, legendloc = "topleft",
                    plotdir = here('models', "_bridging_runs", new_name))

dev.off()



####------------------------------------------------#
## 0_3_3_selexInits_All ----
####------------------------------------------------#

# Update initial selectivity parameters based on new data. Update the initial 
# parameters for parameters 1-4, and setting inits of parameter 5 and 6 to 
# -999 which gives parameter 4 control of domed-ness. Fix parameter 4 for 
# non-survey fleets to a large positive number

new_name <- "0_3_3_selexInits_All"
old_name <- "0_2_0_updateAllData"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)

selex_new <- mod$ctl$size_selex_parms

#since all PR_types are zero these aren't used, just set to any number
selex_new$PR_SD <- 0 
selex_new$PRIOR <- 0

# Fix three parameters of double normal
# -999 for p5 and p6 means set control to p3 and p4
selex_new$INIT[grep('P_2', rownames(selex_new))] <- -15
selex_new$INIT[grep('P_5', rownames(selex_new))] <- -999
selex_new$INIT[grep('P_6', rownames(selex_new))] <- -999
selex_new$PHASE[grep('P_2', rownames(selex_new))] <- -9
selex_new$PHASE[grep('P_5', rownames(selex_new))] <- -9
selex_new$PHASE[grep('P_6', rownames(selex_new))] <- -9
selex_new$LO[grep('P_2', rownames(selex_new))] <- -20
selex_new$HI[grep('P_2', rownames(selex_new))] <- 20

# calculate initial values for p1, p3, p4 for each fleet
selex_modes <- mod$dat$lencomp |>
  dplyr::arrange(fleet) |>
  dplyr::group_by(fleet) |>
  dplyr::summarise(dplyr::across(l10:l50, ~ sum(Nsamp*.x)/sum(Nsamp))) |> 
  tidyr::pivot_longer(cols = -fleet, names_to = 'len_bin', values_to = 'dens') |>
  tidyr::separate(col = len_bin, into = c('sex', 'length'), sep = 1) |> #with unsexed sex is just "l"
  dplyr::group_by(fleet, sex) |> 
  dplyr::summarise(mode = length[which.max(dens)]) |>
  dplyr::summarise(mode = mean(as.numeric(mode))) |>
  dplyr::mutate(asc.slope = log(8*(mode - 12)),
                desc.slope = log(8*(66-mode)))

selex_fleets <- rownames(mod$ctl$size_selex_types)[mod$ctl$size_selex_types$Pattern == 24] |>
  as.list()

# Parameter 1
p1.ind <- grep('P_1', rownames(selex_new))
selex_new$LO[p1.ind] <- 11 #midpoint of first bin
selex_new$HI[p1.ind] <- 51 #midpoint of last bin
selex_new$PHASE[p1.ind] <- 4
#Only update the mode for fleets we have length data for
p1.ind.2 <- intersect(
  grep('P_1', rownames(selex_new)),  
  grep(paste0(fleet.converter[fleet.converter$fleet_num %in% unique(mod$dat$lencomp$fleet), "fleetname"], collapse = "|"), 
       rownames(selex_new)))
selex_new$INIT[p1.ind.2] <- purrr::map(selex_fleets, 
                                       ~ selex_modes$mode[selex_modes$fleet == fleet.converter$fleet_num[fleet.converter$fleetname == .x]]) |> 
  unlist()


### P_3
p3.ind <- grep('P_3', rownames(selex_new))
selex_new$LO[p3.ind] <- 0 #could go negative but slope is super steep. Good to have 0 as the bound
selex_new$HI[p3.ind] <- 9
selex_new$PHASE[p3.ind] <- 5
#Only update the mode for fleets we have length data for
p3.ind.2 <- intersect(
  grep('P_3', rownames(selex_new)),  
  grep(paste0(fleet.converter[fleet.converter$fleet_num %in% unique(mod$dat$lencomp$fleet), "fleetname"], collapse = "|"), 
       rownames(selex_new)))
selex_new$INIT[p3.ind.2] <- purrr::map(selex_fleets, 
                                       ~ selex_modes$asc.slope[selex_modes$fleet == 
                                                                 fleet.converter$fleet_num[fleet.converter$fleetname == .x]]) |>
  unlist()

# P_4
p4.ind <- grep('P_4', rownames(selex_new))
selex_new$LO[p4.ind] <- 0 #could go negative but slope is super steep. Good to have 0 as the bound
selex_new$HI[p4.ind] <- 9
selex_new$PHASE[p4.ind] <- 5
#Only update the mode for fleets we have length data for
p4.ind.2 <- intersect(
  grep('P_4', rownames(selex_new)),
  grep(paste0(fleet.converter[fleet.converter$fleet_num %in% unique(mod$dat$lencomp$fleet), "fleetname"], collapse = "|"),
       rownames(selex_new)))
selex_new$INIT[p4.ind.2] <- purrr::map(selex_fleets,
                                       ~ selex_modes$desc.slope[selex_modes$fleet ==
                                                                  fleet.converter$fleet_num[fleet.converter$fleetname == .x]]) |>
  unlist()
#now for survey fleets fix p_4 at large number
p4.ind.surv <- intersect(p4.ind, grep('CCFRP|ROV', rownames(selex_new)))
selex_new$INIT[p4.ind.surv] <- 15
selex_new$HI[p4.ind.surv] <- 20
selex_new$PHASE[p4.ind.surv] <- -9


#Set new selectivity parameters
mod$ctl$size_selex_parms <- selex_new


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_2_0_updateAllData",
                                                 "0_3_3_selexInits_All")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('All data and estimate growth',
                                     'Update all selex inits'),
                    subplots = c(1,3), print = TRUE, legendloc = "topleft",
                    plotdir = here('models', "_bridging_runs", new_name))

dev.off()


####------------------------------------------------#
## 0_3_4_selexBlocks ----
####------------------------------------------------#

# Add full selectivity blocks

new_name <- "0_3_4_selexBlocks"
old_name <- "0_3_3_selexInits_All"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

# Add selectivity blocks, one each for recreational and commercial
# Setting the end year to -2 sets the forecast selectivity to be the final year,
# otherwise it would get reset to the base selectivity block
mod$ctl$N_Block_Designs <- 2
mod$ctl$blocks_per_pattern <- c(3, 3)
mod$ctl$Block_Design <- list(c(2003, 2013, 2014, 2021, 2022, -2), #commercial fleet
                             c(2001, 2016, 2017, 2022, 2023, -2)) #recreational fleet

### Add block indicators into selectivity table
# Block = number of block to use, Block_Fxn = 2 means replace parameters 
selex_new <- mod$ctl$size_selex_parms

selex_new[intersect(grep("Commercial", rownames(selex_new)), which(selex_new$PHASE > 0)), 
          c("Block")] <- 1
selex_new[intersect(grep("Commercial", rownames(selex_new)), which(selex_new$PHASE > 0)), 
          c("Block_Fxn")] <- 2
selex_new[intersect(grep("Recreational", rownames(selex_new)), which(selex_new$PHASE > 0)), 
          c("Block")] <- 2
selex_new[intersect(grep("Recreational", rownames(selex_new)), which(selex_new$PHASE > 0)), 
          c("Block_Fxn")] <- 2

mod$ctl$size_selex_parms <- selex_new


### Time varying selectivity table
selex_tv_pars <- dplyr::filter(selex_new, Block > 0) |>
  dplyr::select(LO, HI, INIT, PRIOR, PR_SD, PR_type, PHASE, Block) |>
  tidyr::uncount(mod$ctl$blocks_per_pattern[Block], .id = 'id', .remove = FALSE)

rownames(selex_tv_pars) <- rownames(selex_tv_pars) |>
  stringr::str_remove('\\.\\.\\.[:digit:]+') |>
  stringr::str_c('_BLK', selex_tv_pars$Block, 'repl_', mapply("[",mod$ctl$Block_Design[selex_tv_pars$Block], selex_tv_pars$id * 2 - 1))

mod$ctl$size_selex_parms_tv <- selex_tv_pars |>
  dplyr::select(-Block, -id)


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_3_3_selexInits_All",
                                                 "0_3_4_selexBlocks")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Update selex parameters',
                                     'Add blocks'),
                    subplots = c(1,3), print = TRUE, legendloc = "topleft",
                    plotdir = here('models', "_bridging_runs", new_name))

dev.off()


####------------------------------------------------#
## 0_3_5_selexBlocks_fixWarnings ----
####------------------------------------------------#

# Fix all the warnings which deal with forecast setup and entry of blocks
# Also increase R0 init to avoid that warning

new_name <- "0_3_5_selexBlocks_fixWarnings"
old_name <- "0_3_4_selexBlocks"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_bridging_runs", old_name), 
               dir.new = here('models', "_bridging_runs", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_bridging_runs", new_name))


##
#Make Changes
##

# Increase R0 initial to avoid 1st iteration warnings

mod$ctl$SR_parms["SR_LN(R0)", c("INIT", "PRIOR")] <- 5


# Use new forecast formatting to avoid warning

# Type 10 is selectivity, currently set to equal the last year
# Type 11 is relF, currently set to be the last three years
# Type 12 is recruitment, currently set to be all years
mod$fore$Fcast_selex <- -12345
mod$fore$Fcast_years <- data.frame("MG_type" = c(10, 11, 12),
                                   "method" = c(1, 1, 1),
                                   "st_year" = c(0, -3, -999),
                                   "end_year" = c(0, 0, 0),
                                   row.names = c("#_Fcast_years1",
                                                 "#_Fcast_years2",
                                                 "#_Fcast_years3"))


# Adjust selectivity block format to avoid warnings about endyr being past retroyr

# The new forecast formating overrides the timevarying blocking so can reset time blocks
mod$ctl$Block_Design <- list(c(2003, 2013, 2014, 2021, 2022, 2024), #commercial fleet
                             c(2001, 2016, 2017, 2022, 2023, 2024)) #recreational fleet


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_bridging_runs", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_bridging_runs", new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_bridging_runs", new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models', "_bridging_runs"),
                                      subdir = c("0_3_4_selexBlocks",
                                                 "0_3_5_selexBlocks_fixWarnings")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Add blocks',
                                     'Fix warnings'),
                    subplots = c(1,3), print = TRUE, legendloc = "topleft",
                    plotdir = here('models', "_bridging_runs", new_name))

dev.off()


