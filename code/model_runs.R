##########################################################################################-
#
# Model runs for 2025 California Quillback Rockfish 
#   By: Brian Langseth, Melissa Monk, Julia Coates
#
##########################################################################################-
#Alternative to devtools if it doesn't work on your machine
#pak::pkg_install("pfmc-assessments/PEPtools")
#pak::pkg_install("r4ss/r4ss") #Version 1.50.0

#Load packages
library(r4ss)
library(PEPtools)
library(here)
library(dplyr)
library(tictoc)


##########################################################################################-
#                                 ---- TEMPLATE ---- 
#                                 ---- COPY ONLY --- 
##########################################################################################-

#Use this structure to set up model runs below

####------------------------------------------------#
# [model number_model name  ----]
####------------------------------------------------#

new_name <- "model number_model name"
old_name <- "model Number_model name"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

Coded changes go here


##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c('name1',
                                                 'name2',
                                                 new_name)))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('label for model 1',
                                     'label for model 2',
                                     'label for new model'),
                    subplots = c(1,3), print = TRUE, plotdir = here('models', new_name))

dev.off()

##########################################################################################-
#                                 ---- END TEMPLATE ---- 
##########################################################################################-



##########################################################################################-
#                     ---- Set up from 2021 base to 2025 version ----
##########################################################################################-

####------------------------------------------------#
## 0_0_1_2021base  ----
####------------------------------------------------#

# Use most up to date SS3 executable with the 2021 base model

new_name <- "0_0_1_2021base"

##
#Copy inputs
##

copy_SS_inputs(dir.old = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/2_base_model/CA/10_0_0_postNov_base", 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

tictoc::tic()
r4ss::run(dir = here('models',new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess',
          show_in_console = TRUE, 
          skipfinished = FALSE)
tictoc::toc()

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = c("//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/2_base_model/CA", here('models')),
                                      subdir = c('10_0_0_postNov_base',
                                                 new_name)))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('2021 base',
                                     'update exe'),
                    subplots = c(1,3), print = TRUE, plotdir = here('models', new_name))

dev.off()



####------------------------------------------------#
## 0_0_2_2025setup  ----
####------------------------------------------------#

# Change file names and comments to reflect a 2025 model
# Only changing temporal elements here. Model inputs are changed later

new_name <- "0_0_2_2025setup"
old_name <- "0_0_1_2021base"


##
#Copy inputs
##

#Use ss_new inputs so have commonality with other SS3 models
copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


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

#Change inputs
mod$dat$endyr <- 2024


## Forecast file
#Reset benchmark years which were previously hard code. -999 is first model year, 0 is last 
mod$fore$Bmark_years <- c(-999,0, 0,0, 0,0, -999,0, -999,0) #start year and end year for all but selectivity (because of blocks) and relF
mod$fore$Fcast_years <- c(0,0, -3,0, -999,0) #last year for selex, last three years for relF, full time series for average recruitment (though using fcast_rec_option = 0 ignores this)

#Set up forecast even though not yet using
mod$fore$Flimitfraction_m <- data.frame("Year" = 2025:2036, 
                                        "Fraction" = get_buffer(c(2025:2036), sigma = 0.5, pstar = 0.45)[,2])
mod$fore$FirstYear_for_caps_and_allocations <- 2027
mod$fore$ForeCatch <- data.frame("Year" = rep(2025:2026, each = mod$dat$Nfleet),
                                 "Seas" = 1,
                                 "Fleet" = rep(1:mod$dat$Nfleet, 2),
                                 "Catch or F" = 0)


##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

#Because renamed files in starter, new file names are applied. Delete old ones
file.remove(here('models', new_name, c("2021_ca_quillback.dat", 
                                       "2021_ca_quillback.ctl")))

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)



####------------------------------------------------#
## 0_1_0_updateBio  ----
####------------------------------------------------#

# Update biological relationships

new_name <- "0_1_0_updateBio"
old_name <- "0_0_2_2025setup"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##


### Update M prior ----

maxAge <- 84 #this is the maximum age among BC entries in Claires dataset
m_init <- round(5.4/maxAge, 4)
m_se <- 0.31
m_prior <- 3 #log-normal

mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.15, m_init, round(log(m_init), 2), m_se, m_prior, -2)


### Update growth prior ----

mod$ctl$Growth_Age_for_L1 <- 0

#Growth curve from all individuals. Follows from quillback_growth.R
vb_ests <- read.csv(here("data", "vonb_ests.csv"))
vb_k <- vb_ests[vb_ests$X == "K", "ests"]
vb_linf <- vb_ests[vb_ests$X == "Linf", "ests"]
vb_l0 <- vb_ests[vb_ests$X == "L0", "ests"]
vb_cv0 <- vb_ests[vb_ests$X == "CV0", "ests"]
vb_cv1 <- vb_ests[vb_ests$X == "CV1", "ests"]
vb_prior <- 0

mod$ctl$MG_parms['L_at_Amin_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0, 20, vb_l0, vb_l0, 0, vb_prior, -9)
mod$ctl$MG_parms['L_at_Amax_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(35, 50, vb_linf, vb_linf, 0, vb_prior, -9)
mod$ctl$MG_parms['VonBert_K_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.03, 0.3, vb_k, vb_k, 0, vb_prior, -9)
mod$ctl$MG_parms['CV_young_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.5, vb_cv0, vb_cv0, 0, vb_prior, -9)
mod$ctl$MG_parms['CV_old_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.001, 0.5, vb_cv1, vb_cv1, 0, vb_prior, -9)


### Update LW relationship ----

lw_ests <- read.csv(here("data", "lw_ests.csv"))
lw_a <- lw_ests[lw_ests$sex == "all", "A"]
lw_b <- lw_ests[lw_ests$sex == "all", "B"]
lw_prior <- 0

mod$ctl$MG_parms['Wtlen_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0, 0.1, lw_a, lw_a, 0, lw_prior, -9)
mod$ctl$MG_parms['Wtlen_2_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(2, 4, lw_b, lw_b, 0, lw_prior, -9)


### Update maturity ----

l50_fxn <- 28.96 #-a/b
l50_se <- 0.599 #se of -a/b
slope_fxn <- -0.606 #b (SS3 manual says this must be negative)
slope_se <- 0.121 #se of b
mat_prior <- 6 #making it normal

mod$ctl$MG_parms['Mat50%_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(25, 32, l50_fxn, l50_fxn, l50_se, mat_prior, -9)
mod$ctl$MG_parms['Mat_slope_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(-1.0, 0, slope_fxn, slope_fxn, slope_se, mat_prior, -9)

  
### Update fecundity ----

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
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))



####------------------------------------------------#
## 0_2_0_updateData ----
####------------------------------------------------#

# Update model data for all types of data

new_name <- "0_2_0_updateData"
old_name <- "0_1_0_updateBio"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

#Update fleet information for model, lengths, ages, and indices
mod$dat$Nfleets <- 5
mod$dat$fleetinfo <- rbind(mod$dat$fleetinfo,
                           c("type" = 3, "surveytiming" = -1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_Growth"),
                           c("type" = 3, "surveytiming" = -1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_CCFRP"),
                           c("type" = 3, "surveytiming" = -1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_ROV"))
mod$dat$len_info <- rbind(mod$dat$len_info, #set to match that of the other fleets
                          "CA_Growth" = mod$dat$len_info[1,],
                          "CA_CCFRP" = mod$dat$len_info[1,],
                          "CA_ROV" = mod$dat$len_info[1,])
mod$dat$age_info <- rbind(mod$dat$age_info, #set to match that of the other fleets
                          "CA_Growth" = mod$dat$age_info[1,])
mod$dat$CPUEinfo <- rbind(mod$dat$CPUEinfo,
                          "CA_Growth" = c("fleet" = 3, "units" = 1, "errtype" = 0, "SD_report" = 0),
                          "CA_CCFRP" = c("fleet" = 4, "units" = 0, "errtype" = 0, "SD_report" = 0),
                          "CA_ROV" = c("fleet" = 5, "units" = 1, "errtype" = 0, "SD_report" = 0))

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)



### Update catch time series --------------------------------

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



### Update comps --------------------------------

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
mod$dat$ageerror <- mod$dat$ageerror[, 1:(max(mod$dat$agebin_vector) + 1)]

mod$dat$age_info$combine_M_F <- c(0, 0, 0) #dont compress males with females

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



### Update indices --------------------------------

#CCFRP
ccfrp_index <- read.csv(here("data", "forSS3", "CCFRP_noFN_index_forSS.csv")) %>%
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


mod$dat$CPUE <- dplyr::bind_rows(pr_index, ccfrp_index)


## Add necessary composition data for indices

#CCFRP - these use number of fish as sample sizes
ccfrp.lengths <- read.csv(here("data", "forSS3", "Lcomps_ccfrp_noFN_length_comps_unsexed.csv")) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(ccfrp.lengths) <- names(mod$dat$lencomp)

mod$dat$lencomp <- dplyr::bind_rows(mod$dat$lencomp, ccfrp.lengths)



##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

# r4ss::run(dir = here('models', new_name),
#           exe = here('models/ss3_win.exe'),
#           extras = '-nohess',
#           show_in_console = TRUE, #comment out if you dont want to watch model iterations
#           skipfinished = FALSE)




####------------------------------------------------#
## 0_2_1_updateCatch ----
####------------------------------------------------#

# Update model inputs for catch only

new_name <- "0_2_1_updateCatch"
old_name <- "0_1_0_updateBio"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

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
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)



####------------------------------------------------#
## 0_2_3_updateComps_oldFleets ----
####------------------------------------------------#

# Update model data for only comp data (does not include comps for indices) but
# only do so with existing fleet structure (dont add growth fleet comps)

new_name <- "0_2_2_updateComps_oldFleets"
old_name <- "0_1_0_updateBio"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

#Set up fleet converter to set up any com to fleet=1, rec to fleet=2, growth = fleet=3

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


##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)



####------------------------------------------------#
## 0_2_3_updateComps_withNew ----
####------------------------------------------------#

# Update model data for only comp data (does not include comps for indices) but
# include comp data for new fleet (growth fleet)

new_name <- "0_2_3_updateComps_withNew"
old_name <- "0_1_0_updateBio"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


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

mod$dat$age_info$combine_M_F <- c(0, 0, 0) #dont compress males with females

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


# Now change the selectivity tables....

mod$ctl$size_selex_types <- rbind(mod$ctl$size_selex_types, #set to match that of the other fleets
                                  "CA_Growth" = mod$ctl$size_selex_types[1,])

mod$ctl$age_selex_types <- rbind(mod$ctl$age_selex_types, #set to match that of the other fleets
                                 "CA_Growth" = mod$ctl$age_selex_types[1,])

#...and length selectivity parameterization 
#Set the new fleets selectivity to be the same as the rec fleet for now
mod$ctl$size_selex_parms <- rbind(mod$ctl$size_selex_parms,
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
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)



####------------------------------------------------#
## 0_2_4_updateIndices ----
####------------------------------------------------#

# Update model data for indices (also includes comps for indices)

new_name <- "0_2_4_updateIndices"
old_name <- "0_1_0_updateBio"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

## Add fleets for indices. Keep fleet structure from 0_2_2_updateComps

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
                          "CA_ROV" = c("fleet" = 5, "units" = 1, "errtype" = 0, "SD_report" = 0))

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)


## Add index data

#CCFRP
ccfrp_index <- read.csv(here("data", "forSS3", "CCFRP_noFN_index_forSS.csv")) %>%
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


mod$dat$CPUE <- dplyr::bind_rows(pr_index, ccfrp_index)


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
                              "PHASE" = -1,
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

#CCFRP - these use number of fish as sample sizes
ccfrp.lengths <- read.csv(here("data", "forSS3", "Lcomps_ccfrp_noFN_length_comps_unsexed.csv")) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(ccfrp.lengths) <- names(mod$dat$lencomp)

mod$dat$lencomp <- dplyr::bind_rows(mod$dat$lencomp, ccfrp.lengths)

#Now change the selectivity tables....
mod$ctl$size_selex_types <- rbind(mod$ctl$size_selex_types, #set to match that of the other fleets
                                 "CA_Growth" = mod$ctl$size_selex_types[1,],
                                 "CA_CCFRP" = mod$ctl$size_selex_types[1,],
                                 "CA_ROV" = mod$ctl$size_selex_types[1,])

mod$ctl$age_selex_types <- rbind(mod$ctl$age_selex_types, #set to match that of the other fleets
                                 "CA_Growth" = mod$ctl$age_selex_types[1,],
                                 "CA_CCFRP" = mod$ctl$age_selex_types[1,],
                                 "CA_ROV" = mod$ctl$age_selex_types[1,])

#...and length selectivity parameterization 
#Set the new fleets selectivity to be the same as the rec fleet for now
mod$ctl$size_selex_parms <- rbind(mod$ctl$size_selex_parms,
                                  mod$ctl$size_selex_parms[7:12,],
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
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)



####------------------------------------------------#
## 0_3_1_udpateSelex ----
####------------------------------------------------#

#Update selectivity blocks

new_name <- "0_3_1_blockSelex"
old_name <- "0_2_0_updateData"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

# Add selectivity blocks
mod$ctl$size_selex_types


##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c('name1',
                                                 'name2',
                                                 new_name)))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('label for model 1',
                                     'label for model 2',
                                     'label for new model'),
                    subplots = c(1,3), print = TRUE, plotdir = here('models', new_name))

dev.off()



####------------------------------------------------#
## 0_4_1_updateInputs ----
####------------------------------------------------#

# Update model inputs based on new decisions from earlier versions

new_name <- "0_4_1_updateInputs"
old_name <- "0_2_0_updateData"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               use_ss_new = TRUE,
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

# Change accumulator age
mod$dat$Nages <- 80 #reduce from 90. Probably could set lower, but model runs ok

# Change minimum population bin size
mod$dat$minimum_size <- 2 #L0 is just under 4 so need next smallest bin, which is 2


##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)


####------------------------------------------------#
## 0_4_2_fixWarnings ----
####------------------------------------------------#

# Make changes to fix warnings

new_name <- "0_4_2_fixWarnings"
old_name <- "0_4_1_updateInputs"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               use_ss_new = TRUE,
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

# Change accumulator age
mod$dat$Nages <- 80 #reduce from 90. Probably could set lower, but model runs ok

# Change minimum population bin size
mod$dat$minimum_size <- 2 #L0 is just under 4 so need next smallest bin, which is 2


##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)



##########################################################################################-
#                     ---- 2025 exploration runs ----
##########################################################################################-


