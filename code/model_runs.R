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

source(here('code/selexComp.R'))

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

#plot_sel_all(pp) #uncomment if what to plot selectivities of each fleet

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

ccfrp.lengths <- read.csv(here("data", "forSS3", "Lcomps_ccfrp_noFN_length_comps_unsexed.csv")) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(ccfrp.lengths) <- names(com.lengths)

rov.lengths <- read.csv(here("data", "forSS3", "Lcomps_rov_unsexed_raw_10_50.csv")) %>%
  dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

mod$dat$lencomp <-dplyr::bind_rows(com.lengths, rec.lengths, ccfrp.lengths, rov.lengths)


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


# Now change the selectivity tables....

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



### Update indices --------------------------------

ccfrp_index <- read.csv(here("data", "forSS3", "CCFRP_noFN_index_forSS.csv")) %>%
  dplyr::mutate(fleet = "ccfrp") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

pr_index <- read.csv(here("data", "forSS3", "PR_dockside_index_forSS.csv")) %>%
  dplyr::mutate(fleet = "rec") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

rov_index <- read.csv(here("data", "forSS3", "ROV_index_forSS.csv")) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

mod$dat$CPUE <- dplyr::bind_rows(pr_index, ccfrp_index, rov_index)


# Add q setup for surveys with index data

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
## 0_2_2_updateComps_oldFleets ----
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

#CCFRP
#these use number of fish as sample sizes
ccfrp.lengths <- read.csv(here("data", "forSS3", "Lcomps_ccfrp_noFN_length_comps_unsexed.csv")) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(ccfrp.lengths) <- names(mod$dat$lencomp)

#ROV
rov.lengths <- read.csv(here("data", "forSS3", "Lcomps_rov_unsexed_raw_10_50.csv")) %>%
  dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(rov.lengths) <- names(mod$dat$lencomp)

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
## 0_3_1_updateSelex ----
####------------------------------------------------#

#Update selectivity initialization

new_name <- "0_3_1_updateSelex"
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

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)

# Set up selectivity parameterization according following guidance in
# best practices handbook (section 2.7.3). Handbook can be found at 
# https://github.com/pfmc-assessments/pfmc_assessment_handbook 
# Useful to also use this tool: https://connect.fisheries.noaa.gov/ss3-helper/

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

### P_4
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
## 0_3_2_growthSelex ----
####------------------------------------------------#

#Change growth fleet selectivity to constant

new_name <- "0_3_2_growthSelex"
old_name <- "0_3_1_updateSelex"


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

# Set selectivities of growth fleet to 1 for all lengths (type = 0) 
# and 1 for ages > 0 (type = 10)
mod$ctl$size_selex_types[grep("CA_Growth", rownames(mod$ctl$size_selex_types)),] <-
  c(0, 0, 0, 0)

# Selectivity type 0 does not require selectivity parameters so remove
mod$ctl$size_selex_parms <- mod$ctl$size_selex_parms[
  -grep("CA_Growth", rownames(mod$ctl$size_selex_parms)),]


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
## 0_3_3_udpateSelexBlocks ----
####------------------------------------------------#

#Update selectivity blocks

new_name <- "0_3_3_updateSelexBlocks"
old_name <- "0_3_2_growthSelex"


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
## 0_4_1_addAgeErr ----
####------------------------------------------------#

#Add new ageing error matrix

new_name <- "0_4_1_addAgeErr"
old_name <- "0_3_3_updateSelexBlocks"


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

# Add ageing error matrix
ageerr <- read.csv(here("data", "forSS3", "Ageing_error_B01S11_forSS.csv"))

age_ind <- grep(mod$dat$Nages, colnames(ageerr))
mod$dat$ageerror <- ageerr[,1:age_ind]


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
## 0_4_2_addVarAdj ----
####------------------------------------------------#

#Add variance adjustment factors for comp data and reset all to one

new_name <- "0_4_2_addVarAdj"
old_name <- "0_4_1_addAgeErr"


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

# Add variance adjustment for each comp data set
varadj_len <- data.frame("factor" = 4, 
                         fleet = unique(mod$dat$lencomp$fleet), 
                         value = 1,
                         row.names = paste0("Len_", fleet.converter[unique(mod$dat$lencomp$fleet), "fleetname"]))
varadj_age <- data.frame("factor" = 5,
                         fleet = unique(mod$dat$agecomp$fleet), 
                         value = 1,
                         row.names = paste0("Age_", fleet.converter[unique(mod$dat$agecomp$fleet), "fleetname"]))
mod$ctl$Variance_adjustment_list <- dplyr::bind_rows(varadj_len, varadj_age)


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
## 0_5_1_fixWarnings ----
####------------------------------------------------#

# Make changes to fix warnings

new_name <- "0_5_1_fixWarnings"
old_name <- "0_4_2_addVarAdj"


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
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)




####------------------------------------------------#
## 0_5_2_updateInputs ----
####------------------------------------------------#

# Update model inputs based on errors or inaccuracies of decisions from earlier models

new_name <- "0_5_2_updateInputs"
old_name <- "0_5_1_fixWarnings"


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

# # Change accumulator age
# mod$dat$Nages <- 80 #reduce from 90. Probably could set lower, but model runs ok
# 
# #WOULD NEED TO CHANGE AGEING ERROR AT THIS POINT TOO
# 
# # Change minimum population bin size
# mod$dat$minimum_size <- 2 #L0 is just under 4 so need next smallest bin, which is 2
# 
# #MAY NOT NEED TO DO THIS GIVEN NEW GROWTH PATTERN

# Change ROV units from biomass to numbers
mod$dat$CPUEinfo["CA_ROV", "units"] <- 0

# Allow growth to be estimated in the model.
# Currently not estimating CV parameters
mod$ctl$MG_parms[grep("L_at|VonBert", rownames(mod$ctl$MG_parms)), "PHASE"] <- 3



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




##########################################################################################-
#                     ---- 2025 exploration runs ----
##########################################################################################-

# Fits to comps are pretty poor. Explore ways to improve these

####------------------------------------------------#
## 1_0_1_ccfrpSelexLogistic ----
####------------------------------------------------#

# Explore ways to improve comp fits
# CCFRP is way off and not estimating well. Switch to logistic.
# Mirroring rec is an option but the comps dont align very well
# and there wouldn't be blocking structure, so not doing at this time. 

new_name <- "1_0_1_ccfrpSelexLogistic"
old_name <- "0_5_2_fixWarnings"


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

# Fix parameter 4 to make selectivity logistic
mod$ctl$size_selex_parms["SizeSel_P_4_CA_CCFRP(4)", c("HI", "INIT", "PHASE")] <-
  c(20, 15, -9)


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
## 1_0_2_upweightCCFRP ----
####------------------------------------------------#

# Changing CCFRP to logistic did nothing. Try upweighting a LOT with var_adj

new_name <- "1_0_2_upweightCCFRP"
old_name <- "1_0_1_ccfrpSelexLogistic"


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

# Upweight
mod$ctl$Variance_adjustment_list[which(mod$ctl$Variance_adjustment_list$fleet == 4 &
                                         mod$ctl$Variance_adjustment_list$fleet == 4), ] <-
  c(4, 4, 10)


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
## 1_0_3_upweightCCFRPlambda ----
####------------------------------------------------#

# Changing CCFRP to logistic did nothing. Try upweighting a LOT with lambda 

new_name <- "1_0_3_upweightCCFRPlambda"
old_name <- "1_0_1_ccfrpSelexLogistic"


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

# Upweight
mod$ctl$N_lambdas <- 1
mod$ctl$lambdas <- data.frame("like_comp" = 4, 
                             "fleet" = 4,
                             "phase" = 1,
                             "value" = 10,
                             "sizefreq_method" = 1)


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

#Upweighting the lambda doesn't do as well as changing variance model run. 



####------------------------------------------------#
## 1_0_4_reweight ----
####------------------------------------------------#

# Try a single reweight to see if that gets the model in a reasonable place
# My thought is that commercial and recreational comps are dominating the fits

new_name <- "1_0_4_reweight"
old_name <- "1_0_1_ccfrpSelexLogistic"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models',old_name),"Report.sso"),
          to = file.path(here('models',new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"CompReport.sso"),
          to = file.path(here('models',new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"warning.sso"),
          to = file.path(here('models',new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"covar.sso"),
          to = file.path(here('models',new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

pp <- SS_output(here('models',new_name))
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = 0, 
                       extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

colnames(dw)[1] = "factor"
new_var_adj <- dplyr::left_join(mod$ctl$Variance_adjustment_list, dw,
                                by = dplyr::join_by(factor, fleet))
mod$ctl$Variance_adjustment_list$value <-  new_var_adj$New_Var_adj


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


#Still really poor. Growth seems to be a contributing factor, but although
#commercial and recreational look somewhat reasonable, selectivity for CCFRP
#is still really poor. 


####------------------------------------------------#
## 1_0_5_unfixQ ----
####------------------------------------------------#

# Catchabilities are fixed at 1. Relax these are rerun. See if that improves

new_name <- "1_0_5_unfixQ"
old_name <- "1_0_1_ccfrpSelexLogistic"


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

# Estimate Q parameters
mod$ctl$Q_parms[grep("CA_Rec|CA_CCFRP", rownames(mod$ctl$Q_parms)), "PHASE"] <- 2

#Curious about whether there should be blocks around q for rec fleet.
#I dont think so because blocks in selectivity with account for changes in 
#size structure. Block the index means there is something that changed beyond
#what happended in selectivity changes. 


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

#Allowing the indices for both CCFRP and Rec to be fit better and greatly improves 
#estimation for selectivity of all fleets.


####------------------------------------------------#
## 1_0_6_unfixQ_float ----
####------------------------------------------------#

# Catchabilities are fixed at 1. Relax these are rerun. See if that improves

new_name <- "1_0_6_unfixQ_float"
old_name <- "1_0_5_unfixQ"


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

# Float q for previously estimated Q parameters
mod$ctl$Q_options[mod$ctl$Q_options$fleet %in% c(2, 4), "float"] <- 1

# Fix previously estimated Q parameters
mod$ctl$Q_parms[grep("CA_Rec|CA_CCFRP", rownames(mod$ctl$Q_parms)), "PHASE"] <- -2


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

#Likelihood is exactly the same when float Q vs fixing it. I like have an active
#parameter so suggest we do the estimate Q option


####------------------------------------------------#
## 1_0_7_estQ_ROV ----
####------------------------------------------------#

# Catchabilities are fixed at 1 for ROV. Estimate using active parameter

new_name <- "1_0_7_estQ_ROV"
old_name <- "1_0_5_unfixQ"


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

# Turn on estimation for ROV
mod$ctl$Q_parms[grep("ROV", rownames(mod$ctl$Q_parms)), "PHASE"] <- 2


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
## 1_0_8_ROVselexLogistic ----
####------------------------------------------------#

# Explore alternative ROV selectivities - assume logistic

new_name <- "1_0_8_ROVselexLogistic"
old_name <- "1_0_7_estQ_ROV"


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

# Fix parameter 4 to make selectivity logistic
mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
                                   grep("4", rownames(mod$ctl$size_selex_parms))), 
                         c("HI", "INIT", "PHASE")] <- c(20, 15, -9)


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
## 1_0_9_ROVselexFixed1 ----
####------------------------------------------------#

# Explore alternative ROV selectivities - assume fixed at 1

new_name <- "1_0_9_ROVselexFixed1"
old_name <- "1_0_7_estQ_ROV"


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

# Set selectivity of ROV fleet to 1 for all lengths (type = 0) and 1 for ages > 0 (type = 10)
mod$ctl$size_selex_types[grep("CA_ROV", rownames(mod$ctl$size_selex_types)),] <-
  c(0, 0, 0, 0)

# Selectivity type 0 does not require selectivity parameters so remove
mod$ctl$size_selex_parms <- mod$ctl$size_selex_parms[
  -grep("CA_ROV", rownames(mod$ctl$size_selex_parms)),]


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

plot_sel_all(pp)


####------------------------------------------------#
## 1_0_10_growthSelex ----
####------------------------------------------------#

# Set growth fleet selectivity for ages to be type 0 (age 0 selex = 1)

new_name <- "1_0_10_growthSelex"
old_name <- "1_0_7_estQ_ROV"


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

mod$ctl$age_selex_types["CA_Growth", "Pattern"] <- 0


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

plot_sel_all(pp)



####------------------------------------------------#
## 1_1_1_fixGrowthROVlogistic ----
####------------------------------------------------#

#fix growth, now using Diana's age 0 lengths    
# starting model 108 since Melissa forgot to commit older code

 new_name <- "1_1_1_fixGrowthROVlogistic"  
 old_name <- "1_0_8_ROVselexLogistic"

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

mod$ctl$MG_parms$INIT[2] <-  3.986
mod$ctl$MG_parms$INIT[3] <- 41.152
mod$ctl$MG_parms$INIT[4] <-  0.178
mod$ctl$MG_parms$INIT[5] <-  0.207
mod$ctl$MG_parms$INIT[6] <-  0.063
mod$ctl$MG_parms$PRIOR[2:6] <- mod$ctl$MG_parms$INIT[2:6]
#negative phase 
mod$ctl$MG_parms$PHASE[2:6] <- -9

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


#Fits look better.  Growth looks reasonable
#Length fits still off


####------------------------------------------------#
## 1_1_2_estL2estK ----
####------------------------------------------------#


 new_name <- "1_1_2_estL2estK"  
 old_name <- "1_1_1_fixGrowthROVlogistic"

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
#phase 2 for L2 and K 
mod$ctl$MG_parms$PHASE[3:4] <- 2

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

#L2 now at 44.2 and k estimated at .12

####------------------------------------------------#
## 1_1_3_estL2estKCVs ----
####------------------------------------------------#
 
#starting model 108 since Melissa forgot to commit older code

 new_name <- "1_1_3_estL2estKCVs"  
 old_name <- "1_1_2_estL2estK"

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
#phase 2 for L2 and K 
mod$ctl$MG_parms$PHASE[3:6] <- 2
#$triple check changes
aa <- mod$ctl$MG_parms
View(aa)
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

#L2 now at 44.2 and k estimated at .12
#CVs for L1 and L2 look ok


####------------------------------------------------#
## 1_1_4_estAll Growth ----
####------------------------------------------------#

 new_name <- "1_1_4_estAllGrowth"  
 old_name <- "1_1_3_estL2estKCVs"

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
#phase 2 for L2 and K 
mod$ctl$MG_parms$PHASE[2:6] <- 2
#triple check changes
aa <- mod$ctl$MG_parms
View(aa)
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

#looks ok, ROV selex off


####------------------------------------------------#
## 1_1_5_fixL0someCAAL ----
####------------------------------------------------#

 new_name <- "1_1_5_fixL0someCAAL"  
 old_name <- "1_1_4_estAllGrowth"

#Copy inputs
copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))

#Make Changes
# phase -9 for L0
mod$ctl$MG_parms$PHASE[2] <- -2
#$triple check changes
aa <- mod$ctl$MG_parms
View(aa)

#Look at CAAL sample sizes
caal <- mod$dat$agecomp
View(caal)

#get the year and fleet combos where there are fewer than 30 total ages
caal_sum <- caal %>%
group_by(year, fleet) %>%
summarise(sum_ages = sum(Nsamp)) %>%
filter(sum_ages <30)

# set rows to zero where year and fleet are in the caal_sum table
mod$dat$agecomp <-  mod$dat$agecomp %>%
mutate(year = case_when( 
        year %in% c(2007,2011,2012,2023,2024) & fleet == 1 ~ -year,
        year %in% c(1985,2004,2006,2007,2014,2019,2020) & fleet == 3 ~ -year,
        T ~ year))

#Output files and run
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

#removing the caal with small samples sizes helps


####------------------------------------------------#
## 1_1_6_L0to4 ----
####------------------------------------------------#

 new_name <- "1_1_6_L0to4"
 old_name <- "1_1_5_fixL0someCAAL"  

#Copy inputs
copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))

#Make Changes
#Change params L0 to 4
mod$ctl$MG_parms$INIT[2] <- 4


#Output files and run
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
plot_sel_all(pp)
#Effectively does nothing, just removes the warning about the lower length bin


####------------------------------------------------#
## 1_1_7_FirstMatureAge ----
####------------------------------------------------#

 new_name <- "1_1_7_FirstMatureAge"
 old_name <- "1_1_6_L0to4" 

#Copy inputs
copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))

#Change first mature age to 2
mod$ctl$First_Mature_Age <- 2


#Output files and run
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
plot_sel_all(pp)
#Does nothing


####------------------------------------------------#
## 1_1_8_1cmPopBins ----
####------------------------------------------------#

 new_name <- "1_1_8_1cmPopBins"
 old_name <- "1_1_6_L0to4" 

#Copy inputs
copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))

#Change population size bins to 1cm
mod$dat$binwidth <- 1



#Output files and run
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
plot_sel_all(pp)
#Does nothing


####------------------------------------------------#
## 1_1_9_Month1 ----
####------------------------------------------------#

new_name <- "1_1_9_month1"
old_name <- "1_1_6_L0to4" 

#Copy inputs
copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))

#Set length/age/index data to come from month 1 as opposed to month 7
mod$dat$lencomp$month <- 1
mod$dat$agecomp$month <- 1
mod$dat$CPUE$month <- 1


#Output files and run
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

plot_sel_all(pp)


####------------------------------------------------#
## 1_1_10_exponentialDecay0.2 ----
####------------------------------------------------#

#Set exponential decay from 0.01 to 0.2

new_name <- "1_1_10_exponentialDecay0.2"
old_name <- "1_1_6_L0to4" 

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

mod$ctl$Exp_Decay <- 0.2


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

plot_sel_all(pp)


####------------------------------------------------#
## 1_1_11_lowerCatchSE ----
####------------------------------------------------#

#To see if more fixed catch (decreasing catch se) changes results

new_name <- "1_1_11_lowerCatchSE"
old_name <- "1_1_6_L0to4" 

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

mod$dat$catch$catch_se <- 0.01


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

plot_sel_all(pp)


####------------------------------------------------#
## 1_1_12_L1age1fixedto8 ----
####------------------------------------------------#

#Change L1 to be length at age 1 (not 0) and set fixed parameter at 8cm

new_name <- "1_1_12_L1age1fixedto8"
old_name <- "1_1_6_L0to4" 

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

mod$ctl$MG_parms$INIT[2] <- 8
mod$ctl$Growth_Age_for_L1 <- 1

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

plot_sel_all(pp)
#Doesn't look like anything changes - strange....tried cranking length at age 1 to 16 a
#and that's where you see a change


####------------------------------------------------#
## 1_1_13_L1age1EstAllGrowth ----
####------------------------------------------------#

#Change L1 to be length at age 1 (not 0) but estimate it with init set to 8

new_name <- "1_1_13_L1age1EstAllGrowth"
old_name <- "1_1_6_L0to4" 

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

mod$ctl$MG_parms$INIT[2] <- 8
mod$ctl$MG_parms$PHASE[2] <- 2
mod$ctl$Growth_Age_for_L1 <- 1

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

plot_sel_all(pp)


####------------------------------------------------#
## 2_0_1_updateData ----
####------------------------------------------------#

#Updated data from the ROV and added updated 2024 commercial discard estimates

new_name <- "2_0_1_updateData"
old_name <- "1_1_6_L0to4" 

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

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)

#Update 2024 catches with newest estimates from observer program

catches <- read.csv(here("data", "CAquillback_total_removals.csv"))

mod$dat$catch[mod$dat$catch == 2024 & mod$dat$catch$fleet == 
                fleet.converter[fleet.converter$fleet == "com", "fleet_num"], "catch"] <- 
  catches[catches$Year == 2024, "com_tot"]


#Update ROV indices and length data with newest data

#index
rov_index <- read.csv(here("data", "forSS3", "ROV_index_forSS.csv")) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

mod$dat$CPUE[which(mod$dat$CPUE$index %in% 
                     fleet.converter[fleet.converter$fleet == "rov", "fleet_num"]),] <- 
  rov_index

#lengths
rov_lengths <- read.csv(here("data", "forSS3", "Lcomps_rov_unsexed_raw_10_50.csv")) %>%
  dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(rov.lengths) <- names(mod$dat$lencomp)

mod$dat$lencomp[which(mod$dat$lencomp$fleet %in% 
                     fleet.converter[fleet.converter$fleet == "rov", "fleet_num"]),] <- 
  rov_lengths


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_1_1_reweight ----
####------------------------------------------------#

#Reweight model 201

new_name <- "2_1_1_reweight"
old_name <- "2_0_1_updateData" 


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models',old_name),"Report.sso"),
          to = file.path(here('models',new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"CompReport.sso"),
          to = file.path(here('models',new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"warning.sso"),
          to = file.path(here('models',new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"covar.sso"),
          to = file.path(here('models',new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

pp <- SS_output(here('models',new_name))
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = 0, 
                       extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

colnames(dw)[1] = "factor"
new_var_adj <- dplyr::left_join(mod$ctl$Variance_adjustment_list, dw,
                                by = dplyr::join_by(factor, fleet))
mod$ctl$Variance_adjustment_list$value <-  new_var_adj$New_Var_adj


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_1_2_ROVselexFix1 ----
####------------------------------------------------#

#Set ROV selectivity to 1

new_name <- "2_1_2_ROVselexFix1"
old_name <- "2_1_1_reweight" 

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

# Set selectivity of ROV fleet to 1 for all lengths (type = 0) and 1 for ages > 0 (type = 10)
mod$ctl$size_selex_types[grep("CA_ROV", rownames(mod$ctl$size_selex_types)),] <-
  c(0, 0, 0, 0)

# Selectivity type 0 does not require selectivity parameters so remove
mod$ctl$size_selex_parms <- mod$ctl$size_selex_parms[
  -grep("CA_ROV", rownames(mod$ctl$size_selex_parms)),]


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

plot_sel_all(pp)

##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("2_1_1_reweight" ,
                                                 "2_1_2_ROVselexFix1")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('2_1_1_reweight201',
                                     '2_1_2: Selex 1 at all lengths'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))


####------------------------------------------------#
## 2_2_1_updateData_ages ----
####------------------------------------------------#

#Updated data from the most recent age cleanup
#This involves new CCFRP comps (and now split out from growth fleet) and 
#growth fleet comps, as well as updated growth initial values and 

new_name <- "2_2_1_updateData_ages"
old_name <- "1_1_13_L1age1EstAllGrowth" 

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

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)


# Update growth inits
vb_ests <- read.csv(here("data", "vonb_ests_withAge0.csv"))

mod$ctl$MG_parms["L_at_Amax_Fem_GP_1", c("INIT", "PRIOR")] <- 
  vb_ests[vb_ests$X == "Linf", "ests"]
mod$ctl$MG_parms["VonBert_K_Fem_GP_1", c("INIT", "PRIOR")] <- 
  vb_ests[vb_ests$X == "K", "ests"]
mod$ctl$MG_parms["CV_young_Fem_GP_1", c("INIT", "PRIOR")] <- 
  vb_ests[vb_ests$X == "CV0", "ests"]
mod$ctl$MG_parms["CV_old_Fem_GP_1", c("INIT", "PRIOR")] <- 
  vb_ests[vb_ests$X == "CV1", "ests"]


# Update comps

#Commercial comps haven't changed but repulling to keep in all data
com.CAAL <- read.csv(here("data", "forSS3", "CAAL_PacFIN_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

growth.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_nonccfrp_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = "growth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

ccfrp.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_ccfrp_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = "ccfrp") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

new.caal <- dplyr::bind_rows(com.CAAL, growth.CAAL, ccfrp.CAAL)

mod$dat$agecomp <- new.caal


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_2_2_dropSomeCAAL ----
####------------------------------------------------#

#Revisit model 115 choice to remove CAAL < 30 samples per year

new_name <- "2_2_2_dropSomeCAAL"
old_name <- "2_2_1_updateData_ages" 

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

#If want to revisit choice in model 115 to negative year when fewer than 30 total ages
new.caal <- mod$dat$agecomp

small_N <- new.caal %>%
  group_by(year, fleet) %>%
  summarise(sum_ages = sum(Nsamp)) %>%
  filter(sum_ages < 30) %>%
  data.frame()

for(i in unique(small_N$fleet)){
  new.caal <- new.caal %>%
    dplyr::mutate(year = case_when(
      year %in% small_N[small_N$fleet == i, "year"] & fleet == i ~ -year,
      T ~ year))
}

mod$dat$agecomp <- new.caal


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_2_3_combineGrowth_CCFRP ----
####------------------------------------------------#

#Recombine CAAL comps for CCFRP and the growth fleet and remove small samples

new_name <- "2_2_3_combineGrowth_CCFRP"
old_name <- "1_1_13_L1age1EstAllGrowth" 

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

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)


# Update growth inits
vb_ests <- read.csv(here("data", "vonb_ests_withAge0.csv"))

mod$ctl$MG_parms["L_at_Amax_Fem_GP_1", c("INIT", "PRIOR")] <- 
  vb_ests[vb_ests$X == "Linf", "ests"]
mod$ctl$MG_parms["VonBert_K_Fem_GP_1", c("INIT", "PRIOR")] <- 
  vb_ests[vb_ests$X == "K", "ests"]
mod$ctl$MG_parms["CV_young_Fem_GP_1", c("INIT", "PRIOR")] <- 
  vb_ests[vb_ests$X == "CV0", "ests"]
mod$ctl$MG_parms["CV_old_Fem_GP_1", c("INIT", "PRIOR")] <- 
  vb_ests[vb_ests$X == "CV1", "ests"]


# Update comps

#Commercial comps haven't changed but repulling to keep in all data
com.CAAL <- read.csv(here("data", "forSS3", "CAAL_PacFIN_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

#Use growth data that includes CCFRP
growth.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_all_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = "growth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()

mod$dat$agecomp <- dplyr::bind_rows(com.CAAL, growth.CAAL)


#Revisit choice in model 115 to negative year when fewer than 30 total ages
new.caal <- mod$dat$agecomp

small_N <- new.caal %>%
  group_by(year, fleet) %>%
  summarise(sum_ages = sum(input_n)) %>%
  filter(sum_ages < 30) %>%
  data.frame()

for(i in unique(small_N$fleet)){
  new.caal <- new.caal %>%
    dplyr::mutate(year = case_when(
      year %in% small_N[small_N$fleet == i, "year"] & fleet == i ~ -year,
      T ~ year))
}

mod$dat$agecomp <- new.caal


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_3_1_reweight223 ----
####------------------------------------------------#

#Reweight the model with updated age data

new_name <- "2_3_1_reweight223"
old_name <- "2_2_3_combineGrowth_CCFRP" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models',old_name),"Report.sso"),
          to = file.path(here('models',new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"CompReport.sso"),
          to = file.path(here('models',new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"warning.sso"),
          to = file.path(here('models',new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"covar.sso"),
          to = file.path(here('models',new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

pp <- SS_output(here('models',new_name))
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = 0, 
                       extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

colnames(dw)[1] = "factor"
new_var_adj <- dplyr::left_join(mod$ctl$Variance_adjustment_list, dw,
                                by = dplyr::join_by(factor, fleet))
mod$ctl$Variance_adjustment_list$value <-  new_var_adj$New_Var_adj


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

plot_sel_all(pp)



####------------------------------------------------#
## 2_3_2_recdevOption2----
####------------------------------------------------#

#Set recdev option not to sum to 0 (set to 2)

new_name <- "2_3_2_recdevOption2"
old_name <- "2_3_1_reweight223" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##
mod$ctl$do_recdev <- 2

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

plot_sel_all(pp)

#There are not major changes having the recdevs set to option 2, no sum to zero constraint


####------------------------------------------------#
## 2_3_3_Nages70----
####------------------------------------------------#

#Change the accumulator age to 70
#keeps recdevs at option 2

new_name <- "2_3_3_Nages70"
old_name <- "2_3_2_recdevOption2" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##
mod$dat$Nages <- 70 # to correspond with maximum age

mod$dat$ageerror <- mod$dat$ageerror[,1:71]
#ageing error matrix


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

plot_sel_all(pp)

#Could probably go lower for the accumultator age, but leave here for now


####------------------------------------------------#
## 2_3_4_rmTimeBlocks----
####------------------------------------------------#

#Remove all selectivity time blocks

new_name <- "2_3_4_rmTimeBlocks"
old_name <- "2_3_3_Nages70" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##
mod$ctl$N_Block_Designs <- 0
#mod$ctl$N_Block_Designs <- paste0("#",mod$ctl$blocks_per_pattern)
mod$ctl$size_selex_parms$Block  = 0
mod$ctl$size_selex_parms$Block_Fxn  = 0
mod$ctl$size_selex_parms_tv <- 0
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

plot_sel_all(pp)

#Time block doesn't seem to have an effect; as Brian already knows from 2021


####------------------------------------------------#
## 2_3_5_growthParm3----
####------------------------------------------------#

#Set growth parameters to phase 3, as no parameter currently has that phase

new_name <- "2_3_5_growthParm3"
old_name <- "2_3_1_reweight223" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$MG_parms[mod$ctl$MG_parms$PHASE == 2, "PHASE"] <- 3


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

plot_sel_all(pp)

#Not difference in results but does improve convergence. We should do this. 


####------------------------------------------------#
## 2_3_6_noRecDevs----
####------------------------------------------------#

#Turn off recdevs to see the effect

new_name <- "2_3_6_noRecDevs"
old_name <- "2_3_1_reweight223" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$do_recdev <- 0


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_3_7_useSteepness----
####------------------------------------------------#

#Use steepness in equilibrium calculations

new_name <- "2_3_7_useSteepness"
old_name <- "2_3_1_reweight223" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$Use_steep_init_equi <- 1


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_3_8_removeSparseData_5 ----
####------------------------------------------------#

#Remove sparse age and length comps (input sample size < 5)

new_name <- "2_3_8_removeSparseData_5"
old_name <- "2_3_1_reweight223" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$dat$lencomp[which(mod$dat$lencomp$Nsamp <= 5), "year"] <-
  -mod$dat$lencomp[which(mod$dat$lencomp$Nsamp <= 5), "year"]


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_3_9_ROVFixSelex1L8 ----
####------------------------------------------------#

#This model uses ROV length selectivity 11 and sets population bins to 1cm

new_name <- "2_3_9_ROVFixSelex1L8"
old_name <- "2_3_8_removeSparseData_5"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##

#Change population size bins to 1cm
mod$dat$binwidth <- 1

# Set selectivity of ROV fleet to 1 for all lengths >= 8 (5th bin)
mod$ctl$size_selex_types[grep("CA_ROV", rownames(mod$ctl$size_selex_types)),] <- c(11, 0, 0, 0)


# Need two parameters - remove the last 4
mod$ctl$size_selex_parms <- mod$ctl$size_selex_parms[-grep("CA_ROV", rownames(mod$ctl$size_selex_parms))[3:6],]

#change values for first two
mod$ctl$size_selex_parms[grep("CA_ROV", rownames(mod$ctl$size_selex_parms))[1],c("LO", "HI", "INIT", "PHASE")] <- c(1, 6, 5, -9)
mod$ctl$size_selex_parms[grep("CA_ROV", rownames(mod$ctl$size_selex_parms))[2],c("LO", "HI", "INIT", "PHASE")] <- c(60, 60, 60, -9)

aa <- mod$ctl$size_selex_parms
View(aa)


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

plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("2_3_1_reweight223",
                                                 "2_3_8_removeSparseData_5",
                                                 "2_3_9_ROVFixSelex1L8")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('2_3_1_reweight223',
                                     '2_3_8: Remove sparse data',
                                     '2_3_9: Rov selex to 11, and pop bins at 1'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))


####------------------------------------------------#
## 2_3_10_PopSizeBin1cm ----
####------------------------------------------------#

#Try just setting pop bins size to 1 cm

new_name <- "2_3_10_PopSizeBin1cm"
old_name <- "2_3_8_removeSparseData_5"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##

#Change population size bins to 1cm
mod$dat$binwidth <- 1

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

plot_sel_all(pp)


####------------------------------------------------#
## 2_3_11_PopSizeBin1cmPlusROVSelex ----
####------------------------------------------------#

#This model uses ROV length selectivity 11

new_name <- "2_3_11_PopSizeBin1cmPlusROVSelex"
old_name <- "2_3_10_PopSizeBin1cm"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##

# Set selectivity of ROV fleet to 1 for all lengths >= 13 (10th bin)
mod$ctl$size_selex_types[grep("CA_ROV", rownames(mod$ctl$size_selex_types)),] <- c(11, 0, 0, 0)


# Need two parameters - remove the last 4
mod$ctl$size_selex_parms <- mod$ctl$size_selex_parms[-grep("CA_ROV", rownames(mod$ctl$size_selex_parms))[3:6],]

#change values for first two
mod$ctl$size_selex_parms[grep("CA_ROV", rownames(mod$ctl$size_selex_parms))[1],c("LO", "HI", "INIT", "PHASE")] <- c(1, 6, 10, -9)
mod$ctl$size_selex_parms[grep("CA_ROV", rownames(mod$ctl$size_selex_parms))[2],c("LO", "HI", "INIT", "PHASE")] <- c(60, 60, 60, -9)


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_3_12_ROVFixSelexL8 ----
####------------------------------------------------#

#Similar to model 239 but keep population bins at 2

new_name <- "2_3_12_ROVFixSelexL8"
old_name <- "2_3_8_removeSparseData_5"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##

# Set selectivity of ROV fleet to 1 for all lengths >= 8 (3rd bin)
mod$ctl$size_selex_types[grep("CA_ROV", rownames(mod$ctl$size_selex_types)),] <- c(11, 0, 0, 0)

# Need two parameters - remove the last 4
mod$ctl$size_selex_parms <- mod$ctl$size_selex_parms[-grep("CA_ROV", rownames(mod$ctl$size_selex_parms))[3:6],]

#change values for first two
mod$ctl$size_selex_parms[grep("CA_ROV", rownames(mod$ctl$size_selex_parms))[1],c("LO", "HI", "INIT", "PHASE")] <- c(1, 6, 3, -9)
mod$ctl$size_selex_parms[grep("CA_ROV", rownames(mod$ctl$size_selex_parms))[2],c("LO", "HI", "INIT", "PHASE")] <- c(1, 28, 28, -9)


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

plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("2_3_1_reweight223",
                                                 "2_3_8_removeSparseData_5",
                                                 "2_3_9_ROVFixSelex1L8",
                                                 "2_3_10_PopSizeBin1cm",
                                                 "2_3_11_PopSizeBin1cmPlusROVSelex",
                                                 "2_3_12_ROVFixSelexL8")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('2_3_1_reweight223',
                                     '2_3_8: Remove sparse data',
                                     '2_3_9: Rov selex to 11 (8cm), and pop bins at 1',
                                     '2_3_10: Pop bins at 1',
                                     '2_3_11: Rov selex to 11 (13cm), Pop bins at 1',
                                     '2_3_12: Rov selex to 11 (8cm)'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))


####------------------------------------------------#
## 2_4_1_FAA_confidential ----
####------------------------------------------------#

#These data are confidential. Putting in a separate folder and updating gitignore
#to ensure not pushed. 

#Try fleets as areas set up to see if improve fits to data.
#Start from unweighted version given these changes would really change 231 weights
#but then reweight

new_name <- "2_4_1_FAA_confidential"
old_name <- "2_2_3_combineGrowth_CCFRP" 

##
#Copy inputs
##

dir.create(here("models", "_confidential_FAA_runs_noShare"))

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', "_confidential_FAA_runs_noShare", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_confidential_FAA_runs_noShare", new_name))


##
#Make Changes
##

#Set growth parameter to three based on model 235
mod$ctl$MG_parms[mod$ctl$MG_parms$PHASE == 2, "PHASE"] <- 3


#Update fleet information for model, lengths, ages, and indices
mod$dat$Nfleets <- 7
mod$dat$fleetinfo <- rbind(c("type" = 1, "surveytiming" = -1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_Commercial_North"),
                           mod$dat$fleetinfo[1,],
                           c("type" = 1, "surveytiming" = -1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_Recreational_North"),
                           mod$dat$fleetinfo[-1,])
mod$dat$fleetinfo$fleetname[c(2,4)] <- paste0(mod$dat$fleetinfo$fleetname[c(2,4)], "_South")

mod$dat$len_info <- rbind("CA_Commercial_North" = mod$dat$len_info[1,],
                          mod$dat$len_info[1,],
                          "CA_Recreational_North" = mod$dat$len_info[2,],
                          mod$dat$len_info[-1,])
rownames(mod$dat$len_info)[c(2,4)] <- paste0(rownames(mod$dat$len_info)[c(2,4)], "_South")

mod$dat$age_info <- rbind("CA_Commercial_North" = mod$dat$age_info[1,],
                          mod$dat$age_info[1,],
                          "CA_Recreational_North" = mod$dat$age_info[2,],
                          mod$dat$age_info[-1,])
rownames(mod$dat$age_info)[c(2,4)] <- paste0(rownames(mod$dat$age_info)[c(2,4)], "_South")


mod$dat$CPUEinfo <- rbind("CA_Commercial_North" = mod$dat$CPUEinfo[1,],
                          mod$dat$CPUEinfo[1,],
                          "CA_Recreational_North" =  mod$dat$CPUEinfo[2,],
                          mod$dat$CPUEinfo[-1,])
rownames(mod$dat$CPUEinfo)[c(2,4)] <- paste0(rownames(mod$dat$CPUEinfo)[c(2,4)], "_South")
mod$dat$CPUEinfo$fleet <- c(1:7)

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "com", "rec", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(area = c("North", "South", "North", "South", "All", "All", "All")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5, 6, 7)) %>%
  dplyr::mutate(joint = paste0(fleet, area)) %>%
  dplyr::select(fleetname, fleet, area, joint, fleet_num)



### Update catch time series --------------------------------

catches <- read.csv(here("data", "confidential_noShare", "CAquillback_total_removals_faa.csv"))
catches[is.na(catches)] <- 0

updated.catch.df <- catches %>%
  dplyr::select(c(Year, names(catches[grep("tot", names(catches))]))) %>%
  tidyr::pivot_longer(cols = -Year, names_to = c('fleet', 'type', 'area'), values_to = 'catch', 
                      names_sep = '_') %>% #ideally I want to separate by second hyphen but this is a workaround
  dplyr::mutate(joint = paste0(fleet, area)) %>%
  dplyr::left_join(fleet.converter %>% dplyr::select(joint, fleet_num), by = c("joint" = "joint")) %>%
  dplyr::mutate(seas = 1, 
                catch_se = 0.05) %>%
  dplyr::select(year = Year, seas, fleet = fleet_num, catch, catch_se) %>%
  dplyr::arrange(fleet, year) %>%
  as.data.frame()

mod$dat$catch <- updated.catch.df



### Update comps --------------------------------

# Length comps

com.lengths <- read.csv(here("data", "forSS3", "Lcomps_PacFIN_FAA_unsexed_expanded_10_50.csv")) %>%
  dplyr::mutate(fleet = paste0("com", fleet)) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleet), by = c("fleet" = "joint"))$fleet_num) %>%
  as.data.frame()

rec.lengths <- read.csv(here("data", "forSS3", "Lcomps_recreational_FAA_unsexed_raw_10_50.csv")) %>%
  dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = gsub('_', "", fleet)) %>%
  dplyr::mutate(fleet = dplyr::left_join(., 
       dplyr::select(fleet.converter %>% dplyr::mutate(joint = tolower(joint)), -fleet), 
       by = c("fleet" = "joint"))$fleet_num) %>%
  as.data.frame()

#Update previous fleet number for unupdated fleets, and then add new FAA fleets
noFAA.lengths <- mod$dat$lencomp[-which(mod$dat$lencomp$fleet %in% c(1, 2)), ]
noFAA.lengths$fleet <- noFAA.lengths$fleet + 2
names(noFAA.lengths) <- names(com.lengths)

mod$dat$lencomp <- dplyr::bind_rows(com.lengths, rec.lengths, noFAA.lengths)


# Age comps

mod$dat$lbin_method <- 2 #this is the current value, but useful to set.
#Requires length bins to be set to the length bin index, so need to change CAAL
#to reflect bin index. Could set this to 3 and keep length bins as is (i.e. as lengths)

com.CAAL <- read.csv(here("data", "forSS3", "CAAL_PacFIN_FAA_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = paste0("com", fleet)) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleet), by = c("fleet" = "joint"))$fleet_num) %>%
  as.data.frame()

noFAA.caal <- mod$dat$agecomp[-which(mod$dat$agecomp$fleet %in% c(1, 2)), ]
noFAA.caal$fleet <- noFAA.caal$fleet + 2
names(noFAA.caal) <- names(com.CAAL)

mod$dat$agecomp <- dplyr::bind_rows(com.CAAL, noFAA.caal)


# Now change the selectivity tables....

mod$ctl$size_selex_types <- rbind("CA_Commercial_North" = mod$ctl$size_selex_types[1,],
                                  mod$ctl$size_selex_types[1,],
                                  "CA_Recreational_North" = mod$ctl$size_selex_types[2,],
                                  mod$ctl$size_selex_types[-1,])
rownames(mod$ctl$size_selex_types)[c(2,4)] <- paste0(rownames(mod$ctl$size_selex_types)[c(2,4)], "_South")


mod$ctl$age_selex_types <- rbind("CA_Commercial_North" = mod$ctl$age_selex_types[1,],
                                 mod$ctl$age_selex_types[1,],
                                 "CA_Recreational_North" = mod$ctl$age_selex_types[2,],
                                 mod$ctl$age_selex_types[-1,])
rownames(mod$ctl$age_selex_types)[c(2,4)] <- paste0(rownames(mod$ctl$age_selex_types)[c(2,4)], "_South")


#...and length selectivity parameterization....
#Set the new fleets selectivity to be the same as the rec fleet for now
mod$ctl$size_selex_parms <- rbind(mod$ctl$size_selex_parms[1:6,], #com north
                                  mod$ctl$size_selex_parms[1:6,], #com south
                                  mod$ctl$size_selex_parms[7:12,], #rec north
                                  mod$ctl$size_selex_parms[7:12,], #rec south
                                  mod$ctl$size_selex_parms[-c(1:12),])

selex_fleets <- rownames(mod$ctl$size_selex_types)[mod$ctl$size_selex_types$Pattern == 24] |> 
  as.list()
selex_names <- purrr::map(selex_fleets,
                          ~ glue::glue('SizeSel_P_{par}_{fleet_name}({fleet_no})',
                                       par = 1:6,
                                       fleet_name = .x,
                                       fleet_no = fleet.converter$fleet_num[fleet.converter$fleetname == .x])) |>
  unlist()

rownames(mod$ctl$size_selex_parms) <- selex_names


#...and time varying selectivity

selex_new <- mod$ctl$size_selex_parms

selex_tv_pars <- dplyr::filter(selex_new, Block > 0) |>
  dplyr::select(LO, HI, INIT, PRIOR, PR_SD, PR_type, PHASE, Block) |>
  tidyr::uncount(mod$ctl$blocks_per_pattern[Block], .id = 'id', .remove = FALSE)

rownames(selex_tv_pars) <- rownames(selex_tv_pars) |>
  stringr::str_remove('\\.\\.\\.[:digit:]+') |>
  stringr::str_c('_BLK', selex_tv_pars$Block, 'repl_', mapply("[",mod$ctl$Block_Design[selex_tv_pars$Block], selex_tv_pars$id * 2 - 1))

mod$ctl$size_selex_parms_tv <- selex_tv_pars |>
  dplyr::select(-Block, -id)


#Update variance adjustment factors but keep at 1 for now

varadj_len <- data.frame("factor" = 4, 
                         fleet = unique(mod$dat$lencomp$fleet), 
                         value = 1,
                         row.names = paste0("Len_", fleet.converter[unique(mod$dat$lencomp$fleet), "fleetname"]))
varadj_age <- data.frame("factor" = 5,
                         fleet = unique(mod$dat$agecomp$fleet), 
                         value = 1,
                         row.names = paste0("Age_", fleet.converter[unique(mod$dat$agecomp$fleet), "fleetname"]))
mod$ctl$Variance_adjustment_list <- dplyr::bind_rows(varadj_len, varadj_age)




### Update indices --------------------------------

ccfrp_index <- read.csv(here("data", "forSS3", "CCFRP_noFN_index_forSS.csv")) %>%
  dplyr::mutate(fleet = "ccfrp") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  dplyr::rename("seas" = month,
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

pr_index_n <- read.csv(here("data", "forSS3", "PR_index_forSS_FAS_N.csv")) %>%
  dplyr::mutate(fleet = "recNorth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleet), by = c("fleet" = "joint"))$fleet_num) %>%
  dplyr::rename("seas" = month,
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

pr_index_s <- read.csv(here("data", "forSS3", "PR_index_forSS_FAS_S.csv")) %>%
  dplyr::mutate(fleet = "recSouth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleet), by = c("fleet" = "joint"))$fleet_num) %>%
  dplyr::rename("seas" = month,
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

rov_index <- read.csv(here("data", "forSS3", "ROV_index_forSS.csv")) %>%
  dplyr::mutate(fleet = "rovAll") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleet), by = c("fleet" = "joint"))$fleet_num) %>%
  dplyr::rename("seas" = month,
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

mod$dat$CPUE <- dplyr::bind_rows(pr_index_n, pr_index_s, ccfrp_index, rov_index)


# Add q setup for surveys with index data

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
         dir = here('models', "_confidential_FAA_runs_noShare", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_confidential_FAA_runs_noShare", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_confidential_FAA_runs_noShare", new_name))
SS_plots(pp, plot = c(1:26))

plot_sel_all_faa(pp)



####------------------------------------------------#
## 2_4_2_FAA_confidential_reweight ----
####------------------------------------------------#

#Reweight model 242

new_name <- "2_4_2_FAA_confidential_reweight"
old_name <- "2_4_1_FAA_confidential" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_confidential_FAA_runs_noShare", old_name), 
               dir.new = here('models', "_confidential_FAA_runs_noShare", new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models', "_confidential_FAA_runs_noShare", old_name),"Report.sso"),
          to = file.path(here('models', "_confidential_FAA_runs_noShare", new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',"_confidential_FAA_runs_noShare", old_name),"CompReport.sso"),
          to = file.path(here('models',"_confidential_FAA_runs_noShare", new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', "_confidential_FAA_runs_noShare", old_name),"warning.sso"),
          to = file.path(here('models', "_confidential_FAA_runs_noShare", new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', "_confidential_FAA_runs_noShare", old_name),"covar.sso"),
          to = file.path(here('models', "_confidential_FAA_runs_noShare", new_name),"covar.sso"), overwrite = TRUE)


mod <- SS_read(here('models', "_confidential_FAA_runs_noShare", new_name))


##
#Make Changes
##

pp <- SS_output(here('models', "_confidential_FAA_runs_noShare", new_name))
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', "_confidential_FAA_runs_noShare", new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = 0, 
                       extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

colnames(dw)[1] = "factor"
new_var_adj <- dplyr::left_join(mod$ctl$Variance_adjustment_list, dw,
                                by = dplyr::join_by(factor, fleet))
mod$ctl$Variance_adjustment_list$value <-  new_var_adj$New_Var_adj


##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_confidential_FAA_runs_noShare", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_confidential_FAA_runs_noShare", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_confidential_FAA_runs_noShare", new_name))
SS_plots(pp, plot = c(1:26))

plot_sel_all_faa(pp)


####------------------------------------------------#
## 2_4_3_FAA_confidential_fixComSouthSelex ----
####------------------------------------------------#

#Reweight model 242

new_name <- "2_4_3_FAA_confidential_fixComSouthSelex"
old_name <- "2_4_2_FAA_confidential_reweight" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_confidential_FAA_runs_noShare", old_name), 
               dir.new = here('models', "_confidential_FAA_runs_noShare", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_confidential_FAA_runs_noShare", new_name))


##
#Make Changes
##

#To mirror blocks from a fleet, have to split the fleet again.
#Thus, split the commercial south fleet into two. Have fleet 2 be the recent 
#years, and add another fleet (fleet 8) to represent the early block. 

#Other options are 1) to mirror the South to the North (so data in south along with
#north used to estimate first block, only north in next two), or 2) to have only one
#block for the south (which for selex would reflect data < 2003)


## Set up fleet structure

mod$dat$Nfleets <- 8
mod$dat$fleetinfo <- rbind(mod$dat$fleetinfo, mod$dat$fleetinfo[2,])
mod$dat$fleetinfo$fleetname[8] <- c(paste0(mod$dat$fleetinfo$fleetname[2], "_early"))

mod$dat$len_info <- rbind(mod$dat$len_info, 
                          "CA_Commercial_South_early" = mod$dat$len_info[2,])

mod$dat$age_info <- rbind(mod$dat$age_info,
                          "CA_Commercial_South_early" = mod$dat$age_info[2,])

mod$dat$CPUEinfo <- rbind(mod$dat$CPUEinfo,
                          "CA_Commercial_South_early" = mod$dat$CPUEinfo[2,])
mod$dat$CPUEinfo$fleet[8] <- 8


## Set up the data

#Change the catch time series
mod$dat$catch[which(mod$dat$catch$fleet == 2 & mod$dat$catch$year < 2003), "fleet"] <- 8

#Change the length data - there is such small sample size after 2003 so omit that point
mod$dat$lencomp[which(mod$dat$lencomp$fleet == 2 & mod$dat$lencomp$year < 2003), "fleet"] <- 8
mod$dat$lencomp[which(mod$dat$lencomp$fleet == 2 & mod$dat$lencomp$year >= 2003), "year"] <-
  -mod$dat$lencomp[which(mod$dat$lencomp$fleet == 2 & mod$dat$lencomp$year >= 2003), "year"]

#There is no commercial south age data nor is there as commercial index prior to 2003


## Update variance adjustment factors. 

#Give south early (fleet 8) the same weight as original (fleet 2) because more or less same data
mod$ctl$Variance_adjustment_list <- rbind(mod$ctl$Variance_adjustment_list[c(1,3,4,5,6),],
                                          "Commercial_South_early" = mod$ctl$Variance_adjustment_list[2,],
                                          mod$ctl$Variance_adjustment_list[c(7,8),])
mod$ctl$Variance_adjustment_list[
  grep("Commercial_South_early", rownames(mod$ctl$Variance_adjustment_list)), 
       "fleet"] <- 8


## Set up the blocking and mirroring
#Mirror the recent commercial south to commercial north (first block < 2003 wont have influence)
#Set up a single block for commercial south early (only < 2003 will matter)

#Type
mod$ctl$size_selex_types <- rbind(mod$ctl$size_selex_types, 
                                  "CA_Commercial_South_early" = mod$ctl$size_selex_types[2,])
mod$ctl$size_selex_types[2, c("Pattern", "Special")] <- c(15, 1)

mod$ctl$age_selex_types <- rbind(mod$ctl$age_selex_types, 
                                 "CA_Commercial_South_early" = mod$ctl$age_selex_types[2,])

#Regular parameters (remove previous commercial south and add new commercial south early)
parm_loc <- grep("Commercial_South", rownames(mod$ctl$size_selex_parms))
new_parm <- mod$ctl$size_selex_parms[parm_loc,] %>%
  dplyr::mutate(Block = 0, Block_Fxn = 0)
rownames(new_parm) <- paste0(gsub('.{3}$', '', rownames(new_parm)), "_early(8)")

mod$ctl$size_selex_parms <- rbind(mod$ctl$size_selex_parms[-parm_loc,], new_parm)
  
#Time varying parameters (remove previous commercial south)
tv_parm_loc <- grep("Commercial_South", rownames(mod$ctl$size_selex_parms_tv))

mod$ctl$size_selex_parms_tv <- mod$ctl$size_selex_parms_tv[-tv_parm_loc,]



##
#Output files and run
##

SS_write(mod,
         dir = here('models', "_confidential_FAA_runs_noShare", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_confidential_FAA_runs_noShare", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_confidential_FAA_runs_noShare", new_name))
SS_plots(pp, plot = c(1:26))

plot_sel_all_faa(pp)


####------------------------------------------------#
## 2_4_4_FAA_confidential_resetCom ----
####------------------------------------------------#

# Remove the commercial split (only do rec as faa)

new_name <- "2_4_4_FAA_confidential_resetCom"
old_name <- "2_4_2_FAA_confidential_reweight" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_confidential_FAA_runs_noShare", old_name), 
               dir.new = here('models', "_confidential_FAA_runs_noShare", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_confidential_FAA_runs_noShare", new_name))


##
#Make Changes
##

#Given that commercial has some disparate data streams, keep as one fleet. Only
#split recreational. Thus reduce number of fleets from previous model

mod$dat$Nfleets <- 6
mod$dat$fleetinfo <- mod$dat$fleetinfo[-2,]
mod$dat$fleetinfo$fleetname[1] <- "CA_Commercial"

mod$dat$len_info <- mod$dat$len_info[-2,]
rownames(mod$dat$len_info)[1] <- "CA_Commercial"

mod$dat$age_info <- mod$dat$age_info[-2,]
rownames(mod$dat$age_info)[1] <- "CA_Commercial"

mod$dat$CPUEinfo <- mod$dat$CPUEinfo[-2,]
mod$dat$CPUEinfo$fleet <- seq(1:mod$dat$Nfleets)
rownames(mod$dat$CPUEinfo)[1] <- "CA_Commercial"

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(area = c("All", "North", "South", "All", "All", "All")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5, 6)) %>%
  dplyr::mutate(joint = paste0(fleet, area)) %>%
  dplyr::select(fleetname, fleet, area, joint, fleet_num)


## Set up the data

# Catches - combine commercial N/S catches for fleets 1 and 2, and renumber other fleets
mod$dat$catch[which(mod$dat$catch$fleet == 2), "fleet"] <- 1

aggregated.catch.df <- mod$dat$catch %>%
  dplyr::group_by(year, seas, fleet) %>%
  dplyr::summarize(catch = sum(catch)) %>%
  dplyr::arrange(fleet, year) %>%
  dplyr::mutate("catch_se" = 0.05) %>%
  data.frame()

aggregated.catch.df[which(aggregated.catch.df$fleet > 2), "fleet"] <-
  aggregated.catch.df[which(aggregated.catch.df$fleet > 2), "fleet"] - 1

mod$dat$catch <- aggregated.catch.df

# Length comps - redo length comps for the commercial fleet, renumber other fleets.
com.lengths <- read.csv(here("data", "forSS3", "Lcomps_PacFIN_unsexed_expanded_10_50.csv")) %>%
  dplyr::mutate(fleet = "com") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(com.lengths) <- names(mod$dat$lencomp)

mod$dat$lencomp <- dplyr::bind_rows(com.lengths, mod$dat$lencomp[which(mod$dat$lencomp$fleet > 2),])

mod$dat$lencomp[which(mod$dat$lencomp$fleet > 2), "fleet"] <- 
  mod$dat$lencomp[which(mod$dat$lencomp$fleet > 2), "fleet"] - 1

# Age comps - renumber because all commercial ages are in north
mod$dat$agecomp[which(mod$dat$agecomp$fleet > 2), "fleet"] <- 
  mod$dat$agecomp[which(mod$dat$agecomp$fleet > 2), "fleet"] - 1

# CPUE data - redo numbering on CPUE data and q options
mod$dat$CPUE$index <- mod$dat$CPUE$index - 1

mod$ctl$Q_options$fleet <- mod$ctl$Q_options$fleet - 1

# Remove variance adjustment factor for fleet 2 and renumber other fleets
mod$ctl$Variance_adjustment_list <- mod$ctl$Variance_adjustment_list[-2,] 
mod$ctl$Variance_adjustment_list[which(mod$ctl$Variance_adjustment_list$fleet > 2), "fleet"] <-
  mod$ctl$Variance_adjustment_list[which(mod$ctl$Variance_adjustment_list$fleet > 2), "fleet"] - 1


## Set up selectivity

#Regular selectivity parameters...
mod$ctl$size_selex_types <- mod$ctl$size_selex_types[-2,]
rownames(mod$ctl$size_selex_types)[1] <- "CA_Commercial"

mod$ctl$age_selex_types <- mod$ctl$age_selex_types[-2,]
rownames(mod$ctl$age_selex_types)[1] <- "CA_Commercial"

mod$ctl$size_selex_parms <- mod$ctl$size_selex_parms[-c(7:12),]

selex_fleets <- rownames(mod$ctl$size_selex_types)[mod$ctl$size_selex_types$Pattern == 24] |> 
  as.list()
selex_names <- purrr::map(selex_fleets,
                          ~ glue::glue('SizeSel_P_{par}_{fleet_name}({fleet_no})',
                                       par = 1:6,
                                       fleet_name = .x,
                                       fleet_no = fleet.converter$fleet_num[fleet.converter$fleetname == .x])) |>
  unlist()
rownames(mod$ctl$size_selex_parms) <- selex_names

#...and time varying selectivity
selex_new <- mod$ctl$size_selex_parms

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
         dir = here('models', "_confidential_FAA_runs_noShare", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_confidential_FAA_runs_noShare", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_confidential_FAA_runs_noShare", new_name))
SS_plots(pp, plot = c(1:26))

#plot_sel_all_faa(pp) #need to fix with new selectivity structure


####------------------------------------------------#
## 2_4_5_FAA_confidential_resetBlocks ----
####------------------------------------------------#

# Reset recreational blocks

new_name <- "2_4_5_FAA_confidential_resetBlocks"
old_name <- "2_4_4_FAA_confidential_resetCom" 

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', "_confidential_FAA_runs_noShare", old_name), 
               dir.new = here('models', "_confidential_FAA_runs_noShare", new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', "_confidential_FAA_runs_noShare", new_name))


##
#Make Changes
##

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(area = c("All", "North", "South", "All", "All", "All")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5, 6)) %>%
  dplyr::mutate(joint = paste0(fleet, area)) %>%
  dplyr::select(fleetname, fleet, area, joint, fleet_num)


#Redo recreational blocks

mod$ctl$N_Block_Designs <- 3
mod$ctl$blocks_per_pattern <- c(3, 4, 3)
mod$ctl$Block_Design <- list(c(2003, 2013, 2014, 2021, 2022, 2024), #commercial fleet
                             c(2001, 2007, 2008, 2022, 2023, 2023, 2024, 2024), #recreational north fleet
                             c(2001, 2016, 2017, 2022, 2023, 2024)) #recreational south fleet


## Set up the data

## Set up selectivity

#Set recreational south block to new block
mod$ctl$size_selex_parms[intersect(grep("Recreational_South", rownames(mod$ctl$size_selex_parms)),
                                   which(mod$ctl$size_selex_parms$PHASE > 0)), "Block"] <- 3

#...and time varying selectivity
selex_new <- mod$ctl$size_selex_parms

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
         dir = here('models', "_confidential_FAA_runs_noShare", new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', "_confidential_FAA_runs_noShare", new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', "_confidential_FAA_runs_noShare", new_name))
SS_plots(pp, plot = c(1:26))

#plot_sel_all_faa(pp) #need to fix with new selectivity structure




##
#Comparison plots
##

xx <- SSgetoutput(dirvec = c(here("models", "2_2_3_combineGrowth_CCFRP"),
                             glue::glue("{models}/{subdir}", models = here('models', "_confidential_FAA_runs_noShare"),
                                      subdir = c("2_4_2_FAA_confidential_reweight",
                                                 "2_4_3_FAA_confidential_fixComSouthSelex",
                                                 "2_4_4_FAA_confidential_resetCom",
                                                 "2_4_5_FAA_confidential_resetBlocks"))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Non FAA model',
                                     'FAA setup for rec and comm funky',
                                     'FAA setup for rec and comm with comm south mirror',
                                     'FAA setup for rec only',
                                     'FAA setup for rec only and new blocks'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', "_confidential_FAA_runs_noShare", new_name))


####------------------------------------------------#
## 2_5_1_NewCCFRPIndexLengths ----
####------------------------------------------------#

#This model uses ROV length selectivity of 1 for L8-60
#Population size bins of 1
#Changes the CCFRP index for Farallons and the length comps
#Ages stay in the growth fleet

new_name <- "2_5_1_NewCCFRPIndexLengths"
old_name <- "2_3_9_ROVFixSelex1L8"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##
#CCFRP
#these use number of drifts as sample sizes
ccfrp.lengths <- read.csv(here("data", "forSS3", "Lcomps_ccfrp_withFN_weighted_length_comps_unsexed.csv")) %>%
  as.data.frame()
  ccfrp.lengths$fleet <- 4
names(ccfrp.lengths) <- names(mod$dat$lencomp)


#replace the lengths
mod$dat$lencomp[mod$dat$lencomp$fleet == 4, ] <- ccfrp.lengths


#aa <- mod$dat$lencomp
#View(aa)

### Update index --------------------------------#
### This includes the farallons and is weigthed
ccfrp_index <- read.csv(here("data", "forSS3", "CCFRP_withFN_weighted_index_forSS.csv")) #%>%
names(ccfrp_index) <- names(mod$dat$CPUE)

#replace the index
mod$dat$CPUE[mod$dat$CPUE$index == 4, ] <- ccfrp_index
#View(mod$dat$CPUE)


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

plot_sel_all(pp)


####------------------------------------------------#
## 2_5_2_Reweight251 ----
####------------------------------------------------#

new_name <- "2_5_2_reweight251"
old_name <- "2_5_1_NewCCFRPIndexLengths"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models',old_name),"Report.sso"),
          to = file.path(here('models',new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"CompReport.sso"),
          to = file.path(here('models',new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"warning.sso"),
          to = file.path(here('models',new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"covar.sso"),
          to = file.path(here('models',new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

pp <- SS_output(here('models',new_name))
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = 0, 
                       extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

colnames(dw)[1] = "factor"
new_var_adj <- dplyr::left_join(mod$ctl$Variance_adjustment_list, dw,
                                by = dplyr::join_by(factor, fleet))
mod$ctl$Variance_adjustment_list$value <-  new_var_adj$New_Var_adj


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
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("2_3_9_ROVFixSelex1L8",
                                                 "2_5_1_NewCCFRPIndexLengths",
                                                 "2_5_2_reweight251")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('2_3_9_ROVFixSelex1L8',
                                     '2_5_1: Update CCFRP',
                                     '2_5_2: reweight 251'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))


####------------------------------------------------#
## 2_5_3_weightedROVcomps ----
####------------------------------------------------#

#Use weighted ROV comps, where weighting is based on 80/20 split

new_name <- "2_5_3_weightedROVcomps"
old_name <- "2_5_2_reweight251"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)

#Update ROV length comps with weighted version
rov.lengths <- read.csv(here("data", "forSS3", "Lcomps_rov_unsexed_weighted_10_50.csv")) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(rov.lengths) <- names(mod$dat$lencomp)

mod$dat$lencomp[mod$dat$lencomp$fleet == fleet.converter[fleet.converter$fleet == "rov", "fleet_num"],] <- rov.lengths


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
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("2_5_2_reweight251",
                                                 "2_5_3_weightedROVcomps")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 252',
                                     'use weighted ROV comps'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))


####------------------------------------------------#
## 2_5_4_ActualYear_ROVComps ----
####------------------------------------------------#

#Use actual year for ROV comps (omits 3 samples in 2016 over two trips)

new_name <- "2_5_4_ActualYear_ROVComps"
old_name <- "2_5_3_weightedROVcomps"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5)) %>%
  dplyr::select(fleetname, fleet, fleet_num)

#Update ROV length comps with weighted version for actual year (not super year)
rov.lengths <- read.csv(here("data", "forSS3", "Lcomps_rov_unsexed_weighted_ACTUAL_YEAR_10_50.csv")) %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_num) %>%
  as.data.frame()
names(rov.lengths) <- names(mod$dat$lencomp)
rov.lengths[rov.lengths$year == 2016, "year"] <- -2016 #negative year due to small sample size

other.lengths <- mod$dat$lencomp[!mod$dat$lencomp$fleet == fleet.converter[fleet.converter$fleet == "rov", "fleet_num"],]

mod$dat$lencomp <- dplyr::bind_rows(other.lengths, rov.lengths)


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
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("2_5_3_weightedROVcomps",
                                                 "2_5_4_ActualYear_ROVComps")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 253',
                                     'use actual year weighted ROV comps'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))


####------------------------------------------------#
## 2_5_5_update_CCFRPSampleSize ----
####------------------------------------------------#

#Updates samples sizes for CCFRP length comps (total drifts rather than average)

new_name <- "2_5_5_update_CCFRPSampleSize"
old_name <- "2_5_4_ActualYear_ROVComps"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

ccfrp.lengths <- read.csv(here("data", "forSS3", "Lcomps_ccfrp_withFN_weighted_length_comps_unsexed.csv")) %>%
  dplyr::select(-InputN.Year) %>%
  as.data.frame()
ccfrp.lengths$comps.fleet <- 4
names(ccfrp.lengths) <- names(mod$dat$lencomp)

mod$dat$lencomp[mod$dat$lencomp$fleet == 4, ] <- ccfrp.lengths


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
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("2_5_4_ActualYear_ROVComps",
                                                 "2_5_5_update_CCFRPSampleSize")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 254',
                                     'update sample size for CCFRP length comps'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))


####------------------------------------------------#
## 2_5_6_inputFixes ----
####------------------------------------------------#

#Update model inputs for recdev years, growth params phase to 3, and
#PR index units to numbers. Also rest ROV selex to be double normal 
#(which fixes warnings). Keeping 1 cm pop bins though. 

new_name <- "2_5_6_inputFixes"
old_name <- "2_5_5_update_CCFRPSampleSize"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$dat$CPUEinfo["CA_Recreational", "units"] <- 0

mod$ctl$MG_parms[2:6, "PHASE"] <- 3

mod$ctl$MainRdevYrLast <- 2021

#For bias adj ramp need a hessian

mod$dat$maximum_size <- 60 - mod$dat$binwidth #(with 1 cm bins need to change this)

#Reset ROV selectivity to double normal and reuse previous models' values
mod$ctl$size_selex_types["CA_ROV", "Pattern"] <- 24
prev_mod <- SS_read(here('models', '2_3_8_removeSparseData_5'))
prev_selexROV <- prev_mod$ctl$size_selex_parms[grep("ROV", rownames(prev_mod$ctl$size_selex_parms)), ]

mod$ctl$size_selex_parms <- dplyr::bind_rows(
  mod$ctl$size_selex_parms[-grep("ROV", rownames(mod$ctl$size_selex_parms)), ],
  prev_selexROV)


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
plot_sel_all(pp)


####------------------------------------------------#
## 2_5_7_reweight256 ----
####------------------------------------------------#

#Update the model weights

new_name <- "2_5_7_reweight256"
old_name <- "2_5_6_inputFixes"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models',old_name),"Report.sso"),
          to = file.path(here('models',new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"CompReport.sso"),
          to = file.path(here('models',new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"warning.sso"),
          to = file.path(here('models',new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models',old_name),"covar.sso"),
          to = file.path(here('models',new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

pp <- SS_output(here('models',new_name))
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = 0, 
                       extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

colnames(dw)[1] = "factor"
new_var_adj <- dplyr::left_join(mod$ctl$Variance_adjustment_list, dw,
                                by = dplyr::join_by(factor, fleet))
mod$ctl$Variance_adjustment_list$value <-  new_var_adj$New_Var_adj


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
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("2_5_2_reweight251",
                                                 "2_5_3_weightedROVcomps",
                                                 "2_5_4_ActualYear_ROVComps",
                                                 "2_5_5_update_CCFRPSampleSize",
                                                 "2_5_6_inputFixes",
                                                 "2_5_7_reweight256")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 252',
                                     '253: add weighted ROV comps',
                                     '254: change ROV comps to actual years',
                                     '255: change CCFRP comp sample sizes',
                                     '256: revert ROV selex and other inputs',
                                     '257: reweight 256'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))

#Can move this elsewhere - looks at the number of age 1 fish to compare to Tanya's abs. MPA est.
#Sum numbers at age
numbers_at_age <- pp$natage
pp$natageOnePlus_numbers <- numbers_at_age %>%
  filter(`Beg/Mid` == "M") %>% #taking that mid year since that represents the survey
  mutate(numberOfFish = rowSums(across(c("3":"90")))) %>%  #could also look at ages 2+
  dplyr::select(c("Time", "numberOfFish"))

#Tanya predicts 155,255 in 2015 and 298,559 in 2020
#Model predicted 2015.5 - believe these are in 1,000s
ageOnePlus_numbers %>% filter(Time == 2015.5)
#Model predicted 2020.5
ageOnePlus_numbers %>% filter(Time == 2020.5)


####------------------------------------------------#
## 3_0_1_fix_rovIndex_2024discard ----
####------------------------------------------------#

#Update the model weights

new_name <- "3_0_1_fix_rovIndex_2024discard"
old_name <- "2_5_7_reweight256"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

#Fix the 2024 catch estimate for commercial to the new estimate. No other estimate differed
catches <- read.csv(here("data", "CAquillback_total_removals.csv"))

mod$dat$catch[mod$dat$catch$year == 2024 & mod$dat$catch$fleet == 1, "catch"] <- 
  catches[catches$Year == 2024, "com_tot"]


#Use the new ROV index values and se
rov_index <- read.csv(here("data", "forSS3", "ROV_index_forSS.csv")) %>%
  dplyr::rename("seas" = month, 
                "se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()
names(rov_index) <- names(mod$dat$CPUE)

mod$dat$CPUE[mod$dat$CPUE$index == unique(rov_index$index),] <- rov_index 


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
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("2_5_7_reweight256",
                                                 "3_0_1_fix_rovIndex_2024discard")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 257',
                                     'correct ROV index values and 2024 comm catch'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()


####------------------------------------------------#
## 3_0_2_plusGrowth999----
####------------------------------------------------#

#Change the exponential decay function value for decay after plus group

new_name <- "3_0_2_plusGrowth999"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$Exp_Decay <- 999

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
plot_sel_all(pp)
#Does nothing - just a check
#can remove model or change



####------------------------------------------------#
## 3_0_3_Nages60----
####------------------------------------------------#

#Change Nages to 60 

new_name <- "3_0_3_Nages60"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##
mod$dat$Nages <- 60 # to correspond with maximum age

mod$dat$ageerror <- mod$dat$ageerror[,1:61]
#ageing error matrix


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
plot_sel_all(pp)


####------------------------------------------------#
## 3_0_4_recdev_phase ----
####------------------------------------------------#

#Change phase of recdevs from 2 to 4 and early devs from 5 to 2
#to see if this affects things

new_name <- "3_0_4_recdev_phase"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##
mod$ctl$recdev_phase <- 4
mod$ctl$recdev_early_phase <- 2


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
plot_sel_all(pp)

#It does not


####------------------------------------------------#
## 3_0_5_recdev_years ----
####------------------------------------------------#

#Change time period of early recdevs to be from first year and main recdevs
#to go back through to what was the early recdevs

new_name <- "3_0_5_recdev_years"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$recdev_early_start <- 1916 #set to first year of model
mod$ctl$MainRdevYrFirst <- 1940 #set to what was early phase start


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
plot_sel_all(pp)

round(pp$likelihoods_used,2)
round(SS_output(here('models', '3_0_1_fix_rovIndex_2024discard'))$likelihoods_used,2)
#Setting these does reduce the likelihood but the total amount is less than 1 unit
#and the number of parameters is significantly more. Pattern in recdev isn't greatly
#change though trajector does differ a bit. Regardless, there is nothing missing
#from what we had previously. Thus, keep what we had. 

##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_4_recdev_phase",
                                                 "3_0_5_recdev_years")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'change main recdev phase to 4, early to 2',
                                     'change early recdev to styr, and main to 1940'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()


####------------------------------------------------#
## 3_0_6_floatQ ----
####------------------------------------------------#

#Instead of estimating q, float and see if there is a difference

new_name <- "3_0_6_floatQ"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$Q_options$float <- 1
mod$ctl$Q_parms$PHASE <- -2

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
plot_sel_all(pp)

round(SS_output(here('models', '3_0_1_fix_rovIndex_2024discard'))$likelihoods_used,2)
round(pp$likelihoods_used,2)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_6_floatQ")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'float q'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()
#No real effect. Personally I like the parameter


####------------------------------------------------#
## 3_0_7_felFbasis ----
####------------------------------------------------#

#relF basis is 0 in the forecast. This isn't in the manual. Set to 1, which
#is year range of benchmark, which is entered as 2024

new_name <- "3_0_7_felFbasis"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

#Adjust relative F basis
mod$fore$Bmark_relF_Basis <- 1 #there isn't a 0 option in the manual


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
plot_sel_all(pp)


####------------------------------------------------#
## 3_0_8_felFbasis2 ----
####------------------------------------------------#

#Set relF basis to 2 in the forecast, which matches the forecast and is 2021-2024

new_name <- "3_0_8_felFbasis2"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

#Adjust relative F basis
mod$fore$Bmark_relF_Basis <- 2 #there isn't a 0 option in the manual


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
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_7_felFbasis",
                                                 "3_0_8_felFbasis2")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'relF_basis 2024',
                                     'relF_basis 2021-2024'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()
#No real effect on model trajectory. Should affect plots and references for forecasts


####------------------------------------------------#
## 3_0_9_priorLike0 ----
####------------------------------------------------#

#Prior likelihood is included in lieklihood within starter. Either shut off (this run)
#or adjust parameters with prior types so only those we want (M and h) affect

new_name <- "3_0_9_priorLike0"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

#Set prior type like to 0 - meaning only calculate priors for active parameters
mod$start$prior_like <- 0


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

#Greater difference than I was expecting
round(SS_output(here('models', '3_0_1_fix_rovIndex_2024discard'))$likelihoods_used,2)
round(pp$likelihoods_used,2)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_9_priorLike0")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'prior like only for active parameters'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()


####------------------------------------------------#
## 3_0_10_priorLike1 ----
####------------------------------------------------#

#Prior likelihood is included in lieklihood within starter. Either shut off
#or adjust parameters with prior types (this run) so only those we want 
#(M and h) affect

new_name <- "3_0_10_priorLike1"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

#Keep prior type like at 1 - meaning calculate priors for any priors
#but remove the prior types for all but M and h (meaning remove prior type for maturity)
mod$ctl$MG_parms[c("Mat50%_Fem_GP_1", "Mat_slope_Fem_GP_1"), "PR_type"] <- 0


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

#Greater difference than I was expecting
round(SS_output(here('models', '3_0_1_fix_rovIndex_2024discard'))$likelihoods_used,2)
round(pp$likelihoods_used,2)

#Still, to keep profiles smooth, set prior like to 1 but remove prior type for
#all but M and h parameters


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_10_priorLike1")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'prior like for M and h only'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()


####------------------------------------------------#
## 3_0_11_matureAge2 ----
####------------------------------------------------#

#Set first mature age to 2. Maturity is around 0.001 at 16, which is around age 2

new_name <- "3_0_11_matureAge2"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$First_Mature_Age <- 2


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
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_11_matureAge2")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'first mature age set to 2'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()


####------------------------------------------------#
## 3_0_12_Fiter7_Fballpark0.5 ----
####------------------------------------------------#

#See if the number of iterations affects the current F method (hybrid) and
#whether the F ballpark value matters (it shouldn't because its fixed)

new_name <- "3_0_12_Fiter7_Fballpark0.5"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$F_ballpark <- 0.5
mod$ctl$F_iter <- 7


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

mod301 <- SS_output(here('models', '3_0_1_fix_rovIndex_2024discard'))$likelihoods_used
newmod <- round(pp$likelihoods_used,2)
round(cbind(mod301-newmod),3)

#There is a very slight difference with F ballpark though our existing entry (0.04)
#is not too far off from what is in report file for mod301 for 2001 (0.06)
#F iterations changes are also not impactful. Could change F ballpark to 0.06



##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_12_Fiter7_Fballpark0.5")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'increase F iterations and F ballpark'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()


####------------------------------------------------#
## 3_0_13_recdevOption2----
####------------------------------------------------#

#Set recdev option not to sum to 0 (set to 2)

new_name <- "3_0_13_recdevOption2"
old_name <- "3_0_1_fix_rovIndex_2024discard"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##
mod$ctl$do_recdev <- 2

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

plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_13_recdevOption2")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'recdev option2 - no sum to 0'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))

SSsummarize(xx) |>
 SSplotComparisons(legendlabels = c('model 301',
                                     'recdev option2 - no sum to 0'),
                    subplots = c(9:12), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()

#Look at the recdevs



####------------------------------------------------#
## 3_0_14_recdevOption3----
####------------------------------------------------#

#Set recdev option not to sum to 0 (set to 2)

new_name <- "3_0_14_recdevOption3"
old_name <- "3_0_1_fix_rovIndex_2024discard"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##
mod$ctl$do_recdev <- 3

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

plot_sel_all(pp)

####------------------------------------------------#
## 3_0_15_recdevOption4----
####------------------------------------------------#

#Set recdev option not to sum to 0 (set to 2)

new_name <- "3_0_15_recdevOption4"
old_name <- "3_0_1_fix_rovIndex_2024discard"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models',new_name))


##
#Make Changes
##
mod$ctl$do_recdev <- 4

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

plot_sel_all(pp)

##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_13_recdevOption2",
                                                 "3_0_14_recdevOption3",
                                                 "3_0_15_recdevOption4")))
SSsummarize(xx) |>
 SSplotComparisons(legendlabels =  c('model 301',
                                     'recdev option2','recdev option3', 'recdev option4'),
                    subplots = c(1,3,9:12), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()

####------------------------------------------------#
## 3_0_16_Foption4 ----
####------------------------------------------------#

#Set up F option 4  to see how different results are and how well fit 
#catch time series are. Option 4 is also the preferred option now (over 3)

new_name <- "3_0_16_Foption4"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$F_Method <- 4
mod$ctl$F_4_Fleet_Parms <- data.frame("fleet" = c(1,2),
                                      "start_F" = 0.04,
                                      "first_parm_phase" = 2)
mod$ctl$F_iter <- 2 #Manual says 2 is sufficient, could keep at 4 to get what manual says is "near exactly"


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

#Catch fits are good
ggplot(pp$catch) +
  geom_line(aes(y = Obs, x = Yr, color = "Obs")) +
  geom_line(aes(y = Exp, x = Yr, color = "Exp")) + 
  facet_wrap(~ Fleet)

#This results in the same output, and catch fits are just as good as with hybrid


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_0_14_Foption4")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'F method 4 with 4 iters'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()



####------------------------------------------------#
## 3_1_1_hessian301 ----
####------------------------------------------------#

#Run hessian from model 301 to set up correct bias adj ramp and check sigmaR.

new_name <- "3_1_1_hessian301"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##



##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


####------------------------------------------------#
## 3_1_2_biasAdjRamp ----
####------------------------------------------------#

#Set up correct bias adj ramp from hessian model

new_name <- "3_1_2_biasAdjRamp"
old_name <- "3_1_1_hessian301"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models', old_name),"Report.sso"),
          to = file.path(here('models', new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', old_name),"CompReport.sso"),
          to = file.path(here('models',new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', old_name),"warning.sso"),
          to = file.path(here('models',new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', old_name),"covar.sso"),
          to = file.path(here('models',new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here('models',new_name))

pp <- SS_output(here('models',new_name), covar = TRUE)


##
#Make Changes
##

#Update bias adjust? Yes, all values
pp$breakpoints_for_bias_adjustment_ramp

biasadj <- SS_fitbiasramp(pp, verbose = TRUE)

mod$ctl$last_early_yr_nobias_adj <- biasadj$df[1, "value"]
mod$ctl$first_yr_fullbias_adj <- biasadj$df[2, "value"]
mod$ctl$last_yr_fullbias_adj <- biasadj$df[3, "value"] 
mod$ctl$first_recent_yr_nobias_adj <- biasadj$df[4, "value"] 
mod$ctl$max_bias_adj <- biasadj$df[5, "value"]

##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


####------------------------------------------------#
## 3_1_3_biasAdjRamp2 ----
####------------------------------------------------#

#Repeat bias adjust ramp because previous model suggests an update is needed

new_name <- "3_1_3_biasAdjRamp2"
old_name <- "3_1_2_biasAdjRamp"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models', old_name),"Report.sso"),
          to = file.path(here('models', new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', old_name),"CompReport.sso"),
          to = file.path(here('models',new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', old_name),"warning.sso"),
          to = file.path(here('models',new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', old_name),"covar.sso"),
          to = file.path(here('models',new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here('models',new_name))

pp <- SS_output(here('models',new_name), covar = TRUE)


##
#Make Changes
##

#Update bias adjust again? Yes, all values
pp$breakpoints_for_bias_adjustment_ramp

biasadj <- SS_fitbiasramp(pp, verbose = TRUE)

mod$ctl$last_early_yr_nobias_adj <- biasadj$df[1, "value"]
mod$ctl$first_yr_fullbias_adj <- biasadj$df[2, "value"]
mod$ctl$last_yr_fullbias_adj <- biasadj$df[3, "value"] 
mod$ctl$first_recent_yr_nobias_adj <- biasadj$df[4, "value"] 
mod$ctl$max_bias_adj <- biasadj$df[5, "value"]

##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)
#Doesn't look like need to do it a third time


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_1_1_hessian301",
                                                 "3_1_2_biasAdjRamp",
                                                 "3_1_3_biasAdjRamp2")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 311 hessian',
                                     'Update bias adj 1',
                                     'Update bias adj 2'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))

dev.off()


####------------------------------------------------#
## 3_1_4_sigmaR ----
####------------------------------------------------#

#Explore sigmaR adjustments

new_name <- "3_1_4_sigmaR"
old_name <- "3_1_3_biasAdjRamp2"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models', old_name),"Report.sso"),
          to = file.path(here('models', new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', old_name),"CompReport.sso"),
          to = file.path(here('models',new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', old_name),"warning.sso"),
          to = file.path(here('models',new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', old_name),"covar.sso"),
          to = file.path(here('models',new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here('models',new_name))

pp <- SS_output(here('models',new_name), covar = TRUE)


##
#Make Changes
##

#Update sigmaR with tuned value? Suggests higher value
alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main","alternative_sigma_R"]

mod$ctl$SR_parms["SR_sigmaR", "INIT"] <- as.numeric(alt_sigmaR)


##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

#Next round suggest even higher sigmaR. Seem likes its the common pattern that 
#it wants to just keep increasing. Keeping at 0.6 seems reasonable.
pp$sigma_R_info


####------------------------------------------------#
## 3_1_5_sigmaR0.4 ----
####------------------------------------------------#

#Explore sigmaR adjustments

new_name <- "3_1_5_sigmaR0.4"
old_name <- "3_1_4_sigmaR"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))



##
#Make Changes
##

mod$ctl$SR_parms["SR_sigmaR", "INIT"] <- 0.4


##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

#Still wants it higher
pp$sigma_R_info


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_1_3_biasAdjRamp2",
                                                 "3_1_4_sigmaR",
                                                 "3_1_5_sigmaR0.4")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 313 new bias adj ramp',
                                     'increase sigmaR to formula',
                                     'fix sigmaR to 0.4'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()


####------------------------------------------------#
## 3_2_1_issue63Changes ----
####------------------------------------------------#

#Make changes from discussion of issue #63

new_name <- "3_2_1_issue63Changes"
old_name <- "3_0_1_fix_rovIndex_2024discard"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

#Pull bias adjustment values from model 313
biasadj_mod <- SS_read(here('models', "3_1_3_biasAdjRamp2"))
mod$ctl$last_early_yr_nobias_adj <- biasadj_mod$ctl$last_early_yr_nobias_adj
mod$ctl$first_yr_fullbias_adj <- biasadj_mod$ctl$first_yr_fullbias_adj
mod$ctl$last_yr_fullbias_adj <- biasadj_mod$ctl$last_yr_fullbias_adj 
mod$ctl$first_recent_yr_nobias_adj <- biasadj_mod$ctl$first_recent_yr_nobias_adj 
mod$ctl$max_bias_adj <- biasadj_mod$ctl$max_bias_adj

#Keep prior type like at 1 but remove the prior types for all but M and h 
#(meaning remove prior type for maturity)
mod$ctl$MG_parms[c("Mat50%_Fem_GP_1", "Mat_slope_Fem_GP_1"), "PR_type"] <- 0

#Adjust relative F basis
mod$fore$Bmark_relF_Basis <- 2 #there isn't a 0 option in the manual

#Set plus group to 80
mod$dat$Nages <- 80
mod$dat$ageerror <- mod$dat$ageerror[,1:(mod$dat$Nages + 1)]

#Set steepness sd to two sig. digits
mod$ctl$SR_parms["SR_BH_steep", "PR_SD"] = 0.16

#Set exponential decay to 3.24 version
mod$ctl$Exp_Decay <- -999


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
plot_sel_all(pp)


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("3_0_1_fix_rovIndex_2024discard",
                                                 "3_2_1_issue63Changes")))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('model 301',
                                     'input changes from github issue 63'),
                    subplots = c(1,3), print = TRUE, legendloc = "topright",
                    plotdir = here('models', new_name))
dev.off()



####------------------------------------------------#
## 3_2_2_SetUpExtraSE ----
####------------------------------------------------#

#Make changes from discussion of issue #63
#Set up extraSE for the indices but keep phase negative

new_name <- "3_2_2_SetUpExtraSE"
old_name <- "3_2_1_issue63Changes"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))


##
#Make Changes
##

mod$ctl$Q_parms

mod$ctl$Q_parms <- dplyr::add_row(mod$ctl$Q_parms,
                                  LO=0,
                                  HI = 0.5,
                                  INIT = 0,
                                  PRIOR = 0,
                                  PR_SD = 1,
                                  PR_type = 0,
                                  PHASE = -2,
                                  `env_var&link` = 0,
                                  dev_link = 0,
                                  dev_minyr = 0,
                                  dev_maxyr = 0,
                                  dev_PH = 0,
                                  Block = 0,
                                  Block_Fxn = 0,
                                  .before=2)

rownames(mod$ctl$Q_parms)
rownames(mod$ctl$Q_parms) <- c("LnQ_base_CA_Recreational(2)", "ExtraSD_CA_Recreational(2)", "LnQ_base_CA_CCFRP(4)", "LnQ_base_CA_ROV(5)")

mod$ctl$Q_parms <- dplyr::add_row(mod$ctl$Q_parms,
                                  LO=0,
                                  HI = 0.5,
                                  INIT = 0,
                                  PRIOR = 0,
                                  PR_SD = 1,
                                  PR_type = 0,
                                  PHASE = -2,
                                  `env_var&link` = 0,
                                  dev_link = 0,
                                  dev_minyr = 0,
                                  dev_maxyr = 0,
                                  dev_PH = 0,
                                  Block = 0,
                                  Block_Fxn = 0,
                                  .before=4)

rownames(mod$ctl$Q_parms)
rownames(mod$ctl$Q_parms) <- c("LnQ_base_CA_Recreational(2)", "ExtraSD_CA_Recreational(2)", "LnQ_base_CA_CCFRP(4)", "ExtraSD_CA_CCFRP(4)", "LnQ_base_CA_ROV(5)")

mod$ctl$Q_parms <- dplyr::add_row(mod$ctl$Q_parms,
                                  LO=0,
                                  HI = 0.5,
                                  INIT = 0,
                                  PRIOR = 0,
                                  PR_SD = 1,
                                  PR_type = 0,
                                  PHASE = -2,
                                  `env_var&link` = 0,
                                  dev_link = 0,
                                  dev_minyr = 0,
                                  dev_maxyr = 0,
                                  dev_PH = 0,
                                  Block = 0,
                                  Block_Fxn = 0,
                                  .after=5)

rownames(mod$ctl$Q_parms) <- c("LnQ_base_CA_Recreational(2)", "ExtraSD_CA_Recreational(2)", "LnQ_base_CA_CCFRP(4)", "ExtraSD_CA_CCFRP(4)", "LnQ_base_CA_ROV(5)", "ExtraSD_base_CA_ROV(5)")

mod[["ctl"]][["Q_options"]][["extra_se"]] <- c(1,1,1)

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
plot_sel_all(pp)










