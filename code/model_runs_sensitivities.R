##########################################################################################-
#
# Sensitivity runs for 2025 California Quillback Rockfish 
#   By: Brian Langseth, Melissa Monk, Julia Coates
#
##########################################################################################-

#Load packages
library(r4ss)
library(here)
library(dplyr)
library(ggplot2)
source(here('code/selexComp.R'))

#Enter in base model from which to base sensitivities
#base_mod_name <-'3_2_2_SetUpExtraSE' #<---------------UPDATE WHEN CHANGE
base_mod_name <- '4_2_1_propBase' #<---------------UPDATE WHEN CHANGE
base_mod <- SS_read(here('models', base_mod_name))

#Create the sensitivities directory
sens_dir <- here('models', '_sensitivities')
dir.create(sens_dir)

#Set up fleet converter
fleet.converter <- base_mod$dat$fleetinfo %>%
  dplyr::mutate(fleet_no_num = 1:5,
                fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::select(fleetname, fleet_no_num, fleet)


####------------------------------------------------#
# Leave one out explorations ----
####------------------------------------------------#

# ============================================================================ #
## Drop length data by fleet ----
# ============================================================================ #

#Many options to do this
# 1. downweight the comps (but selectivity could get squiggly)
# 2. remove and then fix selectivity to something (i.e. maturity, another fleet)
# 3. remove (but then the selectivity is mostly defined by the bounds and do we want this?)
#Ultimately going with option 3 just to see. 

# ============================================================================ #
# leaveOut_rec_lengths

# Rec lengths
new_name <- 'leaveOut_rec_lengths'

mod <- base_mod #<------- This is a required line so as not to change the base model files

#Set all rec lengths to negative year
mod$dat$lencomp <- mod$dat$lencomp %>% 
  dplyr::mutate(year = ifelse(fleet == fleet.converter$fleet_no_num[grep("rec", fleet.converter$fleet)], 
                              -abs(year), year))

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Remove rec lengths'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))

# ============================================================================ #
# leaveOut_com_lengths

# Commercial lengths

new_name <- 'leaveOut_com_lengths'

mod <- base_mod

#Set all rec lengths to negative year
mod$dat$lencomp <- mod$dat$lencomp %>% 
  dplyr::mutate(year = ifelse(fleet == fleet.converter$fleet_no_num[grep("com", fleet.converter$fleet)], 
                              -abs(year), year))

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Remove com lengths'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
## Drop age data by fleet ----
# ============================================================================ #


# ============================================================================ #
# leaveOut_com_ages

# Commercial ages

new_name <- 'leaveOut_com_ages'

mod <- base_mod

#Set all rec lengths to negative year
mod$dat$agecomp <- mod$dat$agecomp %>% 
  dplyr::mutate(year = ifelse(fleet == fleet.converter$fleet_no_num[grep("com", fleet.converter$fleet)], 
                              -abs(year), year))

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Remove com ages'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))

# ============================================================================ #
# leaveOut_growth_ages

# Growth fleet ages

new_name <- 'leaveOut_growth_ages'

mod <- base_mod

#Set all rec lengths to negative year
mod$dat$agecomp <- mod$dat$agecomp %>% 
  dplyr::mutate(year = ifelse(fleet == fleet.converter$fleet_no_num[grep("growth", fleet.converter$fleet)], 
                              -abs(year), year))

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Remove growth ages'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))



# ============================================================================ #
## Drop index fleets ----
# ============================================================================ #


# ============================================================================ #
# leaveOut_ccfrp

#Drop CCFRP

new_name <- 'leaveOut_ccfrp'

mod <- base_mod

#Set all ccfrp lengths to negative year
mod$dat$lencomp <- mod$dat$lencomp %>% 
  dplyr::mutate(year = ifelse(fleet == fleet.converter$fleet_no_num[grep("ccfrp", fleet.converter$fleet)], 
                              -abs(year), year))

#Set ccfrp index to negative year
mod$dat$CPUE <- mod$dat$CPUE %>% 
  dplyr::mutate(year = ifelse(index == fleet.converter$fleet_no_num[grep("ccfrp", fleet.converter$fleet)], 
                              -abs(year), year))

#Remove CCFRP q
mod$ctl$Q_options <- mod$ctl$Q_options %>%
  dplyr::filter(fleet != fleet.converter$fleet_no_num[grep("ccfrp", fleet.converter$fleet)])
mod$ctl$Q_parms <- mod$ctl$Q_parms[-grep("CCFRP", rownames(mod$ctl$Q_parms)),]


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Remove CCFRP fleet'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))

# ============================================================================ #
# leaveOut_rov

#Drop ROV

new_name <- 'leaveOut_rov'

mod <- base_mod

#Set all rov lengths to negative year
mod$dat$lencomp <- mod$dat$lencomp %>% 
  dplyr::mutate(year = ifelse(fleet == fleet.converter$fleet_no_num[grep("rov", fleet.converter$fleet)], 
                              -abs(year), year))

#Set rov index to negative year
mod$dat$CPUE <- mod$dat$CPUE %>% 
  dplyr::mutate(year = ifelse(index == fleet.converter$fleet_no_num[grep("rov", fleet.converter$fleet)], 
                              -abs(year), year))

#Remove rov q
mod$ctl$Q_options <- mod$ctl$Q_options %>%
  dplyr::filter(fleet != fleet.converter$fleet_no_num[grep("rov", fleet.converter$fleet)])
mod$ctl$Q_parms <- mod$ctl$Q_parms[-grep("ROV", rownames(mod$ctl$Q_parms)),]


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Remove ROV fleet'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
# leaveOut_prIndex

# Drop PR index (example for dropping by using lambda)

new_name <- 'leaveOut_prIndex'

mod <- base_mod

# Create a lambda section 
lambdas <- matrix(NA, 1, 5)
rownames(lambdas) <- "Surv_CA_Recreational_Phz2"
colnames(lambdas) <- c("like_comp", "fleet", "phase", "value", "sizefreq_method")
lambdas[1, ] <- c(1,2,1,0,1)
# Made the matrix into the proper list format
lambdas2 <- as.list(split(lambdas, col(lambdas)))
names(lambdas2) <- colnames(lambdas)
lambdas3 <- as.data.frame(lambdas2)
mod[["ctl"]][["lambdas"]] <- lambdas3
mod[["ctl"]][["N_lambdas"]] <- nrow(lambdas3)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Remove PR Index'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


####------------------------------------------------#
# Catch sensitivities ----
####------------------------------------------------#

# ============================================================================ #
# increaseCatchSE

## Increase Catch SE --------------------------------------------------------

new_name <- 'increaseCatchSE'

mod <- base_mod

mod[["dat"]][["catch"]][["catch_se"]] <- rep(0.1, 218)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Increase Catch SE'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
# CatchOutliers

## Adjusting High Catch Outliers --------------------------------------------------------

new_name <- 'CatchOutliers'

mod <- base_mod

catch <- mod[["dat"]][["catch"]]

# Assign a commercial catch value in 1991 that is the average of the 3 years before and after
catch$catch[catch$fleet==1 & catch$year==1991]
years <- c(1988:1990, 1991:1994)
catch$catch[catch$fleet==1 & catch$year==1991] <- mean(catch$catch[catch$fleet==1 & catch$year %in% years])

# Replace the recreational value in 1983 with the average of the 3 years before and after
# Replace the recreational value in 1993 with the average of the 3 years after
# We can't calculate a 1993 value including the 3 years before because there was no sampling in 1990-1992
# Use new values for these 1990-1992 blanks based on the new values for 1983 and 1993
# These calculations are done in the catches_sensitivity.R script

years <- c(1983, 1990, 1991, 1992, 1993)
catch$catch[catch$fleet==2 & catch$year %in% years] <- c(10.053, 5.688, 5.843, 5.998, 6.003)

mod[["dat"]][["catch"]] <- catch

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Reduce Catch Outliers'),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name))


####------------------------------------------------#
# Other sensitivities ----
####------------------------------------------------#

# ============================================================================ #
# NoRecLen2024

## Leave out 2024 recreational lengths --------------------------------------------------------

new_name <- 'NoRecLen2024'

mod <- base_mod

lencomp <- mod[["dat"]][["lencomp"]]
lencomp$year[lencomp$fleet==2 & lencomp$year==2024] <- -2024
mod[["dat"]][["lencomp"]] <- lencomp

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'No Rec Length 2024'),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name))


## Add extra SE to indices --------------------------------------------------------------------

# ============================================================================ #
# IndexExtraSE

#Make changes from discussion of issue #63
#Set up extraSE for the indices but keep phase negative

new_name <- "IndexExtraSE"

mod <- base_mod

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
                                  PHASE = 2,
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
                                  PHASE = 2,
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
                                  PHASE = 2,
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

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Index Extra SE'),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
# FAA

## Fleets as areas --------------------------------------------------------

#These data are confidential IF splitting the commercial fleet. However this run
#is the version where the commercial fleet is not split so this is not confidential.

new_name <- "FAA_resetCom"

mod <- base_mod


##
#Make Changes
##

#Update fleet information for model, lengths, ages, and indices
mod$dat$Nfleets <- 6
mod$dat$fleetinfo <- rbind(mod$dat$fleetinfo[1,],
                           c("type" = 1, "surveytiming" = -1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_Recreational_North"),
                           mod$dat$fleetinfo[-1,])
mod$dat$fleetinfo$fleetname[3] <- paste0(mod$dat$fleetinfo$fleetname[3], "_South")

mod$dat$len_info <- rbind(mod$dat$len_info[1,],
                          "CA_Recreational_North" = mod$dat$len_info[2,],
                          mod$dat$len_info[-1,])
rownames(mod$dat$len_info)[3] <- paste0(rownames(mod$dat$len_info)[3], "_South")

mod$dat$age_info <- rbind(mod$dat$age_info[1,],
                          "CA_Recreational_North" = mod$dat$age_info[2,],
                          mod$dat$age_info[-1,])
rownames(mod$dat$age_info)[3] <- paste0(rownames(mod$dat$age_info)[3], "_South")

mod$dat$CPUEinfo <- rbind(mod$dat$CPUEinfo[1,],
                          "CA_Recreational_North" =  mod$dat$CPUEinfo[2,],
                          mod$dat$CPUEinfo[-1,])
rownames(mod$dat$CPUEinfo)[3] <- paste0(rownames(mod$dat$CPUEinfo)[3], "_South")
mod$dat$CPUEinfo$fleet <- c(1:6)

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(area = c("All", "North", "South", "All", "All", "All")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5, 6)) %>%
  dplyr::mutate(joint = paste0(fleet, area)) %>%
  dplyr::select(fleetname, fleet, area, joint, fleet_num)


## Set up the data

# Catches
#Just update the rec catches
catches <- read.csv(here("data", "confidential_noShare", "CAquillback_total_removals_faa.csv"))
catches[is.na(catches)] <- 0

updated.rec.catch.df <- catches %>%
  dplyr::select(c(Year, rec_tot_North, rec_tot_South)) %>%
  tidyr::pivot_longer(cols = -Year, names_to = c('fleet', 'type', 'area'), values_to = 'catch', 
                      names_sep = '_') %>% #ideally I want to separate by second hyphen but this is a workaround
  dplyr::mutate(joint = paste0(fleet, area)) %>%
  dplyr::left_join(fleet.converter %>% dplyr::select(joint, fleet_num), by = c("joint" = "joint")) %>%
  dplyr::mutate(seas = 1, 
                catch_se = 0.05) %>%
  dplyr::select(year = Year, seas, fleet = fleet_num, catch, catch_se) %>%
  dplyr::arrange(fleet, year) %>%
  as.data.frame()

updated.catch.df <- dplyr::bind_rows(mod$dat$catch[which(mod$dat$catch$fleet == 1),], updated.rec.catch.df)
mod$dat$catch <- updated.catch.df


# Length comps 
# Update the rec lengths, negative year when sampels <5, and increase fleet number for fleets after
rec.lengths <- read.csv(here("data", "forSS3", "Lcomps_recreational_FAA_unsexed_raw_10_50.csv")) %>%
  dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = gsub('_', "", fleet)) %>%
  dplyr::mutate(fleet = dplyr::left_join(., 
                                         dplyr::select(fleet.converter %>% dplyr::mutate(joint = tolower(joint)), -fleet), 
                                         by = c("fleet" = "joint"))$fleet_num) %>%
  as.data.frame()
names(rec.lengths) <- names(mod$dat$lencomp)
rec.lengths[which(rec.lengths$Nsamp <= 5), "year"] <- -rec.lengths[which(rec.lengths$Nsamp <= 5), "year"]

#Update previous fleet number for unupdated fleets after the rec fleet
noFAA.lengths <- mod$dat$lencomp[-which(mod$dat$lencomp$fleet %in% c(1, 2)), ]
noFAA.lengths$fleet <- noFAA.lengths$fleet + 1

#Combine lengths together
updated.length.df <- dplyr::bind_rows(mod$dat$lencomp[which(mod$dat$lencomp$fleet == 1),],
                                    rec.lengths,
                                    noFAA.lengths)
mod$dat$lencomp <- updated.length.df


# Age comps
#Just need to update fleet numbers
mod$dat$agecomp[which(mod$dat$agecomp$fleet > 1), "fleet"] <- 
  mod$dat$agecomp[which(mod$dat$agecomp$fleet > 1), "fleet"] + 1


# CPUE data
#Add rec index splits and redo numbering on fleets with greater number
pr_index_n <- read.csv(here("data", "forSS3", "PR_index_forSS_FAS_N.csv")) %>%
  dplyr::mutate(fleet = "recNorth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleet), by = c("fleet" = "joint"))$fleet_num) %>%
  dplyr::rename("se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

pr_index_s <- read.csv(here("data", "forSS3", "PR_index_forSS_FAS_S.csv")) %>%
  dplyr::mutate(fleet = "recSouth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleet), by = c("fleet" = "joint"))$fleet_num) %>%
  dplyr::rename("se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

noFAA.cpue <- mod$dat$CPUE[-which(mod$dat$CPUE$index %in% c(1, 2)), ]
noFAA.cpue$index <- noFAA.cpue$index + 1

mod$dat$CPUE <- dplyr::bind_rows(pr_index_n, pr_index_s, noFAA.cpue)


## Set up the parameters and options based on updated data

# Selectivity tables
mod$ctl$size_selex_types <- rbind(mod$ctl$size_selex_types[1,],
                                  "CA_Recreational_North" = mod$ctl$size_selex_types[2,],
                                  mod$ctl$size_selex_types[-1,])
rownames(mod$ctl$size_selex_types)[3] <- paste0(rownames(mod$ctl$size_selex_types)[3], "_South")

mod$ctl$age_selex_types <- rbind(mod$ctl$age_selex_types[1,],
                                 "CA_Recreational_North" = mod$ctl$age_selex_types[2,],
                                 mod$ctl$age_selex_types[-1,])
rownames(mod$ctl$age_selex_types)[3] <- paste0(rownames(mod$ctl$age_selex_types)[3], "_South")


# Selectivity parameterization
mod$ctl$size_selex_parms <- rbind(mod$ctl$size_selex_parms[1:6,], #com north
                                  mod$ctl$size_selex_parms[7:12,], #rec north
                                  mod$ctl$size_selex_parms[7:12,], #rec south
                                  mod$ctl$size_selex_parms[-c(1:12),])

selex_fleets <- rownames(mod$ctl$size_selex_types)[mod$ctl$size_selex_types$Pattern == 24] |> as.list()
selex_names <- purrr::map(selex_fleets,
                          ~ glue::glue('SizeSel_P_{par}_{fleet_name}({fleet_no})',
                                       par = 1:6,
                                       fleet_name = .x,
                                       fleet_no = fleet.converter$fleet_num[fleet.converter$fleetname == .x])) |> unlist()
rownames(mod$ctl$size_selex_parms) <- selex_names

selex_new <- mod$ctl$size_selex_parms

#Use main block parameters to setup parameter names
selex_tv_pars_blocks <- dplyr::filter(selex_new, Block > 0) |>
  dplyr::select(LO, HI, INIT, PRIOR, PR_SD, PR_type, PHASE, Block) |>
  tidyr::uncount(mod$ctl$blocks_per_pattern[Block], .id = 'id', .remove = FALSE)

#But maintain base model parameterization
selex_tv_pars <- dplyr::bind_rows(mod$ctl$size_selex_parms_tv,
                                  mod$ctl$size_selex_parms_tv[7:9,])
rownames(selex_tv_pars) <- rownames(selex_tv_pars_blocks) |>
  stringr::str_remove('\\.\\.\\.[:digit:]+') |>
  stringr::str_c('_BLK', selex_tv_pars_blocks$Block, 'repl_', mapply("[",mod$ctl$Block_Design[selex_tv_pars_blocks$Block], selex_tv_pars_blocks$id * 2 - 1))

mod$ctl$size_selex_parms_tv <- selex_tv_pars


# Add q setup for surveys with index data
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


# Add variance adjustment factor for rec split and renumber other fleets
mod$ctl$Variance_adjustment_list[which(mod$ctl$Variance_adjustment_list$fleet > 2), "fleet"] <-
  mod$ctl$Variance_adjustment_list[which(mod$ctl$Variance_adjustment_list$fleet > 2), "fleet"] + 1

mod$ctl$Variance_adjustment_list <- dplyr::bind_rows(mod$ctl$Variance_adjustment_list[c(1:2),],
                                                     mod$ctl$Variance_adjustment_list[2,],
                                                     mod$ctl$Variance_adjustment_list[-c(1:2),])
mod$ctl$Variance_adjustment_list[3, "fleet"] <- 3


##
#Output files and run
##

SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all_faa(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'FAA'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()



# ============================================================================ #
# FAAreblock

## Fleets as areas reblock --------------------------------------------------------

#The FAA model doesn't fit recreational south length comps very well. 
#Consider reverting to more complete blocking structure for rec fleets

new_name <- "FAA_resetCom_reblock"

mod <- base_mod


##
#Make Changes
##

#Update fleet information for model, lengths, ages, and indices
mod$dat$Nfleets <- 6
mod$dat$fleetinfo <- rbind(mod$dat$fleetinfo[1,],
                           c("type" = 1, "surveytiming" = -1, "area" = 1, "units" = 1, "need_catch_mult" = 0,
                             "fleetname" = "CA_Recreational_North"),
                           mod$dat$fleetinfo[-1,])
mod$dat$fleetinfo$fleetname[3] <- paste0(mod$dat$fleetinfo$fleetname[3], "_South")

mod$dat$len_info <- rbind(mod$dat$len_info[1,],
                          "CA_Recreational_North" = mod$dat$len_info[2,],
                          mod$dat$len_info[-1,])
rownames(mod$dat$len_info)[3] <- paste0(rownames(mod$dat$len_info)[3], "_South")

mod$dat$age_info <- rbind(mod$dat$age_info[1,],
                          "CA_Recreational_North" = mod$dat$age_info[2,],
                          mod$dat$age_info[-1,])
rownames(mod$dat$age_info)[3] <- paste0(rownames(mod$dat$age_info)[3], "_South")

mod$dat$CPUEinfo <- rbind(mod$dat$CPUEinfo[1,],
                          "CA_Recreational_North" =  mod$dat$CPUEinfo[2,],
                          mod$dat$CPUEinfo[-1,])
rownames(mod$dat$CPUEinfo)[3] <- paste0(rownames(mod$dat$CPUEinfo)[3], "_South")
mod$dat$CPUEinfo$fleet <- c(1:6)

fleet.converter <- mod$dat$fleetinfo %>%
  dplyr::mutate(fleet = c("com", "rec", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::mutate(area = c("All", "North", "South", "All", "All", "All")) %>%
  dplyr::mutate(fleet_num = c(1, 2, 3, 4, 5, 6)) %>%
  dplyr::mutate(joint = paste0(fleet, area)) %>%
  dplyr::select(fleetname, fleet, area, joint, fleet_num)


# Update blocks
#Revert to more detailed blocks for recreational fleets
mod$ctl$N_Block_Designs <- 3
mod$ctl$blocks_per_pattern <- c(2, 4, 3)
mod$ctl$Block_Design <- list(mod$ctl$Block_Design[[1]], #commercial fleet
                             c(2001, 2007, 2008, 2022, 2023, 2023, 2024, 2024), #recreational north fleet
                             c(2001, 2016, 2017, 2022, 2023, 2024)) #recreational south fleet


## Set up the data

# Catches
#Just update the rec catches
catches <- read.csv(here("data", "confidential_noShare", "CAquillback_total_removals_faa.csv"))
catches[is.na(catches)] <- 0

updated.rec.catch.df <- catches %>%
  dplyr::select(c(Year, rec_tot_North, rec_tot_South)) %>%
  tidyr::pivot_longer(cols = -Year, names_to = c('fleet', 'type', 'area'), values_to = 'catch', 
                      names_sep = '_') %>% #ideally I want to separate by second hyphen but this is a workaround
  dplyr::mutate(joint = paste0(fleet, area)) %>%
  dplyr::left_join(fleet.converter %>% dplyr::select(joint, fleet_num), by = c("joint" = "joint")) %>%
  dplyr::mutate(seas = 1, 
                catch_se = 0.05) %>%
  dplyr::select(year = Year, seas, fleet = fleet_num, catch, catch_se) %>%
  dplyr::arrange(fleet, year) %>%
  as.data.frame()

updated.catch.df <- dplyr::bind_rows(mod$dat$catch[which(mod$dat$catch$fleet == 1),], updated.rec.catch.df)
mod$dat$catch <- updated.catch.df


# Length comps 
# Update the rec lengths, negative year when sampels <5, and increase fleet number for fleets after
rec.lengths <- read.csv(here("data", "forSS3", "Lcomps_recreational_FAA_unsexed_raw_10_50.csv")) %>%
  dplyr::select(-Nsamp) %>%
  dplyr::mutate(fleet = gsub('_', "", fleet)) %>%
  dplyr::mutate(fleet = dplyr::left_join(., 
                                         dplyr::select(fleet.converter %>% dplyr::mutate(joint = tolower(joint)), -fleet), 
                                         by = c("fleet" = "joint"))$fleet_num) %>%
  as.data.frame()
names(rec.lengths) <- names(mod$dat$lencomp)
rec.lengths[which(rec.lengths$Nsamp <= 5), "year"] <- -rec.lengths[which(rec.lengths$Nsamp <= 5), "year"]

#Update previous fleet number for unupdated fleets after the rec fleet
noFAA.lengths <- mod$dat$lencomp[-which(mod$dat$lencomp$fleet %in% c(1, 2)), ]
noFAA.lengths$fleet <- noFAA.lengths$fleet + 1

#Combine lengths together
updated.length.df <- dplyr::bind_rows(mod$dat$lencomp[which(mod$dat$lencomp$fleet == 1),],
                                      rec.lengths,
                                      noFAA.lengths)
mod$dat$lencomp <- updated.length.df


# Age comps
#Just need to update fleet numbers
mod$dat$agecomp[which(mod$dat$agecomp$fleet > 1), "fleet"] <- 
  mod$dat$agecomp[which(mod$dat$agecomp$fleet > 1), "fleet"] + 1


# CPUE data
#Add rec index splits and redo numbering on fleets with greater number
pr_index_n <- read.csv(here("data", "forSS3", "PR_index_forSS_FAS_N.csv")) %>%
  dplyr::mutate(fleet = "recNorth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleet), by = c("fleet" = "joint"))$fleet_num) %>%
  dplyr::rename("se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

pr_index_s <- read.csv(here("data", "forSS3", "PR_index_forSS_FAS_S.csv")) %>%
  dplyr::mutate(fleet = "recSouth") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleet), by = c("fleet" = "joint"))$fleet_num) %>%
  dplyr::rename("se_log" = logse,
                "index" = fleet) %>%
  as.data.frame()

noFAA.cpue <- mod$dat$CPUE[-which(mod$dat$CPUE$index %in% c(1, 2)), ]
noFAA.cpue$index <- noFAA.cpue$index + 1

mod$dat$CPUE <- dplyr::bind_rows(pr_index_n, pr_index_s, noFAA.cpue)


## Set up the parameters and options based on updated data

# Selectivity tables
mod$ctl$size_selex_types <- rbind(mod$ctl$size_selex_types[1,],
                                  "CA_Recreational_North" = mod$ctl$size_selex_types[2,],
                                  mod$ctl$size_selex_types[-1,])
rownames(mod$ctl$size_selex_types)[3] <- paste0(rownames(mod$ctl$size_selex_types)[3], "_South")

mod$ctl$age_selex_types <- rbind(mod$ctl$age_selex_types[1,],
                                 "CA_Recreational_North" = mod$ctl$age_selex_types[2,],
                                 mod$ctl$age_selex_types[-1,])
rownames(mod$ctl$age_selex_types)[3] <- paste0(rownames(mod$ctl$age_selex_types)[3], "_South")


# Selectivity parameterization
mod$ctl$size_selex_parms <- rbind(mod$ctl$size_selex_parms[1:6,], #com north
                                  mod$ctl$size_selex_parms[7:12,], #rec north
                                  mod$ctl$size_selex_parms[7:12,], #rec south
                                  mod$ctl$size_selex_parms[-c(1:12),])

selex_fleets <- rownames(mod$ctl$size_selex_types)[mod$ctl$size_selex_types$Pattern == 24] |> as.list()
selex_names <- purrr::map(selex_fleets,
                          ~ glue::glue('SizeSel_P_{par}_{fleet_name}({fleet_no})',
                                       par = 1:6,
                                       fleet_name = .x,
                                       fleet_no = fleet.converter$fleet_num[fleet.converter$fleetname == .x])) |> unlist()
rownames(mod$ctl$size_selex_parms) <- selex_names

mod$ctl$size_selex_parms[intersect(grep("CA_Recreational_South", rownames(mod$ctl$size_selex_parms)),
                                   which(mod$ctl$size_selex_parms$Block == 2)), "Block"] <- 3

#Allow domed shaped back for rec parameter 4 for SOUTH only
#Explorations with domed for north suggest asymptotic for all blocks
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Recreational_South(3)", c("HI", "INIT", "PHASE")] <-
  mod$ctl$size_selex_parms["SizeSel_P_3_CA_Recreational_South(3)", c("HI", "INIT", "PHASE")]
# mod$ctl$size_selex_parms["SizeSel_P_4_CA_Recreational_North(2)", c("HI", "INIT", "PHASE")] <-
#   mod$ctl$size_selex_parms["SizeSel_P_3_CA_Recreational_North(2)", c("HI", "INIT", "PHASE")]


#Set up time varying parameters for rec only (keep origian tv parms for commercial)
selex_new <- mod$ctl$size_selex_parms

selex_tv_pars <- dplyr::filter(selex_new, Block > 0) |>
  dplyr::select(LO, HI, INIT, PRIOR, PR_SD, PR_type, PHASE, Block) |>
  tidyr::uncount(mod$ctl$blocks_per_pattern[Block], .id = 'id', .remove = FALSE)

rownames(selex_tv_pars) <- rownames(selex_tv_pars) |>
  stringr::str_remove('\\.\\.\\.[:digit:]+') |>
  stringr::str_c('_BLK', selex_tv_pars$Block, 'repl_', mapply("[",mod$ctl$Block_Design[selex_tv_pars$Block], selex_tv_pars$id * 2 - 1))

updated.selex.tv.df <- dplyr::bind_rows(mod$ctl$size_selex_parms_tv[grep('Commercial', rownames(mod$ctl$size_selex_parms_tv)),],
                                        selex_tv_pars[grep('Recreational', rownames(selex_tv_pars)),])
mod$ctl$size_selex_parms_tv <- updated.selex.tv.df %>%
  dplyr::select(-Block, -id)


# Add q setup for surveys with index data
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


# Add variance adjustment factor for rec split and renumber other fleets
mod$ctl$Variance_adjustment_list[which(mod$ctl$Variance_adjustment_list$fleet > 2), "fleet"] <-
  mod$ctl$Variance_adjustment_list[which(mod$ctl$Variance_adjustment_list$fleet > 2), "fleet"] + 1

mod$ctl$Variance_adjustment_list <- dplyr::bind_rows(mod$ctl$Variance_adjustment_list[c(1:2),],
                                                     mod$ctl$Variance_adjustment_list[2,],
                                                     mod$ctl$Variance_adjustment_list[-c(1:2),])
mod$ctl$Variance_adjustment_list[3, "fleet"] <- 3


##
#Output files and run
##

SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all_faa(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', 'FAA_resetCom_reblock')))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'FAA full block'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()


# ============================================================================ #
# FAAreweight

## Fleets as areas reweight --------------------------------------------------------

#Reweight the FAA sensitivity run starting from 1

new_name <- "FAA_resetCom_reblock_reweight"
old_name <- "FAA_resetCom_reblock"

copy_SS_inputs(dir.old = here(sens_dir, old_name), 
               dir.new = here(sens_dir, new_name),
               overwrite = TRUE)

mod <- SS_read(here(sens_dir, new_name))

mod$ctl$Variance_adjustment_list$value <- 1

SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

#Reweight
pp <- SS_output(here(sens_dir, new_name))
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here(sens_dir, new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = 3, 
                       extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all_faa(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', "FAA_resetCom_reblock"),
                                                   file.path('_sensitivities', "FAA_resetCom_reblock_reweight")))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'FAA reblock',
                                     'FAA reblock reweight'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()

#Reweighted south length fits are really off for 2017-2021 when reweighting. 
#Selectivity shifts pretty far right in the north and the south selex for 2017-2021 is 
#really off. I dont trust the reweighted version. When setting all weights to 1
#the model is much more well behaved. Overall, doesn't seem to suggest wide differences
#at existing weights with non-FAA model. Early explorations should reweighting didn't
#give as large of differences and that was done with reweighting first and then
#updating blocks. 


# ============================================================================ #
# marginalComAge

## Replace commercial CAAL with marginal age comps --------------------------------------------------------

new_name <- 'marginalComAge'

mod <- base_mod

com.marg <- read.csv(here("data", "forSS3", "Acomps_PacFIN_unsexed_expanded_1_60.csv")) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = "com") %>%
  dplyr::mutate(fleet = dplyr::left_join(., dplyr::select(fleet.converter, -fleetname))$fleet_no_num) %>%
  as.data.frame()
names(com.marg) <- names(mod$dat$agecomp)
#Negative year years with < 10 Nsamp
com.marg[which(com.marg$Nsamp <= 10), "year"] <- -com.marg[which(com.marg$Nsamp <= 10), "year"]

new.age.df <- dplyr::bind_rows(com.marg, mod$dat$agecomp[which(mod$dat$agecomp$fleet > 1),])
mod$dat$agecomp <- new.age.df


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Marginal commercial age comps'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()


# ============================================================================ #
# noNegYear

## Use all data: Replace all negative year with positive --------------------------------------------------------

new_name <- 'noNegYear'

mod <- base_mod

mod$dat$agecomp[which(mod$dat$agecomp$year < 0), "year"] <- -mod$dat$agecomp[which(mod$dat$agecomp$year < 0), "year"]
mod$dat$lencomp[which(mod$dat$lencomp$year < 0), "year"] <- -mod$dat$lencomp[which(mod$dat$lencomp$year < 0), "year"]


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Marginal commercial age comps'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()


# ============================================================================ #
# noAgeErr

## Remove ageing error --------------------------------------------------------

new_name <- 'noAgeErr'

mod <- base_mod

mod$dat$ageerror[1,] <- -1
mod$dat$ageerror[2,] <- 0.01

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'No ageing error'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()


# ============================================================================ #
# Biology sensitivities -----
# ============================================================================ #

# ============================================================================ #
# fecundity_EJest


### Fecundity to E.J.'s Pteropodus values' ----
new_name <- 'fecundity_EJest'

mod <- base_mod

#change fixed values
eggs_b <- 3.702
eggs_a <- 3.93e-07
fec_prior <- 0

mod$ctl$MG_parms['Eggs_alpha_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(-3, 3, eggs_a, eggs_a, 0, fec_prior, -9)
mod$ctl$MG_parms['Eggs_beta_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(1, 7, eggs_b, eggs_b, 0, fec_prior, -9)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Fecundity fixed EJ est.'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
# Maturity_2021est

### Maturity to 2021 estimates ----

new_name <- 'Maturity_2021est'

mod <- base_mod

#change fixed values
l50_fxn <- 29.23 #-a/b
l50_se <- 5 #se of -a/b
slope_fxn <- -0.8 #b (SS3 manual says this must be negative)
slope_se <- 0.4 #se of b
mat_prior <- 0 #making it normal

mod$ctl$MG_parms['Mat50%_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(25, 32, l50_fxn, l50_fxn, l50_se, mat_prior, -9)
mod$ctl$MG_parms['Mat_slope_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(-1.0, 0, slope_fxn, slope_fxn, slope_se, mat_prior, -9)


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Maturity set to 2021 est.'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
# Natural mortality and steepness ----
# ============================================================================ #


# ============================================================================ #
# Estimate_M

## Estimate M --------------------------------------------------------
new_name <- 'Estimate_M'

mod <- base_mod

#change values
mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c( 'PHASE')] <- 2

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Estimate M'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
# maxAge70

## Max Age 70 --------------------------------------------------------
new_name <- 'maxAge70'

mod <- base_mod

#change values
maxAge <- 70 
m_init <- round(5.4/maxAge, 4)
m_se <- 0.31
m_prior <- 3 #log-normal

mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.15, m_init, round(log(m_init), 2), m_se, m_prior, -2)


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Max age 70'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
# maxAge75

## Max Age 75 --------------------------------------------------------
new_name <- 'maxAge75'

mod <- base_mod

#change values
maxAge <- 75 
m_init <- round(5.4/maxAge, 4)
m_se <- 0.31
m_prior <- 3 #log-normal

mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.15, m_init, round(log(m_init), 2), m_se, m_prior, -2)


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Max age 75'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
# maxAge80

## Max Age 80 --------------------------------------------------------
new_name <- 'maxAge80'

mod <- base_mod

#change values
maxAge <- 80 
m_init <- round(5.4/maxAge, 4)
m_se <- 0.31
m_prior <- 3 #log-normal

mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.15, m_init, round(log(m_init), 2), m_se, m_prior, -2)


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Max age 80'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
# estimate_h

## Estimate h --------------------------------------------------------
new_name <- 'estimate_h'

mod <- base_mod

#change values
mod$ctl$SR_parms['SR_BH_steep', c('PHASE')] <- 4


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Estimate h'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))

# ============================================================================ #
# estimate_M_and_h

## Estimate M and h --------------------------------------------------------
new_name <- 'estimate_M_and_h'

mod <- base_mod

#change values
mod$ctl$SR_parms['SR_BH_steep', c('PHASE')] <- 4
mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c( 'PHASE')] <- 2

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
         # extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)




xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Estimate M and h'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))

#========================================================================================#
# Selectivity Alternatives -----
#========================================================================================#

## Alternative Commercial Blocks --------------------------------------------------------

new_name <- 'AltComBlocks'

mod <- base_mod

mod$ctl$blocks_per_pattern <- c(2, 1)
mod$ctl$Block_Design <- list(c(2003, 2022, 2023, 2024), #commercial fleet
                             c(2017, 2024)) #recreational fleet

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

## No Blocks --------------------------------------------------------

new_name <- 'NoBlocks'

mod <- base_mod

mod$ctl$N_Block_Designs <- 0
#mod$ctl$N_Block_Designs <- paste0("#",mod$ctl$blocks_per_pattern)
mod$ctl$size_selex_parms$Block  = 0
mod$ctl$size_selex_parms$Block_Fxn  = 0
mod$ctl$size_selex_parms_tv <- 0

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

## ROV and CCFRP Domed --------------------------------------------------------

new_name <- 'ROVandCCFRPDomed'

mod <- base_mod

mod$ctl$size_selex_parms[intersect(grep("CCFRP", rownames(mod$ctl$size_selex_parms)),
                                   grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, 4)


mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
                                   grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, 4)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name),
                                                   file.path('_sensitivities', "AltComBlocks"),
                                                   file.path('_sensitivities', "NoBlocks")))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'ROV & CCFRP Domed', "Simplify Commercial Blocks", "No Blocks"),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name))

## More Rec Blocks with Early Period Asymptotic --------------------------------------------------------
# Leave CCFRP and ROV asymptotic

new_name <- 'EarlyRecAsymp'

mod <- base_mod

mod[["ctl"]][["blocks_per_pattern"]][["blocks_per_pattern_2"]] <- 3
mod[["ctl"]][["Block_Design"]][[2]] <- c(2001,2016,2017,2022,2023,2024)

tv <- mod[["ctl"]][["size_selex_parms_tv"]]
tv <- rbind(tv, tv[rep(7, 2),])
tv <- tv[c(1:6, 10,11, 7:9),]
tv <- rbind(tv, tv[rep(10, 2),])
tv <- tv[c(1:9, 12,13, 10,11),]
tv <- rbind(tv, tv[rep(13, 2),])
row.names(tv)[7:15] <- c("SizeSel_P_1_CA_Recreational(2)_BLK2repl_2001", "SizeSel_P_1_CA_Recreational(2)_BLK2repl_2017", "SizeSel_P_1_CA_Recreational(2)_BLK2repl_2023", "SizeSel_P_3_CA_Recreational(2)_BLK2repl_2001", "SizeSel_P_3_CA_Recreational(2)_BLK2repl_2017", "SizeSel_P_3_CA_Recreational(2)_BLK2repl_2023", "SizeSel_P_4_CA_Recreational(2)_BLK2repl_2001", "SizeSel_P_4_CA_Recreational(2)_BLK2repl_2017", "SizeSel_P_4_CA_Recreational(2)_BLK2repl_2023")

tv[13:15,2] <- 9
tv[13:15,3] <- 5.54518
tv[13:15,7] <- 5

mod[["ctl"]][["size_selex_parms_tv"]] <- tv

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'More Rec Blocks'),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name))

## More Rec Blocks with Early Period Asymptotic --------------------------------------------------------
# Leave CCFRP and ROV asymptotic

new_name <- 'EarlyRecAsympDomeSurveys'

mod <- SS_read(here('models', '_sensitivities', 'EarlyRecAsymp'))

mod[["ctl"]][["size_selex_parms"]][["PHASE"]][16] <- 5 # Turn on estimation of parameter 4 for CCFRP
mod[["ctl"]][["size_selex_parms"]][["PHASE"]][22] <- 5 # Turn on estimation of parameter 4 for ROV

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', 'EarlyRecAsymp'),
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Late Rec Blocks Domed',
                                     'Late Rec Blocks & Surveys Domed'),
                    subplots = c(1:3), print = TRUE, plotdir = here(sens_dir, new_name))

## Commercial Asymptotic --------------------------------------------------------
#Asymptotic for final block (keeping earliest block as domed)

new_name <- 'ComAsymp'

mod <- base_mod

mod[["ctl"]][["size_selex_parms"]][["PHASE"]][4] <- -5
mod[["ctl"]][["size_selex_parms"]][["INIT"]][4] <- 15

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Commercial Asymptotic'),
                    subplots = c(1:3), print = TRUE, plotdir = here(sens_dir, new_name))


## Commercial All Asymptotic --------------------------------------------------------
#Asymptotic for all blocks

new_name <- 'ComAsymp_allBlocks'

mod <- base_mod

mod$ctl$size_selex_parms["SizeSel_P_4_CA_Commercial(1)", "PHASE"] <- -5
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Commercial(1)", "INIT"] <- 15

mod$ctl$size_selex_parms_tv["SizeSel_P_4_CA_Commercial(1)_BLK1repl_1916", 
                            c("HI", "INIT", "PHASE")] <- c(20, 15, -5)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Commercial Asymptotic All blocks'),
                    subplots = c(1:3), print = TRUE, plotdir = here(sens_dir, new_name))

##Compare likelihoods of selectivity sensitivities - for testing purposes
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("4_2_1_propBase",
                                                 file.path("_sensitivities", "AltComBlocks"),
                                                 file.path("_sensitivities", "NoBlocks"),
                                                 file.path("_sensitivities", "ROVandCCFRPDomed"),
                                                 file.path("_sensitivities", "EarlyRecAsymp"),
                                                 file.path("_sensitivities", "EarlyRecAsympDomeSurveys"),
                                                 file.path("_sensitivities", "ComAsymp"),
                                                 file.path("_sensitivities", "ComAsymp_allBlocks"))))

#Compare likelihoods
xx.sum <- SSsummarize(xx)
xx.tab <- SStableComparisons(xx.sum, 
                             modelnames = c("Base",
                                            "Alt com blocks",
                                            "No blocks",
                                            "ROV and CCFRP Domed",
                                            "Alt rec blocks domed",
                                            "Alt rec blocks, ROV and CCFRP domed",
                                            "Last com block asymptotic",
                                            "All com block asymptotic")) |>
  dplyr::mutate(across(-Label, ~ round(., 2))) |>
  dplyr::rename(' ' = Label)
xx.val <- rbind(xx.sum$npars, xx.tab[,-1]) #add number of parameters
rownames(xx.val) <- c("Npars", xx.tab[,1]) #add rownames so dataframe stays numerical
write.csv(t(xx.val[1:5,]), here(sens_dir, new_name, 'like_comp.csv'), row.names = TRUE)

#========================================================================================
# ROV Absolute Abundance -----
#========================================================================================

## Fix M, h, and growth at base values.  Apply a multiplier to catch. --------------------------------------

new_name <- 'ROV_Abs_1'

mod <- base_mod

mod[["ctl"]][["MG_parms"]][["PHASE"]][2:6] <- -3

mod[["ctl"]][["MG_parms"]][["INIT"]][2:6] <- c(9.8983100, 42.7777000, 0.1256130, 0.1823610, 0.0862424)

# I estimated the proportion of quillback habitat in MPAs to be 0.25.  Need to check with Melissa on this.
# Tanya estimated 298559 fish in MPAs in 2020.
# Expanding that to all habitat results in 1194236 fish.
# Summary biomass in numbers of fish in 2020 is estimated by the base model to be 408690. 

pp <- SS_output(here('models', '4_2_1_propBase'))
numbers_at_age <- pp$natage
pp$natageOnePlus_numbers <- numbers_at_age %>%
  filter(`Beg/Mid` == "M") %>% #taking that mid year since that represents the survey
  mutate(numberOfFish = rowSums(across(c("3":"80")))) %>%  #could also look at ages 2+
  dplyr::select(c("Time", "numberOfFish"))

# Using the code above from Melissa to look at numbers of fish in the middle of the year,
# we get 389656 fish in 2020.  

# The ROV estimate is 3.065 times the model estimate.  
# See what happens to summary biomass when we multiply catches x 5.

mod[["dat"]][["catch"]][["catch"]] <- mod[["dat"]][["catch"]][["catch"]] * 5

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))

numbers_at_age <- pp$natage
pp$natageOnePlus_numbers <- numbers_at_age %>%
  filter(`Beg/Mid` == "M") %>% #taking that mid year since that represents the survey
  mutate(numberOfFish = rowSums(across(c("3":"80")))) %>%  #could also look at ages 2+
  dplyr::select(c("Time", "numberOfFish"))

# Try increasing catch x 3
mod <- base_mod
mod[["ctl"]][["MG_parms"]][["PHASE"]][2:6] <- -3
mod[["ctl"]][["MG_parms"]][["INIT"]][2:6] <- c(9.8983100, 42.7777000, 0.1256130, 0.1823610, 0.0862424)
mod[["dat"]][["catch"]][["catch"]] <- mod[["dat"]][["catch"]][["catch"]] * 3

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))

# Multiplying catches by 3 gets us close with a biomass in mid-year 2020 of 1168857

## Find values of M, h, and growth that produce survey estimate without adjusting base catch ----------------------
# Quick way by just replacing ROV index values

new_name <- 'ROV_Abs_2'

mod <- base_mod

mod[["dat"]][["CPUE"]][["obs"]][27:28] <- c(155225, 298559)
mod[["dat"]][["CPUE"]][["se_log"]][27:28] <- c(0.118, 0.0666)

mod[["ctl"]][["MG_parms"]][1,7] <- 2  # Allow M to be estimated

mod[["ctl"]][["Q_parms"]][3,3] <- 1 # Set ROV index q=1 and fix
mod[["ctl"]][["Q_parms"]][3,7] <- -2

# Run and see what this does to M and growth params

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
