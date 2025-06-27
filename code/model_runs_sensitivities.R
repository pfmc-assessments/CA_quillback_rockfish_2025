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
source(here('code/model_runs_growth_comparison.R'))

#Enter in base model from which to base sensitivities
#base_mod_name <-'3_2_2_SetUpExtraSE' #<---------------UPDATE WHEN CHANGE
#base_mod_name <- '4_2_1_propBase' #<---------------UPDATE WHEN CHANGE
base_mod_name <- '5_1_3_preStarBase' #<---------------UPDATE WHEN CHANGE
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

######-
### leaveOut_rec_lengths  -----------------------------

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

######-
### leaveOut_com_lengths ---------------------------------

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

######-
### leaveOut_fleet_lengths ---------------------------------

# Remove recreational and commercial lengths using lambdas

new_name <- 'leaveOut_fleet_lengths'

mod <- base_mod


# Create a lambda section 
lambdas <- data.frame("like_comp" = c(4, 4), #length comps
                      "fleet" = c(1, 2),
                      "phase" = c(1, 1),
                      "value" = c(0, 0),
                      "sizefreq_method" = c(1, 1))
rownames(lambdas) <- c("lenComp_CA_Recreational", "lenComp_CA_Commercial")

mod$ctl$N_lambdas <- nrow(lambdas)
mod$ctl$lambdas <- lambdas


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
                                     'Remove com and rec lengths'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))



# ============================================================================ #
## Drop age data by fleet ----
# ============================================================================ #

######-
### leaveOut_com_ages --------------------------------

# Commercial ages

new_name <- 'leaveOut_com_ages'

mod <- base_mod

#Set all com ages to negative year
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

######-
### leaveOut_growth_ages ------------------------------------

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


######-
### leaveOut_all_ages ---------------------------------

# Remove commercial and CAAL ages using lambdas

new_name <- 'leaveOut_all_ages'

mod <- base_mod


# Create a lambda section 
lambdas <- data.frame("like_comp" = c(5, 5), #age comps
                      "fleet" = c(1, 3),
                      "phase" = c(1, 1),
                      "value" = c(0, 0),
                      "sizefreq_method" = c(1, 1))
rownames(lambdas) <- c("CAAL_CA_Commercial", "CAAL_CA_Growth")

mod$ctl$N_lambdas <- nrow(lambdas)
mod$ctl$lambdas <- lambdas


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
                                     'Remove com and growth caal'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


# ============================================================================ #
## Drop index fleets ----
# ============================================================================ #

######-
### Drop CCFRP ------------------------------------

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

######-
### Drop ROV ---------------------------------

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


######-
### Drop PR index (example for dropping by using lambda) ---------------------------------

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


######-
### Drop all indices ---------------------------------

# Remove recreationals, ROV, and CCFRP indices using lambdas

new_name <- 'leaveOut_all_indices'

mod <- base_mod


# Create a lambda section 
lambdas <- data.frame("like_comp" = c(1, 1, 1), #length comps
                      "fleet" = c(2, 4, 5),
                      "phase" = c(1, 1, 1),
                      "value" = c(0, 0, 0),
                      "sizefreq_method" = c(1, 1, 1))
rownames(lambdas) <- c("Survey_CA_Recreational", "Survey_CCFRP", "Survey_ROV")

mod$ctl$N_lambdas <- nrow(lambdas)
mod$ctl$lambdas <- lambdas


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
                                     'Remove all indices'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))



####------------------------------------------------#
# Catch sensitivities ----
####------------------------------------------------#

######-
## Increase Catch SE --------------------------------------------------------

new_name <- 'catchIncreaseSE'

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


######-
## Increase Catch SE More and Just < 1980 --------------------------------------------------------

new_name <- 'catchIncreaseSE_0.5pre1980'

mod <- base_mod

mod$dat$catch[mod$dat$catch$year < 1980, "catch_se"] <- 0.5

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
                                     'Increase Catch SE 0.5 pre 1980'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))



######-
## Adjusting High Catch Outliers --------------------------------------------------------

new_name <- 'catchOutliers'

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
# These calculations are done (in commented out lines) in the catches.R script

years <- c(1983, 1990, 1991, 1992, 1993)
catch$catch[catch$fleet==2 & catch$year %in% years] <- c(10.053, 5.688, 5.706, 5.724, 5.182)
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


######-
## Catch smoother --------------------------------------------------------

new_name <- 'catchSmooth'

mod <- base_mod

catch <- mod$dat$catch

#Moving average function. Does not set values at bounds
ma <- function(x, n = 5) { 
  stats::filter(x, rep(1 / n, n), sides = 2)
}

#Test smooth
plot(catch[which(catch$fleet == 1), "catch"])
lines(ma(catch[which(catch$fleet == 1), "catch"], n =5), col = 2)

com_smooth <- ma(catch[which(catch$fleet == 1), "catch"], n =5)
rec_smooth <- ma(catch[which(catch$fleet == 2), "catch"], n =5)

#Use values from original time series for first two and last two years
smooth_catch <-c(catch[which(catch$fleet == 1), "catch"][1:2],
                 com_smooth[c(3:107)],
                 catch[which(catch$fleet == 1), "catch"][108:109],
                 catch[which(catch$fleet == 2), "catch"][1:2],
                 rec_smooth[c(3:107)],
                 catch[which(catch$fleet == 2), "catch"][108:109])

#Test to see if working
plot(catch$catch, type = "o")
lines(smooth_catch, col = 2)

mod$dat$catch$catch <- round(smooth_catch, 3)


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
                                     'Five year moving average for catch'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))


####------------------------------------------------#
# Data weighting sensitivities ----
####------------------------------------------------#

######-
## Data weighting - Dirichlet --------------------------------------------------------

new_name <- 'dw_dirichlet'

mod <- base_mod

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))

dw <- r4ss::tune_comps(replist = pp, 
                       option = 'DM', 
                       dir = here(sens_dir, new_name), 
                       exe = here('models/ss3_win.exe'),
                       niters_tuning = 1, 
                       extras = '-nohess',
                       show_in_console = TRUE)

pp <- SS_output(here(sens_dir, new_name))
pp$Dirichlet_Multinomial_pars
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

#DM weights are nearly all close to 1


######-
## Data weighting - MI --------------------------------------------------------

new_name <- 'dw_MI'

mod <- base_mod

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))

#Just one iteration
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'MI', 
                       dir = here(sens_dir, new_name), 
                       exe = here('models/ss3_win.exe'),
                       niters_tuning = 1, 
                       extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', "dw_dirichlet"),
                                                   file.path('_sensitivities', "dw_MI")))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Dirichlet data weighting',
                                     'MI weighting'),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name))



####------------------------------------------------#
# Other sensitivities ----
####------------------------------------------------#

######-
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
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))

######-
## Add extra SE to indices --------------------------------------------------------------------

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



######-
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



######-
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
  dplyr::left_join(fleet.converter %>% dplyr::select(joint, fleet_num), by = c("joint"="joint")) %>%
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



######-
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
#at existing weights with non-FAA model. Early explorations showed reweighting didn't
#give as large of differences and that was done with reweighting first and then
#updating blocks. 


########-
## Replace commercial CAAL with marginal age comps --------------------------------------------------------

new_name <- 'marginalComAge'

mod <- base_mod

#Reset fleet.converter
fleet.converter <- base_mod$dat$fleetinfo %>%
  dplyr::mutate(fleet_no_num = 1:5,
                fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::select(fleetname, fleet_no_num, fleet)

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


######-
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



######-
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


######-
## Remove small sample com length comps --------------------------------------------------------

#Remove sample sizes with N < 10 in commercial length comps makes fit look better
#even though its functionally the same

new_name <- 'comLenSampleSize'

mod <- base_mod

mod$dat$lencomp[which(mod$dat$lencomp$Nsamp < 10  & 
                        mod$dat$lencomp$fleet == 1), "year"] <-
  -abs(mod$dat$lencomp[which(mod$dat$lencomp$Nsamp < 10  & 
                               mod$dat$lencomp$fleet == 1), "year"])

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
                                     'Remove com lengths with sample size < 10'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()


# ============================================================================ #
# Biology sensitivities -----
# ============================================================================ #

######-
## Fecundity to E.J.'s Pteropodus values' ----------
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


######-
## Maturity to 2021 estimates ----

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


######-
## Growth to 2021 estimates ----

new_name <- 'Growth_2021est'

mod <- base_mod

mod$ctl$MG_parms['L_at_Amin_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(8.23, 8.23, -3)
mod$ctl$MG_parms['L_at_Amax_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(43.04, 43.04, -3)
mod$ctl$MG_parms['VonBert_K_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(0.199, 0.199, -3)
mod$ctl$MG_parms['CV_young_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(0.1, 0.1, -3)
mod$ctl$MG_parms['CV_old_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(0.1, 0.1, -3)

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
                                     'Growth set to 2021 est.'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()


## Growth to 2021 estimates without ages ----

new_name <- 'Growth_2021est_noAges'

mod <- base_mod

mod$ctl$MG_parms['L_at_Amin_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(8.23, 8.23, -3)
mod$ctl$MG_parms['L_at_Amax_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(43.04, 43.04, -3)
mod$ctl$MG_parms['VonBert_K_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(0.199, 0.199, -3)
mod$ctl$MG_parms['CV_young_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(0.1, 0.1, -3)
mod$ctl$MG_parms['CV_old_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(0.1, 0.1, -3)

mod$dat$agecomp$year <- -abs(mod$dat$agecomp$year)

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
                                                   file.path('_sensitivities', 'Growth_2021est'),
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Growth fixed to 2021 est.',
                                     'Growth fixed to 2021 est. and no ages'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()


######-
## Fix L1 to 8 ----

new_name <- 'Fix_L1_8'

mod <- base_mod

mod$ctl$MG_parms['L_at_Amin_Fem_GP_1', c('INIT', 'PRIOR', 'PHASE')] <- c(8, 8, -3)


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
                                     'L1 fixed at 8'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))
dev.off()

# ============================================================================ #
# Natural mortality and steepness ----
# ============================================================================ #

######-
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

######-
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



######-
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


######-
## Max Age 84 --------------------------------------------------------
new_name <- 'maxAge84'

mod <- base_mod

#change values
maxAge <- 84 
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
                                     'Max age 84'),
                    subplots = c(1,3), print = TRUE, plotdir = here(sens_dir, new_name))



######-
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

######-
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

numbers_at_age <- pp$natage
pp$natageOnePlus_numbers <- numbers_at_age %>%
  filter(`Beg/Mid` == "M") %>% #taking that mid year since that represents the survey
  mutate(numberOfFish = rowSums(across(c("3":"80")))) %>%  #could also look at ages 2+
  dplyr::select(c("Time", "numberOfFish"))


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

######-
## Alternative Commercial Blocks --------------------------------------------------------

new_name <- 'sel_AltComBlocks'

mod <- base_mod

mod$ctl$blocks_per_pattern <- c(2, 1)
mod$ctl$Block_Design <- list(c(2003, 2022, 2023, 2024), #commercial fleet
                             c(2017, 2024)) #recreational fleet

#main block is now the first so set that to be domed (based on previous INIT)
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Commercial(1)", c("INIT", "PHASE")] <- c(5.48064, 5)

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

######-
## Alternative Commercial Blocks in 2018 instead of 2014 --------------------------------------------------------

#This is based on looking back at discussions with thompson and budrick suggesting
#2017/2018 is a better block than 2014. Likelihood is not as good however. 

new_name <- 'sel_AltComBlocks_2018'

mod <- base_mod

mod$ctl$Block_Design <- list(c(1916, 2002, 2018, 2021), #commercial fleet
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


######-
## No Blocks --------------------------------------------------------

new_name <- 'sel_NoBlocks'

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

######-
## ROV and CCFRP Domed --------------------------------------------------------

new_name <- 'sel_ROVandCCFRPDomed'

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
                                                   file.path('_sensitivities', "sel_AltComBlocks"),
                                                   file.path('_sensitivities', "sel_NoBlocks")))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'ROV & CCFRP Domed', "Simplify Commercial Blocks", "No Blocks"),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name))

######-
## ROV and CCFRP and Rec Domed --------------------------------------------------------

new_name <- 'sel_ROVandCCFRPandRecDomed'

mod <- base_mod

mod$ctl$size_selex_parms[intersect(grep("CCFRP", rownames(mod$ctl$size_selex_parms)),
                                   grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, 4)


mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
                                   grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, 4)

#Setting init for p4 for rec to its original from model 322
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Recreational(2)", 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.54518, 5)
mod$ctl$size_selex_parms_tv["SizeSel_P_4_CA_Recreational(2)_BLK2repl_2017",
                            c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.54518, 5)


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


######-
## All Domed --------------------------------------------------------

new_name <- 'sel_AllDomed'

mod <- base_mod

mod$ctl$size_selex_parms[intersect(grep("CCFRP", rownames(mod$ctl$size_selex_parms)),
                                   grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, 4)

mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
                                   grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, 4)

#Setting init for p4 for rec to its original from model 322
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Recreational(2)", 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.54518, 5)
mod$ctl$size_selex_parms_tv["SizeSel_P_4_CA_Recreational(2)_BLK2repl_2017",
                            c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.54518, 5)

#Setting init for p4 for com to its original from model 322
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Commercial(1)", 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.48064, 5)
mod$ctl$size_selex_parms_tv["SizeSel_P_4_CA_Commercial(1)_BLK1repl_2014",
                            c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.48064, 5)


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


######-
## Just Com Domed --------------------------------------------------------

new_name <- 'sel_ComDomed'

mod <- base_mod

#Setting init for p4 for com to its original from model 322
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Commercial(1)", 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.48064, 5)
mod$ctl$size_selex_parms_tv["SizeSel_P_4_CA_Commercial(1)_BLK1repl_2014",
                            c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.48064, 5)


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


######-
## Just Rec Domed --------------------------------------------------------

new_name <- 'sel_RecDomed'

mod <- base_mod

#Setting init for p4 for rec to its original from model 322
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Recreational(2)", 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.54518, 5)
mod$ctl$size_selex_parms_tv["SizeSel_P_4_CA_Recreational(2)_BLK2repl_2017",
                            c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.54518, 5)


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
                                                   file.path('_sensitivities', "sel_ROVandCCFRPDomed"),
                                                   file.path('_sensitivities', "sel_RecDomed"),
                                                   file.path('_sensitivities', "sel_ROVandCCFRPandRecDomed"),
                                                   file.path('_sensitivities', "sel_ComDomed"),
                                                   file.path('_sensitivities', "sel_AllDomed")))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'ROV & CCFRP Domed',
                                     'Rec Domed',
                                     "ROV & CCFRP & Rec Domed",
                                     "Com Domed",
                                     'All Domed'),
                    subplots = c(1:3), print = TRUE, plotdir = here(sens_dir, new_name))


######-
## More Rec Blocks with Early Period Asymptotic --------------------------------------------------------
# Leave CCFRP and ROV asymptotic

new_name <- 'sel_EarlyRecAsymp'

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
                    subplots = c(1:3), print = TRUE, plotdir = here(sens_dir, new_name))

######-
## More Rec Blocks with Early Period Asymptotic --------------------------------------------------------
# Leave CCFRP and ROV asymptotic

new_name <- 'sel_EarlyRecAsympDomeSurveys'

mod <- SS_read(here('models', '_sensitivities', 'sel_EarlyRecAsymp'))

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
                                                   file.path('_sensitivities', 'sel_EarlyRecAsymp'),
                                                   file.path('_sensitivities', new_name)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Late Rec Blocks Domed',
                                     'Late Rec Blocks & Surveys Domed'),
                    subplots = c(1:3), print = TRUE, plotdir = here(sens_dir, new_name))

######-
## Commercial Asymptotic --------------------------------------------------------
#Asymptotic for final block (keeping earliest block as domed)
#This applies only for sens from model 421 as it was implemented for model 513
 
# new_name <- 'sel_ComAsymp'
# 
# mod <- base_mod
# 
# mod[["ctl"]][["size_selex_parms"]][["PHASE"]][4] <- -5
# mod[["ctl"]][["size_selex_parms"]][["INIT"]][4] <- 15
# 
# # Write model and run
# SS_write(mod, here(sens_dir, new_name),
#          overwrite = TRUE)
# 
# r4ss::run(dir = here(sens_dir, new_name), 
#           exe = here('models/ss3_win.exe'), 
#           extras = '-nohess', 
#           show_in_console = TRUE, 
#           skipfinished = FALSE)
# 
# pp <- SS_output(here(sens_dir, new_name))
# SS_plots(pp, plot = c(1:26))
# plot_sel_all(pp)
# 
# xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
#                                         subdir = c(base_mod_name,
#                                                    file.path('_sensitivities', new_name)))))
# SSsummarize(xx) |>
#   SSplotComparisons(legendlabels = c('Base model',
#                                      'Commercial Asymptotic'),
#                     subplots = c(1:3), print = TRUE, plotdir = here(sens_dir, new_name))

######-
## Commercial All Asymptotic --------------------------------------------------------
#Asymptotic for all blocks

new_name <- 'sel_ComAsymp_allBlocks'

mod <- base_mod

#mod$ctl$size_selex_parms["SizeSel_P_4_CA_Commercial(1)", "PHASE"] <- -5
#mod$ctl$size_selex_parms["SizeSel_P_4_CA_Commercial(1)", "INIT"] <- 15

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

##Compare likelihoods of selectivity sensitivities - for testing purposes
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c("5_1_3_preStarBase",
                                                 file.path("_sensitivities", "sel_AltComBlocks"),
                                                 file.path("_sensitivities", "sel_NoBlocks"),
                                                 file.path("_sensitivities", "sel_ROVandCCFRPDomed"),
                                                 file.path("_sensitivities", "sel_RecDomed"),
                                                 file.path("_sensitivities", "sel_ROVandCCFRPandRecDomed"),
                                                 file.path("_sensitivities", "sel_ComDomed"),
                                                 file.path("_sensitivities", "sel_AllDomed"),
                                                 file.path("_sensitivities", "sel_EarlyRecAsymp"),
                                                 file.path("_sensitivities", "sel_EarlyRecAsympDomeSurveys"),
                                                 #file.path("_sensitivities", "sel_ComAsymp"),
                                                 file.path("_sensitivities", "sel_ComAsymp_allBlocks"))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c("Base",
                                     "Alt com blocks",
                                     "No blocks",
                                     "ROV and CCFRP Domed",
                                     "Rec Domed",
                                     "ROV and CCFRP and Rec Domed",
                                     "Com Domed",
                                     "All Domed",
                                     "Alt rec blocks domed",
                                     "Alt rec blocks, ROV and CCFRP domed",
                                     #"Last com block asymptotic",
                                     "All com block asymptotic"),
                    subplots = c(1:3), print = TRUE, plotdir = here(sens_dir, new_name))

#Compare likelihoods
xx.sum <- SSsummarize(xx)
xx.tab <- SStableComparisons(xx.sum, 
                             modelnames = c("Base",
                                            "Alt com blocks",
                                            "No blocks",
                                            "ROV and CCFRP Domed",
                                            "Rec Domed",
                                            "ROV and CCFRP and Rec Domed",
                                            "Com Domed",
                                            "All Domed",
                                            "Alt rec blocks domed",
                                            "Alt rec blocks, ROV and CCFRP domed",
                                            #"Last com block asymptotic",
                                            "All com block asymptotic")) |>
  dplyr::mutate(across(-Label, ~ round(., 2))) |>
  dplyr::rename(' ' = Label)
xx.val <- rbind(xx.sum$npars, xx.tab[,-1]) #add number of parameters
rownames(xx.val) <- c("Npars", xx.tab[,1]) #add rownames so dataframe stays numerical
write.csv(t(xx.val[1:5,]), here(sens_dir, new_name, 'like_comp.csv'), row.names = TRUE)


######-
## Selex using 24 but more flexible -----
new_name <- 'sel_SpecialROV'

mod <- base_mod

mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
                                   grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, 4)

mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
                                   grep("P_6", rownames(mod$ctl$size_selex_parms))), 
                         c("LO", "HI", "INIT", "PHASE")] <- c(-12, 20, 15, 4)

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
#paramter not well estimted - lo



######-
## Use full block structure for catch fleets and allow domed -----

new_name <- 'sel_fullBlocks_DomedRecCom'

mod <- base_mod

mod$ctl$blocks_per_pattern <- c(3, 3)
mod$ctl$Block_Design <- list(c(2003, 2013, 2014, 2021, 2022, 2024), #commercial fleet
                             c(2001, 2016, 2017, 2022, 2023, 2024)) #recreational fleet

mod$ctl$size_selex_parms["SizeSel_P_4_CA_Commercial(1)", 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.48064, 5)
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Recreational(2)", 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.54518, 5)

### Time varying selectivity table
selex_new <- mod$ctl$size_selex_parms

selex_tv_pars <- dplyr::filter(selex_new, Block > 0) |>
  dplyr::select(LO, HI, INIT, PRIOR, PR_SD, PR_type, PHASE, Block) |>
  tidyr::uncount(mod$ctl$blocks_per_pattern[Block], .id = 'id', .remove = FALSE)

rownames(selex_tv_pars) <- rownames(selex_tv_pars) |>
  stringr::str_remove('\\.\\.\\.[:digit:]+') |>
  stringr::str_c('_BLK', selex_tv_pars$Block, 'repl_', mapply("[",mod$ctl$Block_Design[selex_tv_pars$Block], selex_tv_pars$id * 2 - 1))

mod$ctl$size_selex_parms_tv <- selex_tv_pars |>
  dplyr::select(-Block, -id)

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


######-
## Use full block structure for catch fleets and allow domed but comm block 2018 -----

#This is based on looking back at discussions with thompson and budrick suggesting
#2017/2018 is a better block than 2014. Likelihood is not as good however. 

new_name <- 'sel_fullBlocks_DomedRecCom_2018com'

mod <- base_mod

mod$ctl$blocks_per_pattern <- c(3, 3)
mod$ctl$Block_Design <- list(c(2003, 2017, 2018, 2021, 2022, 2024), #commercial fleet
                             c(2001, 2016, 2017, 2022, 2023, 2024)) #recreational fleet

mod$ctl$size_selex_parms["SizeSel_P_4_CA_Commercial(1)", 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.48064, 5)
mod$ctl$size_selex_parms["SizeSel_P_4_CA_Recreational(2)", 
                         c("LO", "HI", "INIT", "PHASE")] <- c(0, 9, 5.54518, 5)

### Time varying selectivity table
selex_new <- mod$ctl$size_selex_parms

selex_tv_pars <- dplyr::filter(selex_new, Block > 0) |>
  dplyr::select(LO, HI, INIT, PRIOR, PR_SD, PR_type, PHASE, Block) |>
  tidyr::uncount(mod$ctl$blocks_per_pattern[Block], .id = 'id', .remove = FALSE)

rownames(selex_tv_pars) <- rownames(selex_tv_pars) |>
  stringr::str_remove('\\.\\.\\.[:digit:]+') |>
  stringr::str_c('_BLK', selex_tv_pars$Block, 'repl_', mapply("[",mod$ctl$Block_Design[selex_tv_pars$Block], selex_tv_pars$id * 2 - 1))

mod$ctl$size_selex_parms_tv <- selex_tv_pars |>
  dplyr::select(-Block, -id)

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



#========================================================================================#
# ROV Absolute Abundance -----
#========================================================================================#

## Fix M, h, and growth at base values.  Apply a multiplier to catch. --------------------------------------

new_name <- 'ROV_Abs_1'

mod <- base_mod

mod[["ctl"]][["MG_parms"]][["PHASE"]][2:6] <- -3

mod[["ctl"]][["MG_parms"]][["INIT"]][2:6] <- c(9.8983100, 42.7777000, 0.1256130, 0.1823610, 0.0862424)

# The estimated proportion of quillback habitat in MPAs is 0.20.
# Tanya estimated 155225 fish in MPAs (actually 151,934 in final report) in 2015.
# Expanding that to all habitat results in 759670 fish.

pp <- SS_output(here('models', base_mod_name))
pp <- SS_output(here('models', '5_1_3_preStarBase'))

numbers_at_age <- pp$natage
pp$natageOnePlus_numbers <- numbers_at_age %>%
  filter(`Beg/Mid` == "M") %>% #taking that mid year since that represents the survey
  mutate(numberOfFish = rowSums(across(c("3":"80")))) %>%  #could also look at ages 2+
  dplyr::select(c("Time", "numberOfFish"))

# Using the code above from Melissa to look at numbers of fish in the middle of the year,
# we get 348697 fish in 2015.  

# The ROV estimate is 2.18 times the model estimate.  

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

numbers_at_age <- pp$natage
pp$natageOnePlus_numbers <- numbers_at_age %>%
  filter(`Beg/Mid` == "M") %>% #taking that mid year since that represents the survey
  mutate(numberOfFish = rowSums(across(c("3":"80")))) %>%  #could also look at ages 2+
  dplyr::select(c("Time", "numberOfFish"))

# This results in 957451 fish in 2015.  That's too many.  Let's try multiplying catch by 2.2.  
mod <- base_mod
mod[["ctl"]][["MG_parms"]][["PHASE"]][2:6] <- -3
mod[["ctl"]][["MG_parms"]][["INIT"]][2:6] <- c(9.8983100, 42.7777000, 0.1256130, 0.1823610, 0.0862424)
mod[["dat"]][["catch"]][["catch"]] <- mod[["dat"]][["catch"]][["catch"]] * 2.2

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

# This gives 766260 fish in 2015 - pretty close to the ROV estimate.  


######-
## Include as an absolute abundance index ----

# Set q to 0.2 to approximate the amount of area set aside as MPA
# Doesn't matter if we multiple est by 5 or q by 0.2

new_name <- 'ROV_Abs_2_q0.2'

mod <- base_mod

#Need to enter as thousands of fish
mod$dat$CPUE[which(mod$dat$CPUE$index == 5), "obs"] <- c(151934, 317274)/1000
mod$dat$CPUE[which(mod$dat$CPUE$index == 5), "se_log"] <- c(0.118, 0.0666)

#Q params are ln(q) so fix to 0 in log space
mod$ctl$Q_parms["LnQ_base_CA_ROV(5)", c("INIT", "PHASE")] <- c(log(0.2), -2)

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
plot_sel_all(pp)


######-
## Include as an absolute abundance index with extra SE----

# Set q to 0.2 to approximate the amount of area set aside as MPA
# Estimate extra SD for q

new_name <- 'ROV_Abs_3_extraSE'

mod <- base_mod

#Need to enter as thousands of fish
mod$dat$CPUE[which(mod$dat$CPUE$index == 5), "obs"] <- c(151934, 317274)/1000
mod$dat$CPUE[which(mod$dat$CPUE$index == 5), "se_log"] <- c(0.118, 0.0666)

#Q params are ln(q) so fix in log space
mod$ctl$Q_parms["LnQ_base_CA_ROV(5)", c("INIT", "PHASE")] <- c(log(0.2), -2)

mod$ctl$Q_options["CA_ROV", "extra_se"] <- 1
mod$ctl$Q_parms <- 
  dplyr::add_row(mod$ctl$Q_parms,
                 LO=0,
                 HI = 5,
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
                 Block_Fxn = 0)
rownames(mod$ctl$Q_parms)[4] <- "ExtraSD_CA_ROV(5)"

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


######-
## Include as an absolute abundance index with estiamted q----

# Set q to 0.2 to approximate the amount of area set aside as MPA

new_name <- 'ROV_Abs_4_estq'

mod <- base_mod

#Need to enter as thousands of fish
mod$dat$CPUE[which(mod$dat$CPUE$index == 5), "obs"] <- c(151934, 317274)/1000
mod$dat$CPUE[which(mod$dat$CPUE$index == 5), "se_log"] <- c(0.118, 0.0666)

#Q params are ln(q) so fix in log space
mod$ctl$Q_parms["LnQ_base_CA_ROV(5)", c("INIT", "PHASE")] <- c(log(0.2), 2)

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
                                                   file.path('_sensitivities', 'ROV_Abs_1'),
                                                   file.path('_sensitivities', 'ROV_Abs_2_q0.2'),
                                                   file.path('_sensitivities', 'ROV_Abs_3_extraSE'),
                                                   file.path('_sensitivities', 'ROV_Abs_4_estq')))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Increase catch to approximate scale',
                                     'Include as absolute abundance index with fixed Q',
                                     'Include as absolute abundance index with extraSE',
                                     'Include as absolute abundance index with est Q'),
                    subplots = c(1,3,18), print = TRUE, plotdir = here(sens_dir, new_name))



#========================================================================================#
# RecDev sensitivities -----
#========================================================================================#

######-
## Turn off recdevs --------------------------------------------------------

new_name <- 'recdev_Off'

mod <- base_mod

mod$ctl$do_recdev <- 0

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


######-
## Start recdevs at 1990 --------------------------------------------------------

new_name <- 'recdev_1990'

mod <- base_mod

mod$ctl$recdev_early_phase <- -5 #turn off early recdevs
mod$ctl$MainRdevYrFirst <- 1990

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


######-
## Start recdevs at 2000 --------------------------------------------------------

new_name <- 'recdev_2000'

mod <- base_mod

mod$ctl$recdev_early_phase <- -5 #turn off early recdevs
mod$ctl$MainRdevYrFirst <- 2000

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
                                                   file.path('_sensitivities', 'recdev_1990'),
                                                   file.path('_sensitivities', 'recdev_2000')))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     "Recdevs start at 1990",
                                     "Recdevs start at 2000"),
                    subplots = c(1,3,9,18), print = TRUE, plotdir = here(sens_dir, new_name))


######-
## Use recdev option 2 --------------------------------------------------------

new_name <- 'recdev_opt2'

mod <- base_mod

mod$ctl$do_recdev <- 2

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


######-
## Use recdev option 2 with main devs set at 1990 --------------------------------------------------------

new_name <- 'recdev_1990opt2'

mod <- base_mod

mod$ctl$recdev_early_phase <- -5 #turn off early recdevs
mod$ctl$MainRdevYrFirst <- 1990
mod$ctl$do_recdev <- 2

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
                                                   file.path('_sensitivities', 'recdev_off'),
                                                   file.path('_sensitivities', 'recdev_opt2'),
                                                   file.path('_sensitivities', 'recdev_1990'),
                                                   file.path('_sensitivities', 'recdev_1990opt2')))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'No recdevs',
                                     "Recdev option 2",
                                     "Recdevs start at 1990",
                                     "Recdevs options 2 start at 1990"),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name))


##########################################################################################-
#
# Summary Tables and Figures ----
#
##########################################################################################-

# The following outputs an overall sensitivity figure and table as well as 
# individual sensitivity figures and tables for each specified grouping 

make_detailed_sensitivites <- function(biglist, 
                                       mods_to_include, 
                                       pretty_names = mods_to_include, 
                                       outdir, 
                                       grp_name) {
  
  shortlist <-   biglist[c('base', mods_to_include)] |>
    r4ss::SSsummarize() 
  
  # r4ss::SSplotComparisons(shortlist,
  #                         subplots = c(2,4), 
  #                         print = FALSE,  
  #                         plot = FALSE,
  #                         plotdir = file.path(outdir, 'figures'), 
  #                         filenameprefix = paste0('sens_', grp_name, "_"),
  #                         legendlabels = c('Base', pretty_names))
  
  r4ss::plot_twopanel_comparison(biglist[c('base', mods_to_include)],
                                 dir = file.path(outdir, 'figures'), 
                                 filename = paste0("sens_", grp_name, '_comparison.png'),
                                 legendlabels = c('Base', pretty_names), 
                                 legendloc = 'bottomleft',
                                 endyrvec = 2025)
  
  SStableComparisons(shortlist, 
                     modelnames = c('Base', pretty_names),
                     likenames = c("TOTAL", "Survey", "Length_comp", "Age_comp", "Recruitment", "priors"),
                     names =c("Recr_Virgin", "R0", "steep", "NatM", "L_at_Amax", "VonBert_K", "SSB_Virg",
                              "SSB_2025", "Bratio_2025", "SPRratio_2024")) |>
    dplyr::mutate(dplyr::across(-Label, ~ sapply(., format, digits = 3, nsmall = 3, scientific = FALSE) |>
                                  stringr::str_replace('NA', ''))) |>
    `names<-`(c(' ', 'Base', pretty_names)) %>%
    rbind(c("Npar", shortlist$npars), .) |>
    write.csv(file.path(outdir, 'tables', paste0('sens_', grp_name, '_table.csv')), 
              row.names = FALSE)
}

# Selectivity models

selec_models <- c('sel_AllDomed', 
                  'sel_ComDomed',
                  'sel_RecDomed',
                  'sel_ROVandCCFRPDomed',
                  'sel_NoBlocks',
                  'sel_ComAsymp_allBlocks',
                  'sel_EarlyRecAsympDomeSurveys',
                  'sel_fullBlocks_DomedRecCom')

selec_pretty <- c('All fleets domed',
                  'Com domed',
                  'Rec domed',
                  'Surveys domed',
                  'No blocks',
                  'All asymptotic',
                  'Full rec block with domed',
                  'Full rec and com block with domed')

# # For the report scenarios in these two categories were combined as 'data_contribution_models'
# #Weighting models
# 
# weighting_models <- c('dw_dirichlet',
#                       'dw_MI',
#                       'IndexExtraSE')
# 
# weighting_pretty <- c('Dirichlet',
#                       'McAllister-Ianelli',
#                       'Index Extra SD')
# 
# #Leave out explorations
# 
# leaveOut_models <- c('leaveOut_com_ages',
#                      'leaveOut_com_lengths',
#                      'leaveOut_rec_lengths',
#                      'leaveOut_growth_ages', 
#                      'leaveOut_prIndex',
#                      'leaveOut_rov',
#                      'leaveOut_ccfrp')
# 
# leaveOut_pretty <- c('Remove com ages',
#                      'Remove com lengths',
#                      'Remove rec lengths',
#                      'Remove growth ages',
#                      'Remove PR index',
#                      'Remove ROV index',
#                      'Remove CCFRP index')


#Combined weighting and data contributions

data_contribution_models <- c('dw_dirichlet',
                              'dw_MI',
                              'IndexExtraSE',
                              'leaveOut_fleet_lengths',
                              'leaveOut_all_ages',
                              'leaveOut_all_indices')

data_contribution_pretty <- c('Dirichlet',
                              'McAllister-Ianelli',
                              'Index Extra SD',
                              'Remove fleet lengths',
                              'Remove ages',
                              'Remove indices')


#Data related models

data_models <- c(#'catchIncreaseSE',
                 'catchOutliers',
                 'catchSmooth',
                 #'comLenSampleSize', #minor
                 'FAA_resetCom_reblock_reweight', 
                 #'marginalComAge', #minor
                 'noAgeErr')#,
                 #'noNegYear', #minor
                 #'NoRecLen2024') #minor

data_pretty <- c(#'Increase catch se',
                 'Reduce large catches',
                 'Catch as five year moving average',
                 #'Remove com length comps with N < 10',
                 'Fleets as areas',
                 #'Use marginal com age comps',
                 'Remove ageing error')#,
                 #'Add all low sample size comps',
                 #'Remove rec length comp in 2024')



#Productivity models

productivity_models <- c('estimate_h',
                         'estimate_M',
                         'estimate_M_and_h',
                         'recdev_1990',
                         'recdev_Off',
                         'Growth_2021est',
                         'Growth_2021est_noAges')

productivity_pretty <- c('Estimate h',
                         'Estimate M',
                         'Estimate M and h',
                         'Start recdevs in 1990',
                         'Turn off recdevs',
                         'Fix growth at 2021 values',
                         'Fix growth at 2021 values without ages')


# #Biology models - not ultimately used in the report
# 
# biology_models <- c('Maturity_2021est',
#                     'fecundity_EJest',
#                     'maxAge84',
#                     'maxAge70')
# 
# biology_pretty <- c('Oregon maturity',
#                     'Dick 2017 fecundity',
#                     'Max age 84',
#                     'Max age 70')


# List all groups of models together

models_all <- c(selec_models,
                data_models,
                #weighting_models,
                #leaveOut_models,
                data_contribution_models,
                productivity_models)#, 
                #biology_models)

pretty_all <- c(selec_pretty,
                data_pretty,
                #weighting_pretty,
                #leaveOut_pretty,
                data_contribution_pretty,
                productivity_pretty)#,
                #biology_pretty)

big_sensitivity_output <- SSgetoutput(dirvec = c(
  here('models', base_mod_name),
  glue::glue("{models}/{subdir}", 
             models = here('models', '_sensitivities'),
             subdir = models_all))) |>
  `names<-`(c('base', models_all))


# test to make sure reading
which(sapply(big_sensitivity_output, length) < 180) # if integer(0) then good

# Directory that contains the figures and tables folders

outdir <- here('report')

make_detailed_sensitivites(big_sensitivity_output,
                           mods_to_include = selec_models,
                           outdir = outdir,
                           grp_name = 'selectivity',
                           pretty_names = selec_pretty)

# For the report scenarios in these categories were combined, see 'data_contribution_models'
# make_detailed_sensitivites(big_sensitivity_output, 
#                            mods_to_include = weighting_models,
#                            outdir = outdir,
#                            grp_name = 'weighting',
#                            pretty_names = weighting_pretty)

# make_detailed_sensitivites(big_sensitivity_output, 
#                            mods_to_include = leaveOut_models,
#                            outdir = outdir,
#                            grp_name = 'leaveOut',
#                            pretty_names = leaveOut_pretty)

make_detailed_sensitivites(big_sensitivity_output, 
                           mods_to_include = data_contribution_models,
                           outdir = outdir,
                           grp_name = 'data_contribution',
                           pretty_names = data_contribution_pretty)

make_detailed_sensitivites(big_sensitivity_output, 
                           mods_to_include = data_models,
                           outdir = outdir,
                           grp_name = 'data',
                           pretty_names = data_pretty)

make_detailed_sensitivites(big_sensitivity_output, 
                           mods_to_include = productivity_models,
                           outdir = outdir,
                           grp_name = 'productivity',
                           pretty_names = productivity_pretty)

#Not ultimately used in the report. 
# make_detailed_sensitivites(big_sensitivity_output, 
#                            mods_to_include = biology_models,
#                            outdir = outdir,
#                            grp_name = 'biology',
#                            pretty_names = biology_pretty)


## Big plot
current.year <- 2025
CI <- 0.95

sensitivity_output <- SSsummarize(big_sensitivity_output) 

lapply(big_sensitivity_output, function(.)
  .$warnings[grep('gradient', .$warnings)]) # check gradients

dev.quants.SD <- c(
  sensitivity_output$quantsSD[sensitivity_output$quantsSD$Label == "SSB_Initial", 1],
  sensitivity_output$quantsSD[sensitivity_output$quantsSD$Label == paste0("SSB_", current.year), 1],
  sensitivity_output$quantsSD[sensitivity_output$quantsSD$Label == paste0("Bratio_", current.year), 1],
  sensitivity_output$quantsSD[sensitivity_output$quantsSD$Label == "Dead_Catch_SPR", 1],
  sensitivity_output$quantsSD[sensitivity_output$quantsSD$Label == "annF_SPR", 1]
)

dev.quants <- rbind(
  sensitivity_output$quants[sensitivity_output$quants$Label == "SSB_Initial", 
                            1:(dim(sensitivity_output$quants)[2] - 2)],
  sensitivity_output$quants[sensitivity_output$quants$Label == paste0("SSB_", current.year), 
                            1:(dim(sensitivity_output$quants)[2] - 2)],
  sensitivity_output$quants[sensitivity_output$quants$Label == paste0("Bratio_", current.year), 
                            1:(dim(sensitivity_output$quants)[2] - 2)],
  sensitivity_output$quants[sensitivity_output$quants$Label == "Dead_Catch_SPR", 
                            1:(dim(sensitivity_output$quants)[2] - 2)],
  sensitivity_output$quants[sensitivity_output$quants$Label == "annF_SPR", 
                            1:(dim(sensitivity_output$quants)[2] - 2)]
) |>
  cbind(baseSD = dev.quants.SD) |>
  dplyr::mutate(Metric = c("SB0", paste0("SSB_", current.year), paste0("Bratio_", current.year), "MSY_SPR", "F_SPR")) |>
  tidyr::pivot_longer(-c(base, Metric, baseSD), names_to = 'Model', values_to = 'Est') |>
  dplyr::mutate(relErr = (Est - base)/base,
                logRelErr = log(Est/base),
                mod_num = rep(1:length(models_all), 5))

metric.labs <- c(
  SB0 = expression(SB[0]),
  SSB_2023 = as.expression(bquote("SB"[.(current.year)])),
  Bratio_2023 = bquote(frac(SB[.(current.year)], SB[0])),
  MSY_SPR = expression(Yield['SPR=0.50']),
  F_SPR = expression(F['SPR=0.50'])
)

CI.quants <- dev.quants |>
  dplyr::filter(Model == unique(dev.quants$Model)[1]) |>
  dplyr::select(base, baseSD, Metric) |>
  dplyr::mutate(CI = qnorm((1-CI)/2, 0, baseSD)/base)

ggplot(dev.quants, aes(x = relErr, y = mod_num, col = Metric, pch = Metric)) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  geom_point() +
  geom_segment(aes(x = CI, xend = abs(CI), col = Metric,
                   y = length(models_all) + 1.5 + seq(-0.5, 0.5, length.out = length(metric.labs)),
                   yend = length(models_all) + 1.5 + seq(-0.5, 0.5, length.out = length(metric.labs))), 
               data = CI.quants, linewidth = 2, show.legend = FALSE, lineend = 'round') +
  theme_bw() +
  scale_shape_manual(
    values = c(15:18, 12),
    # name = "",
    labels = metric.labs
  ) +
  # scale_color_discrete(labels = metric.labs) +
  scale_y_continuous(breaks = 1:length(models_all), name = '', labels = pretty_all, 
                     limits = c(1, length(models_all) + 2), minor_breaks = NULL) +
  xlab("Relative change") +
  viridis::scale_color_viridis(discrete = TRUE, labels = metric.labs)
ggsave(file.path(outdir, 'figures', 'sens_summary.png'),  dpi = 300,  
       width = 6, height = 7, units = "in")

######-
## Remove ages and fix growth at internal est.
###fix growth to internal and leave out ages
new_name <- 'fix_growth_no_ages'
old_name <- base_mod_name

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))

#

mod$ctl$MG_parms$INIT[2] <-  9.8022100
mod$ctl$MG_parms$INIT[3] <- 42.7486000	
mod$ctl$MG_parms$INIT[4] <-  0.1261450	
mod$ctl$MG_parms$INIT[5] <-  0.1836600
mod$ctl$MG_parms$INIT[6] <-  0.0854974
mod$ctl$MG_parms$PRIOR[2:6] <- mod$ctl$MG_parms$INIT[2:6]
#negative phase 
mod$ctl$MG_parms$PHASE[2:6] <- -9


# Create a lambda section 
lambdas <- data.frame("like_comp" = c(5, 5), #age comps
                      "fleet" = c(1, 3),
                      "phase" = c(1, 1),
                      "value" = c(0, 0),
                      "sizefreq_method" = c(1, 1))
rownames(lambdas) <- c("CAAL_CA_Commercial", "CAAL_CA_Growth")

mod$ctl$N_lambdas <- nrow(lambdas)
mod$ctl$lambdas <- lambdas

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



###fix growth to internal and leave out ages
new_name <- 'fix_growth_external_no_ages'
old_name <- base_mod_name

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here(sens_dir, new_name),
               overwrite = TRUE)

mod <- SS_read(here(sens_dir,new_name))

 #         K        Linf          L0         CV0         CV1
 #0.17840051 41.17135518  3.99192977  0.20157461  0.06413399
#This is L0 so change that in the model
mod$ctl$Growth_Age_for_L1 <- 0
mod$ctl$MG_parms$INIT[2] <-  3.99192977
mod$ctl$MG_parms$INIT[3] <- 41.17135518	
mod$ctl$MG_parms$INIT[4] <-  0.17840051	
mod$ctl$MG_parms$INIT[5] <-  0.20157461
mod$ctl$MG_parms$INIT[6] <-  0.06413399
mod$ctl$MG_parms$PRIOR[2:6] <- mod$ctl$MG_parms$INIT[2:6]
#negative phase 
mod$ctl$MG_parms$PHASE[2:6] <- -9


# Create a lambda section 
lambdas <- data.frame("like_comp" = c(5), #age comps
                      "fleet" = c(3),
                      "phase" = c(1),
                      "value" = c(0),
                      "sizefreq_method" = c(1))
rownames(lambdas) <- c("CAAL_CA_Growth")

mod$ctl$N_lambdas <- nrow(lambdas)
mod$ctl$lambdas <- lambdas

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





##########################################################################################-
#
# STAR Panel Requests ----
#
##########################################################################################-

######-
## Request 1 - plotting recruitment from existing sensitivities --------------------------------------------------------

new_name <- 'STAR Req1'
dir.create(here(sens_dir, new_name))

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', 'leaveOut_all_ages'),
                                                   file.path('_sensitivities', 'leaveOut_growth_ages'),
                                                   file.path('_sensitivities', 'Growth_2021est'),
                                                   file.path('_sensitivities', 'Growth_2021est_noAges'),
                                                   file.path('_sensitivities', 'Estimate_M'),
                                                   file.path('_sensitivities', 'sel_NoBlocks')))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Remove All Ages',
                                     "Remove Growth Fleet",
                                     "Fix Growth at 2021 w/ Ages",
                                     "Fix Growth at 2021 w/out Ages",
                                     "Estimate M",
                                     "No Blocks on Selectivity"),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name), legendloc = 'topleft')


#####-
## Request 3 - leaveOut_all_ages_NoBlocks ---------------------------------

# Remove commercial and CAAL ages using lambdas

new_name <- 'STAR_request_3'

mod <- base_mod

# Create a lambda section 
lambdas <- data.frame("like_comp" = c(5, 5), #age comps
                      "fleet" = c(1, 3),
                      "phase" = c(1, 1),
                      "value" = c(0, 0),
                      "sizefreq_method" = c(1, 1))
rownames(lambdas) <- c("CAAL_CA_Commercial", "CAAL_CA_Growth")

mod$ctl$N_lambdas <- nrow(lambdas)
mod$ctl$lambdas <- lambdas

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
          show_in_console = TRUE,           skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$likelihoods_used

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name),
                                                   file.path('_sensitivities', 'sel_NoBlocks'),
                                                   file.path('_sensitivities', 'leaveOut_all_ages')))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'No Blocks without Ages',
                                     'No Blocks with Ages',
                                     'Blocks without Ages'),
                    subplots = c(1:14), print = TRUE, plotdir = here(sens_dir, new_name))


#####-
## Request 4 - commercial N_trips ---------------------------------

#Use number of trips for commercial input N lengths instead of the formula

new_name <- "STAR_request4_comNtrips"
old_name <- base_mod_name

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', '_sensitivities', new_name),
               overwrite = TRUE)
mod <- SS_read(here('models', '_sensitivities', new_name))

##
#Make Changes and run models
##

#Get number of trips and use and sample size for commercial lengths
commlengths <- read.csv(here::here('data', "SampleSize_length.csv"), check.names = FALSE) |>
  dplyr::select(c(Year, pacfin_Nfish, pacfin_Ntrip)) |>
  dplyr::mutate(pacfin_Ninput = 
                  ifelse(pacfin_Nfish/pacfin_Ntrip < 44, 
                         pacfin_Ntrip + 0.138 * pacfin_Nfish,
                         7.06 * pacfin_Ntrip)) |>
  dplyr::filter(!is.na(pacfin_Ninput))

mod$dat$lencomp[mod$dat$lencomp$fleet == 1, "Nsamp"] <- commlengths$pacfin_Ntrip


#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

##
#Compare effective sample size for commercial lengths
##

pp_base <- SS_output(here('models', base_mod_name))
effN <- data.frame("Year" = pp_base$len_comp_fit_table[pp_base$len_comp_fit_table$Fleet == 1, "Yr"],
                   "effN_base" = pp_base$len_comp_fit_table[pp_base$len_comp_fit_table$Fleet == 1, "Nsamp_adj"],
                   "effN_Ntrip" = pp$len_comp_fit_table[pp$len_comp_fit_table$Fleet == 1, "Nsamp_adj"])
inputN <- data.frame("Year" = pp_base$len_comp_fit_table[pp_base$len_comp_fit_table$Fleet == 1, "Yr"],
                     "effN_base" = pp_base$len_comp_fit_table[pp_base$len_comp_fit_table$Fleet == 1, "Nsamp_in"],
                     "effN_Ntrip" = pp$len_comp_fit_table[pp$len_comp_fit_table$Fleet == 1, "Nsamp_in"])

png(here('models', '_sensitivities', new_name, paste0("_Sample size compare",".png")), width = 6, height = 4, units = "in", res = 300)
  plot(effN$Year, effN$effN_Ntrip, pch = 19, ylim = c(0,50),
       col = "green", xlab = "Year", ylab = "Sample size")
  abline(h = mean(effN$effN_Ntrip), lty = 2, col = "green")
  points(effN$Year, effN$effN_base, pch = 19)
  abline(h = mean(effN$effN_base))
  legend("topright", c("Base","Ntrip"), pch = c(19,19), col = c("black","green"))
dev.off()


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_request4_comNtrips"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'Reweight based on comm length trips'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "topright",
                    plotdir = here('models', '_sensitivities', new_name))
dev.off()


######-
## Request 6 - sigmaR tuning --------------------------------------------------------

#Explore sigmaR adjustments 
#First step

new_name <- "STAR_request6_sigmaR1"

#Calculate new sigmaR adjustment
pp <- SS_output(here('models',base_mod_name), covar = TRUE)
alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main", "alternative_sigma_R"]

#Update in new sensitivity
mod <- base_mod

mod$ctl$SR_parms["SR_sigmaR", "INIT"] <- as.numeric(alt_sigmaR)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


#Second step

new_name <- "STAR_request6_sigmaR2"

#Calculate new sigmaR adjustment from first step
alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main", "alternative_sigma_R"]

#Update in new sensitivity
mod <- base_mod

mod$ctl$SR_parms["SR_sigmaR", "INIT"] <- as.numeric(alt_sigmaR)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', 'STAR_request6_sigmaR1'),
                                                   file.path('_sensitivities', 'STAR_request6_sigmaR2')))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Run 1: 0.85',
                                     'Run 2: 1.12'),
                    subplots = c(1, 3, 9, 11), print = TRUE, plotdir = here(sens_dir, new_name))

r4ss::plot_twopanel_comparison(xx,
                               dir = here('models', '_sensitivities', 'STAR_request6_sigmaR2'),
                               filename = "_sigmaR_comparison.png",
                               legendlabels = c('Base model', 'Run 1: 0.85', 'Run 2: 1.12'),
                               legendloc = 'bottomleft',
                               hessian = FALSE,
                               subplot1 = 1,
                               subplot2 = 3,
                               endyrvec = 2025)

#What is the sigmaR for the run with recdevs starting in 1990
pp <- SS_output(here('models', '_sensitivities', 'recdev_1990_hessian'), covar = TRUE)
alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main", "alternative_sigma_R"]
#Still wants to increase

#What is the sigmaR for the run with recdevs starting in 1990 with recdev option2
pp <- SS_output(here('models', '_sensitivities', 'recdev_1990opt2_hessian'), covar = TRUE)
alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main", "alternative_sigma_R"]
#Still wants to increase


######-
## Request 7 - assign growth fleet ages elsewhere --------------------------------------------------------

#Remove the growth fleet CAAL and assign CCFRP ages to the survey fleet

new_name <- "STAR_request7_CCFRPages"

mod <- base_mod

#Remove growth fleet CAAL data and add in CCFRP only CAAL and assign to that fleet
mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])

ccfrp.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_ccfrp_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 4) %>%
  as.data.frame()
names(ccfrp.CAAL) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, ccfrp.CAAL)

#Set francis weight for growth fleet ages to be the same as for ccfrp
mod$ctl$Variance_adjustment_list[6,2] <- 4


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


######-
## Request 7 reweight - assign growth fleet ages elsewhere and reweight --------------------------------------------------------

#Copy request 7 and now reweight

new_name <- "STAR_request7_CCFRPages_reweight"

mod <- base_mod

#Remove growth fleet CAAL data and add in CCFRP only CAAL and assign to that fleet
mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])

ccfrp.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_ccfrp_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 4) %>%
  as.data.frame()
names(ccfrp.CAAL) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, ccfrp.CAAL)

#Set francis weight for growth fleet ages to be the same as for ccfrp
mod$ctl$Variance_adjustment_list[6,2] <- 4

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       #extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$sigma_R_info #0.8


##Comparison plots
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_request7_CCFRPages"),
                                                 file.path('_sensitivities', "STAR_request7_CCFRPages_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'Add CCFRP growth fleet ages into CCFRP fleet',
                                     'Same as above but reweight ccfrp ages'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "topright",
                    plotdir = here('models', '_sensitivities', "STAR_request7_CCFRPages_reweight"))
dev.off()
plot_compare_growth(models = xx,
                    new_name = "STAR_request7_CCFRPages_reweight",
                    legend_names = c('Base',
                                     'Add CCFRP growth fleet ages into CCFRP fleet',
                                     'Same as above but reweight ccfrp ages'))

#Non reweighted and reweighted runs are similar. Can present only reweighted


######-
## Request 7b - assign growth fleet and other ages elsewhere --------------------------------------------------------

#Remove the growth fleet CAAL and assign CCFRP ages and other ages to their fleets

new_name <- "STAR_request7b_CCFRP_rec_ages"

mod <- base_mod

#Remove growth fleet CAAL data 
mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])

#Add in other Rec related CAAL and assign to that fleet
otherRec.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_possibleRec_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 2) %>%
  as.data.frame()
names(otherRec.CAAL) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, otherRec.CAAL)

#Add in CCFRP only CAAL and assign to that fleet
ccfrp.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_ccfrp_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 4) %>%
  as.data.frame()
names(ccfrp.CAAL) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, ccfrp.CAAL)


#Set francis weight for growth fleet ages to be the same for ccfrp and rec
mod$ctl$Variance_adjustment_list[6,2] <- 2
mod$ctl$Variance_adjustment_list[7,] <- mod$ctl$Variance_adjustment_list[6,]
mod$ctl$Variance_adjustment_list[7,2] <- 4
rownames(mod$ctl$Variance_adjustment_list)[7] <- "Variance_adjustment_list7"


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


######-
## Request 7b reweight - assign growth fleet and other ages elsewhere and reweight --------------------------------------------------------

#Copy model from request 7b and now reweight

new_name <- "STAR_request7b_CCFRP_rec_ages_reweight"

mod <- SS_read(here('models', '_sensitivities', "STAR_request7b_CCFRP_rec_ages"))

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       #extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$sigma_R_info #0.86

##Comparison plots
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_request7b_CCFRP_rec_ages"),
                                                 file.path('_sensitivities', "STAR_request7b_CCFRP_rec_ages_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'Add CCFRP growth fleet ages into CCFRP fleet /n Add som rec growth fleet ages into rec fleet',
                                     'Same as above but reweight ccfrp and rec ages'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "topright",
                    plotdir = here('models', '_sensitivities', "STAR_request7b_CCFRP_rec_ages_reweight"))
dev.off()

#Non reweighted and reweighted runs are similar. Can present only reweighted


#For STAR presentation
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_request7_CCFRPages_reweight"),
                                                 file.path('_sensitivities', "STAR_request7b_CCFRP_rec_ages_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'CCFRP growth fleet ages to CCFRP fleet',
                                     'CCFRP and some rec growth fleet ages to CCFRP and rec fleet'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "topright",
                    plotdir = here('models', '_sensitivities', "STAR_request7b_CCFRP_rec_ages_reweight"))
dev.off()

plot_compare_growth(models = xx,
                    new_name = "STAR_request7b_CCFRP_rec_ages_reweight",
                    legend_names = c('Base',
                                     'CCFRP ages to CCFRP fleet',
                                     'CCFRP and some rec growth fleet ages to fleets'))

######-
## Request 7b2 - assign growth fleet and Abrams ages elsewhere --------------------------------------------------------

#Remove the growth fleet CAAL and assign CCFRP ages and Rec ages to their fleets
#This wasn't shown to the panel but was mentioned. The rationale is that based on 
#rec_length_and_laa_boxplots.png, the Abrams lengths more align with lengths from recfin

new_name <- "STAR_request7b2_CCFRP_recAbrams_ages"

mod <- base_mod

#Remove growth fleet CAAL data 
mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])

#Add in other Rec related CAAL and assign to that fleet
otherRec.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_AbramsRec_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 2) %>%
  as.data.frame()
names(otherRec.CAAL) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, otherRec.CAAL)

#Add in CCFRP only CAAL and assign to that fleet
ccfrp.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_ccfrp_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 4) %>%
  as.data.frame()
names(ccfrp.CAAL) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, ccfrp.CAAL)


#Set francis weight for growth fleet ages to be the same for ccfrp and rec
mod$ctl$Variance_adjustment_list[6,2] <- 2
mod$ctl$Variance_adjustment_list[7,] <- mod$ctl$Variance_adjustment_list[6,]
mod$ctl$Variance_adjustment_list[7,2] <- 4
rownames(mod$ctl$Variance_adjustment_list)[7] <- "Variance_adjustment_list7"


# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


######-
## Request 7b2 reweight - assign growth fleet and Abrams only elsewhere and reweight --------------------------------------------------------

#Copy model from request 7b2 and now reweight

new_name <- "STAR_request7b2_CCFRP_recAbrams_ages_reweight"

mod <- SS_read(here('models', '_sensitivities', "STAR_request7b2_CCFRP_recAbrams_ages"))

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       #extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$sigma_R_info #0.85

##Comparison plots
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_request7b2_CCFRP_recAbrams_ages"),
                                                 file.path('_sensitivities', "STAR_request7b2_CCFRP_recAbrams_ages_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'Add CCFRP growth fleet ages into CCFRP fleet and Abrams growth fleet ages into rec fleet',
                                     'Same as above but reweight ccfrp and rec ages'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "topright",
                    plotdir = here('models', '_sensitivities', "STAR_request7b2_CCFRP_recAbrams_ages_reweight"))
dev.off()

#Non reweighted and reweighted runs are similar. Can present only reweighted


#For STAR presentation
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_request7_CCFRPages_reweight"),
                                                 file.path('_sensitivities', "STAR_request7b2_CCFRP_recAbrams_ages_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'CCFRP growth fleet ages to CCFRP fleet',
                                     'CCFRP and Abrams growth fleet ages to CCFRP and rec fleet'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "topright",
                    plotdir = here('models', '_sensitivities', "STAR_request7b2_CCFRP_recAbrams_ages_reweight"))
dev.off()



######-
## Request 8 - growth fleet selectivity --------------------------------------------------------

#Explore asymptotic shaped selectivity for growth fleet 

new_name <- "STAR_request8_growthselex"

dir.create(here(sens_dir, new_name))


#Turn on length based selectivity for growth fleet
mod <- base_mod

#hold

#Size selex to 24
mod$ctl$size_selex_types$Pattern[3] <- 24

#Add in the new growth parameters by copying the rec ones
selex_new <- mod$ctl$size_selex_parms
selex_growth <- selex_new[19:24,]

rownames(selex_growth) <- c(
"SizeSel_P_1_CA_Growth(3)",
"SizeSel_P_2_CA_Growth(3)",
"SizeSel_P_3_CA_Growth(3)",
"SizeSel_P_4_CA_Growth(3)",
"SizeSel_P_5_CA_Growth(3)",
"SizeSel_P_6_CA_Growth(3)")

selex_new1 <- rbind(selex_new[1:12,],
                    selex_growth,
                    selex_new[13:24,])
#View(selex_new1)
mod$ctl$size_selex_parms <- selex_new1

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


######-
## Request 8b - growth fleet selectivity --------------------------------------------------------

#Explore dome shaped selectivity for growth fleet 

new_name <- "STAR_request8b_growthselexdomed"

dir.create(here(sens_dir, new_name))


#Turn on length based selectivity for growth fleet
mod <- base_mod

#hold

#Size selex to 24
mod$ctl$size_selex_types$Pattern[3] <- 24

#Add in the new growth parameters by copying the rec ones
selex_new <- mod$ctl$size_selex_parms
selex_growth <- selex_new[19:24,]

rownames(selex_growth) <- c(
"SizeSel_P_1_CA_Growth(3)",
"SizeSel_P_2_CA_Growth(3)",
"SizeSel_P_3_CA_Growth(3)",
"SizeSel_P_4_CA_Growth(3)",
"SizeSel_P_5_CA_Growth(3)",
"SizeSel_P_6_CA_Growth(3)")

selex_new1 <- rbind(selex_new[1:12,],
                    selex_growth,
                    selex_new[13:24,])
#View(selex_new1)
mod$ctl$size_selex_parms <- selex_new1

mod$ctl$size_selex_parms["SizeSel_P_4_CA_Growth(3)", 
                         c("PHASE")] <- 4
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
alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main", "alternative_sigma_R"]


#Request 8 comparison with the base model
xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', 'STAR_request8_growthselex')))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base model',
                                     'Growth selectivity asymptotic'),
                    subplots = c(1, 3, 9, 11), print = TRUE, plotdir = here(sens_dir, new_name))

r4ss::plot_twopanel_comparison(xx,
                               dir = here('models', '_sensitivities', 'STAR_request8_growthselex'),
                               filename = "_sigmaR_comparison.png",
                               legendlabels = c('Base model', 'Growth selectivity asymptotic'),
                               legendloc = 'bottomleft',
                               hessian = FALSE,
                               subplot1 = 1,
                               subplot2 = 3,
                               endyrvec = 2025)

plot_compare_growth(models = xx,
                    legend_names = c("base","Request 8"))


# ######-
# ## Request 11 - redo request 7b using additional rec sources NOT USED --------------------------------------------------------
# 
# #During discussion it was noted that surrendered fish and CDFW Gfish columns should be switched
# #To capture all ages from recreational sources, we should add both in.
# #Thus redo request 7b but include the 55 fish from RBG sources
# 
# new_name <- "STAR_request11_redo7b"
# 
# mod <- base_mod
# 
# #Remove growth fleet CAAL data
# mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])
# 
# #Add in other Rec related CAAL and assign to that fleet
# otherRec.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_possibleRec_withRBG_unsexed_10_50_1_60.csv")) %>%
#   dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
#   dplyr::mutate(ageerr = 1) %>%
#   dplyr::mutate(fleet = 2) %>%
#   as.data.frame()
# names(otherRec.CAAL) <- names(mod$dat$agecomp)
# 
# mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, otherRec.CAAL)
# 
# #Add in CCFRP only CAAL and assign to that fleet
# ccfrp.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_ccfrp_unsexed_10_50_1_60.csv")) %>%
#   dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
#   dplyr::mutate(ageerr = 1) %>%
#   dplyr::mutate(fleet = 4) %>%
#   as.data.frame()
# names(ccfrp.CAAL) <- names(mod$dat$agecomp)
# 
# mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, ccfrp.CAAL)
# 
# 
# #Set francis weight for growth fleet ages to be the same for ccfrp and rec
# mod$ctl$Variance_adjustment_list[6,2] <- 2
# mod$ctl$Variance_adjustment_list[7,] <- mod$ctl$Variance_adjustment_list[6,]
# mod$ctl$Variance_adjustment_list[7,2] <- 4
# rownames(mod$ctl$Variance_adjustment_list)[7] <- "Variance_adjustment_list7"
# 
# 
# # Write model and run
# SS_write(mod, here(sens_dir, new_name),
#          overwrite = TRUE)
# 
# r4ss::run(dir = here(sens_dir, new_name),
#           exe = here('models/ss3_win.exe'),
#           extras = '-nohess',
#           show_in_console = TRUE,
#           skipfinished = FALSE)
# 
# pp <- SS_output(here(sens_dir, new_name))
# SS_plots(pp, plot = c(1:26))
# plot_sel_all(pp)
# 
# 
# ######-
# ##Copy model from request 11 and now reweight
# 
# new_name <- "STAR_request11_redo7b_reweight"
# 
# mod <- SS_read(here('models', '_sensitivities', "STAR_request11_redo7b"))
# 
# #Run based on weights set to one
# mod$ctl$Variance_adjustment_list$value <-  1
# 
# SS_write(mod,
#          dir = here('models', '_sensitivities', new_name),
#          overwrite = TRUE)
# 
# r4ss::run(dir = here('models', '_sensitivities', new_name),
#           exe = here('models/ss3_win.exe'),
#           extras = '-nohess',
#           show_in_console = TRUE,
#           skipfinished = FALSE)
# 
# #Now iteratively reweight based on weight = 1 run and reassign weights
# pp <- SS_output(here('models', '_sensitivities', new_name))
# iter <- 3
# dw <- r4ss::tune_comps(replist = pp,
#                        option = 'Francis',
#                        dir = here('models', '_sensitivities', new_name),
#                        exe = here('models/ss3_win.exe'),
#                        niters_tuning = iter,
#                        #extras = '-nohess',
#                        allow_up_tuning = TRUE,
#                        show_in_console = TRUE)
# 
# pp <- SS_output(here('models', '_sensitivities', new_name))
# SS_plots(pp, plot = c(1:26))
# plot_sel_all(pp)
# 
# pp$sigma_R_info #0.98
# 
# #Our reweighting goes wonky and wants to upweight rec ages, which then induces
# #odd bias adjustment. Since we dont like this run anyway, dont suggest doing
# #request 11 with this data added.
# 
# #Comparison plots
# 
# xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
#                                       subdir = c(base_mod_name,
#                                                  file.path('_sensitivities', "STAR_request7_CCFRPages_reweight"),
#                                                  file.path('_sensitivities', "STAR_request7b_CCFRP_rec_ages_reweight"),
#                                                  file.path('_sensitivities', "STAR_request11_redo7b_reweight"))))
# 
# SSsummarize(xx) |>
#   SSplotComparisons(legendlabels = c('Base',
#                                      'CCFRP growth fleet ages to CCFRP fleet',
#                                      'CCFRP and some rec growth fleet ages to CCFRP and rec fleet',
#                                      'CCFRP and some more rec growth fleet ages to CCFRP and rec fleet'),
#                     subplots = c(1,3, 9, 11), print = TRUE, legendloc = "topright",
#                     plotdir = here('models', '_sensitivities', new_name))
# dev.off()



######-
## Request 11 - redo request 7, assign CCFRP ages, based on what was presented to panel --------------------------------------------------------

new_name <- "STAR_request11_CCFRPages"

mod <- base_mod

#Remove growth fleet CAAL data 
mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])

#Add in CCFRP only marginal ages and assign to that fleet
ccfrp.age <- read.csv(here("data", "forSS3", "Acomps_noncommercial_ccfrp_unsexed_raw_1_60.csv")) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 4) %>%
  as.data.frame()
names(ccfrp.age) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, ccfrp.age)


#Set francis weight for growth fleet ages to be the same for ccfrp and rec
mod$ctl$Variance_adjustment_list[6,2] <- 4


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


#Copy model from request 11 above and now reweight

new_name <- "STAR_request11_CCFRPages_reweight"

mod <- SS_read(here('models', '_sensitivities', "STAR_request11_CCFRPages"))

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       #extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$sigma_R_info #0.73




######-
## Request 11 - redo request 7b, assign CCFRP and rec ages, based on what was presented to panel --------------------------------------------------------

new_name <- "STAR_request11_CCFRP_rec_ages"

mod <- base_mod

#Remove growth fleet CAAL data 
mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])

#Add in other Rec related marginal ages and assign to that fleet
otherRec.age <- read.csv(here("data", "forSS3", "Acomps_possibleRec_unsexed_raw_1_60.csv")) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 2) %>%
  as.data.frame()
names(otherRec.age) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, otherRec.age)

#Add in CCFRP only marginal ages and assign to that fleet
ccfrp.age <- read.csv(here("data", "forSS3", "Acomps_noncommercial_ccfrp_unsexed_raw_1_60.csv")) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 4) %>%
  as.data.frame()
names(ccfrp.age) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, ccfrp.age)


#Set francis weight for growth fleet ages to be the same for ccfrp and rec
mod$ctl$Variance_adjustment_list[6,2] <- 2
mod$ctl$Variance_adjustment_list[7,] <- mod$ctl$Variance_adjustment_list[6,]
mod$ctl$Variance_adjustment_list[7,2] <- 4
rownames(mod$ctl$Variance_adjustment_list)[7] <- "Variance_adjustment_list7"


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


#Copy model from request 11 and now reweight

new_name <- "STAR_request11_CCFRP_rec_ages_reweight"

mod <- SS_read(here('models', '_sensitivities', "STAR_request11_CCFRP_rec_ages"))

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       #extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$sigma_R_info #0.78


#Comparison plots for STAR presentation
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_request11_CCFRPages_reweight"),
                                                 file.path('_sensitivities', "STAR_request11_CCFRP_rec_ages_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'CCFRP growth fleet ages to marginal CCFRP fleet',
                                     'CCFRP and some rec growth fleet ages to marginal CCFRP and rec fleet'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "bottomleft",
                    col = rich.colors.short(3),
                    plotdir = here('models', '_sensitivities', new_name))
dev.off()

plot_compare_growth(models = xx,
                    new_name = new_name,
                    legend_names = c('Base',
                                     'CCFRP ages to marginal CCFRP fleet',
                                     'CCFRP and some rec growth fleet ages to marginal fleets'))





######-
## STAR Request 12 Step 1--------------------------------------------------------
# Adjust main period start to 1994

new_name <- 'STAR_Req12_Step1'

mod <- base_mod

#mod$ctl$recdev_early_phase <- -5 #turn off early recdevs
mod$ctl$MainRdevYrFirst <- 1994

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main", "alternative_sigma_R"]


## STAR Request 12 Step 2--------------------------------------------------------
# Adjust main period start to 1994

new_name <- 'STAR_Req12_Step2'

# Update sigmaR 1st time to 0.8845576

mod <- base_mod

#mod$ctl$recdev_early_phase <- -5 #turn off early recdevs
mod$ctl$MainRdevYrFirst <- 1994
mod$ctl$SR_parms["SR_sigmaR", "INIT"] <- as.numeric(alt_sigmaR)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main", "alternative_sigma_R"]

## STAR Request 12 Step 3--------------------------------------------------------
# Adjust main period start to 1994

new_name <- 'STAR_Req12_Step3'

# Update sigmaR 1st time to 1.053906

mod <- base_mod

#mod$ctl$recdev_early_phase <- -5 #turn off early recdevs
mod$ctl$MainRdevYrFirst <- 1994
mod$ctl$SR_parms["SR_sigmaR", "INIT"] <- as.numeric(alt_sigmaR)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main", "alternative_sigma_R"]  # 1.140418

##Comparison plots
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_Req12_Step1"),
                                                 file.path('_sensitivities', "STAR_Req12_Step3"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'Rec Main 1994',
                                     'Rec Main 1994, sigmaR = 1.0534'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "topright",
                    plotdir = here('models', '_sensitivities', "STAR_Req12_Step3"))
dev.off()


######-
## Request 13 - assign growth fleet ages to early year and reweight --------------------------------------------------------

#Collapse the growth fleet comps into 1920

new_name <- "STAR_request13_collapseGrowth"

mod <- base_mod

#Remove growth fleet CAAL data and add in CCFRP only CAAL and assign to that fleet
mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])

CAAL.1920 <- read.csv(here("data", "forSS3", "CAAL_noncommercial_all1920_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 3) %>%
  as.data.frame()
names(CAAL.1920) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, CAAL.1920)

# Write model and run
SS_write(mod, here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


#Copy model from request 13 and now reweight

new_name <- "STAR_request13_collapseGrowth_reweight"

mod <- SS_read(here('models', '_sensitivities', "STAR_request13_collapseGrowth"))

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       #extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$sigma_R_info #0.82

######-
## Request  13 remove the growth fleet completely
##
new_name <- 'STAR_Request13_nogrowthfleet'
old_name <- base_mod_name

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here(sens_dir, new_name),
               overwrite = TRUE)

mod <- SS_read(here(sens_dir,new_name))

#Remove growth fleet CAAL data 
mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])


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

######-
## Request 13 remove the growth fleet completely - reweighting

new_name <- "STAR_request13_nogrowthfleet_reweight"

mod <- SS_read(here('models', '_sensitivities', "STAR_request13_nogrowthfleet"))

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       #extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

alt_sigmaR <- pp$sigma_R_info[pp$sigma_R_info$period == "Main", "alternative_sigma_R"]
pp$sigma_R_info

##Request 13 comparison Comparison plots
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_request13_collapseGrowth_reweight"),
                                                 file.path('_sensitivities', "STAR_request13_nogrowthfleet_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'STAR_request13_collapseGrowth_reweight',
                                     'STAR_request13_nogrowthfleet_reweight'),
                     col = rich.colors.short(3),
                    subplots = c(2,4, 9, 11), print = TRUE, legendloc = "topright",
                    plotdir = here('models', '_sensitivities', "STAR_request13_collapseGrowth_reweight"))
dev.off()

plot_compare_growth(models = xx,
                                new_name = "STAR_request13_collapseGrowth_reweight",
                                legend_names = c('Base',
                                     'STAR_request13_collapseGrowth_reweight',
                                     'STAR_request13_nogrowthfleet_reweight'),
                                max_age = 60) 

#####-
## Request 14 - update bias adjust for request 7 run --------------------------------------------------------


new_name <- "STAR_request14_CCFRP_BiasAdj"
old_name <- "STAR_request7_CCFRPages_reweight"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', '_sensitivities', old_name), 
               dir.new = here('models', '_sensitivities', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models', '_sensitivities', old_name),"Report.sso"),
          to = file.path(here('models', '_sensitivities', new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', '_sensitivities', old_name),"CompReport.sso"),
          to = file.path(here('models', '_sensitivities', new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', '_sensitivities', old_name),"warning.sso"),
          to = file.path(here('models', '_sensitivities', new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', '_sensitivities', old_name),"covar.sso"),
          to = file.path(here('models', '_sensitivities', new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here('models','_sensitivities', new_name))

pp <- SS_output(here('models', '_sensitivities', new_name), covar = TRUE)


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
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$sigma_R_info #0.8

# Check new bias adjustment recommendation

pp$breakpoints_for_bias_adjustment_ramp

biasadj <- SS_fitbiasramp(pp, verbose = TRUE)

# These look good.  No need to adjust again.  Now reweight

#Compare this to the previous request 7 run without bias adj
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(file.path('_sensitivities', "STAR_request14_CCFRP_BiasAdj"),
                                                 file.path('_sensitivities', "STAR_request7_CCFRPages_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Request 7 CCFRP growth fleet ages',
                                     'Request 14 adjust bias ramp'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "bottomleft",
                    plotdir = here('models', '_sensitivities', new_name))

#####-
## Now reweight. Given request 7 was already reweighted before bias correct
## and doing the following reweighting did not greatly alter the model weights
## nor the suggested bias adj ramp. I dont think this step is necessary.
# We can present only the bias adjusted run

new_name <- "STAR_request14_CCFRP_BiasAdj_reweight"
old_name <- "STAR_request14_CCFRP_BiasAdj"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', '_sensitivities', old_name), 
               dir.new = here('models', '_sensitivities', new_name),
               overwrite = TRUE)

file.copy(from = file.path(here('models', '_sensitivities', old_name),"Report.sso"),
          to = file.path(here('models', '_sensitivities', new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', '_sensitivities', old_name),"CompReport.sso"),
          to = file.path(here('models', '_sensitivities', new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', '_sensitivities', old_name),"warning.sso"),
          to = file.path(here('models', '_sensitivities', new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here('models', '_sensitivities', old_name),"covar.sso"),
          to = file.path(here('models', '_sensitivities', new_name),"covar.sso"), overwrite = TRUE)


mod <- SS_read(here('models', '_sensitivities', new_name))


##
#Make Changes and run models
##

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       #extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

#Compare this to the previous request 7 run without bias adj
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(file.path('_sensitivities', "STAR_request7_CCFRPages_reweight"),
                                                 file.path('_sensitivities', "STAR_request14_CCFRP_BiasAdj"),
                                                 file.path('_sensitivities', "STAR_request14_CCFRP_BiasAdj_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Request 7 CCFRP growth fleet ages',
                                     'Request 14 adjust bias ramp',
                                     'Request 14 adjust bias ramp reweight'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "bottomleft",
                    plotdir = here('models', '_sensitivities', new_name))



######-
## Request 14 Add CCFRP to the CCFRP fleet and Abrams to the growth fleet --------------------------------------------------------

#Copy over Model 7 and then keep only positive years on 2010 and 2011
#in the growth fleet

new_name <- "STAR_request14_CCFRPages_Abramsgrowth"

mod <- base_mod

#Remove growth fleet CAAL data and add in CCFRP only CAAL and assign to that fleet

aa <- mod$dat$agecomp
aa <- aa %>%
    mutate(year = case_when(fleet == 3  ~ -abs(year), 
                             TRUE ~ year)) %>%
    mutate(year = case_when(fleet == 3 & year %in% c(-2010,-2011) ~ abs(year), 
                            TRUE ~ year))
mod$dat$agecomp <- aa


ccfrp.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_ccfrp_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 4) %>%
  as.data.frame()
names(ccfrp.CAAL) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, ccfrp.CAAL)

#Set francis weight for growth fleet ages to be the same as for ccfrp
mod$ctl$Variance_adjustment_list[7,] <- mod$ctl$Variance_adjustment_list[6,]
mod$ctl$Variance_adjustment_list[7,2] <- 4
rownames(mod$ctl$Variance_adjustment_list)[7] <- "Variance_adjustment_list7"


SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)


pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)


######-
## Request 14 reweight CCFRP to CCFRP fleet and Abrams to growth ---------------------------------------------

#Copy request 14 and reweight
new_name <- "STAR_request14_CCFRPages_Abramsgrowth_reweight"

mod <- base_mod

#Remove growth fleet CAAL data and add in CCFRP only CAAL and assign to that fleet

aa <- mod$dat$agecomp
aa <- aa %>%
    mutate(year = case_when(fleet == 3  ~ -abs(year), 
                             TRUE ~ year)) %>%
    mutate(year = case_when(fleet == 3 & year %in% c(-2010,-2011) ~ abs(year), 
                            TRUE ~ year))
mod$dat$agecomp <- aa


ccfrp.CAAL <- read.csv(here("data", "forSS3", "CAAL_noncommercial_ccfrp_unsexed_10_50_1_60.csv")) %>%
  dplyr::mutate(dplyr::across(Lbin_lo:Lbin_hi, ~ match(., mod$dat$lbin_vector))) %>%
  dplyr::mutate(ageerr = 1) %>%
  dplyr::mutate(fleet = 4) %>%
  as.data.frame()
names(ccfrp.CAAL) <- names(mod$dat$agecomp)

mod$dat$agecomp <- dplyr::bind_rows(mod$dat$agecomp, ccfrp.CAAL)

#Set francis weight for growth fleet ages to be the same as for ccfrp
mod$ctl$Variance_adjustment_list[7,] <- mod$ctl$Variance_adjustment_list[6,]
mod$ctl$Variance_adjustment_list[7,2] <- 4
rownames(mod$ctl$Variance_adjustment_list)[7] <- "Variance_adjustment_list7"

#Run based on weights set to one
mod$ctl$Variance_adjustment_list$value <-  1

SS_write(mod,
         dir = here('models', '_sensitivities', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_sensitivities', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)

#Now iteratively reweight based on weight = 1 run and reassign weights
pp <- SS_output(here('models', '_sensitivities', new_name))
iter <- 3
dw <- r4ss::tune_comps(replist = pp, 
                       option = 'Francis', 
                       dir = here('models', '_sensitivities', new_name), 
                       exe = here('models/ss3_win.exe'), 
                       niters_tuning = iter, 
                       #extras = '-nohess',
                       allow_up_tuning = TRUE,
                       show_in_console = TRUE)

pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$sigma_R_info #alternate 0.84


#Compare this to the previous request 7 run without bias adj
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(file.path('_sensitivities', "STAR_request14_CCFRPages_Abramsgrowth"),
                                                 file.path('_sensitivities', "STAR_request14_CCFRPages_Abramsgrowth_reweight"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Request 14 Abrams',
                                     'Request 14 Abrams reweight'),
                    subplots = c(1,3, 9, 11), print = TRUE, legendloc = "bottomleft",
                    plotdir = here('models', '_sensitivities', new_name))



####------------------------------------------------#
## STAR_request14_CCFRPAbrams_biasAdjRamp ----
####------------------------------------------------#

#Test whether a change to bias adjsut ramp is really necessary. 
#Switch to suggestion from hessian model

new_name <- "STAR_request14_CCFRPAbrams_biasAdj"
old_name <- "STAR_request14_CCFRPages_Abramsgrowth_reweight"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here(sens_dir, old_name), 
               dir.new = here(sens_dir, new_name),
               overwrite = TRUE)

file.copy(from = file.path(here(sens_dir, old_name),"Report.sso"),
          to = file.path(here(sens_dir, new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here(sens_dir, old_name),"CompReport.sso"),
          to = file.path(here(sens_dir,new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here(sens_dir, old_name),"warning.sso"),
          to = file.path(here(sens_dir,new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here(sens_dir, old_name),"covar.sso"),
          to = file.path(here(sens_dir,new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here(sens_dir,new_name))

pp <- SS_output(here(sens_dir,new_name), covar = TRUE)


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
         dir = here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)



####------------------------------------------------#
## STAR_request14_CCFRPages_Abrams_biasAdjRamp_alt ----
####------------------------------------------------#

#Test whether a change to bias adjsut ramp is really necessary. 
#Switch to a manual value based on trying to match data rather than the algorithm

new_name <- "STAR_request14_CCFRPages_Abrams_biasAdjRamp_alt"
old_name <- "STAR_request14_CCFRPages_Abramsgrowth_reweight"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here(sens_dir, old_name), 
               dir.new = here(sens_dir, new_name),
               overwrite = TRUE)

file.copy(from = file.path(here(sens_dir, old_name),"Report.sso"),
          to = file.path(here(sens_dir, new_name),"Report.sso"), overwrite = TRUE)
file.copy(from = file.path(here(sens_dir, old_name),"CompReport.sso"),
          to = file.path(here(sens_dir,new_name),"CompReport.sso"), overwrite = TRUE)
file.copy(from = file.path(here(sens_dir, old_name),"warning.sso"),
          to = file.path(here(sens_dir,new_name),"warning.sso"), overwrite = TRUE)
file.copy(from = file.path(here(sens_dir, old_name),"covar.sso"),
          to = file.path(here(sens_dir,new_name),"covar.sso"), overwrite = TRUE)

mod <- SS_read(here(sens_dir,new_name))

pp <- SS_output(here(sens_dir,new_name), covar = TRUE)


##
#Make Changes
##

#Update bias adjust? Yes, all values
pp$breakpoints_for_bias_adjustment_ramp

biasadj <- SS_fitbiasramp(pp, verbose = TRUE)

#This pattern doesnt look good. Match the data themselves
mod$ctl$last_early_yr_nobias_adj <- 1995
mod$ctl$first_yr_fullbias_adj <- 1998
mod$ctl$last_yr_fullbias_adj <- 2016.5 
mod$ctl$first_recent_yr_nobias_adj <- 2018 
mod$ctl$max_bias_adj <- 0.55

##
#Output files and run
##

SS_write(mod,
         dir = here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          #extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

pp$sigma_R_info #0.85

#This looks good enough

#Reweight this once doesn't greatly differ weights, does downweight CCFRP 
#but result isn't greatly different going forward with what we have



#Compare this to the previous request 7 run without bias adj
xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path('_sensitivities', "STAR_request14_CCFRP_BiasAdj"),
                                                 file.path('_sensitivities', "STAR_request14_CCFRPages_Abrams_biasAdjRamp_alt"))))

SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base',
                                     'Request 7 CCFRP bias adj',
                                     'Request 14 CCFRP Abrams reweight bias adj'),
                    subplots = c(2, 4, 9, 11), print = TRUE, legendloc = "bottomleft",
                    col = rich.colors.short(3),
                    plotdir = here('models', '_sensitivities', new_name))



