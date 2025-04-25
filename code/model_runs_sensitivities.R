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
base_mod_name <- '3_2_2_SetUpExtraSE' #<---------------UPDATE WHEN CHANGE
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


## Drop length data by fleet --------------------------------------------------------

#Many options to do this
# 1. downweight the comps (but selectivity could get squiggly)
# 2. remove and then fix selectivity to something (i.e. maturity, another fleet)
# 3. remove (but then the selectivity is mostly defined by the bounds and do we want this?)
#Ultimately going with option 3 just to see. 

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


## Drop age data by fleet --------------------------------------------------------

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


## Drop index fleets --------------------------------------------------------

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

## Drop data sets using lambdas --------------------------------------------------------

#Example below for the PR index

new_name <- 'leaveOut_pr'

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
mod[["ctl"]][["N_lambdas"]] <- 1

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
# Other sensitivities ----
####------------------------------------------------#


## Reduce catches --------------------------------------------------------



## Continue on here