#R script to temporary hold model runs before copying over to model_runs.R
#Avoids merge conflicts

#Load packages
library(r4ss)
library(PEPtools)
library(here)
library(dplyr)
library(tictoc)
library(nwfscSurvey)
source(here('code/selexComp.R'))



#Enter in base model from which to base sensitivities
base_mod_name <- '4_2_1_propBase' #<---------------UPDATE WHEN CHANGE
base_mod <- SS_read(here('models', base_mod_name))

#Create the sensitivities directory
#sens_dir <- here('models', '_sensitivities')
#dir.create(sens_dir)

#Set up fleet converter
fleet.converter <- base_mod$dat$fleetinfo %>%
  dplyr::mutate(fleet_no_num = 1:5,
                fleet = c("com", "rec", "growth", "ccfrp", "rov")) %>%
  dplyr::select(fleetname, fleet_no_num, fleet)

####------------------------------------------------#
# Biology sensitivities ----
####------------------------------------------------#


## Biological parameters to 2021 assessment values --------------------------------------------------------
### Fecundity to E.J.'s Pteropodus values'
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

### Maturity to 2021 estimates

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


## Max Age 70 --------------------------------------------------------
new_name <- 'maxAge70'

mod <- base_mod

#change values
maxAge <- 70 #this is the maximum age among BC entries in Claires dataset
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

## Max Age 75 --------------------------------------------------------
new_name <- 'maxAge75'

mod <- base_mod

#change values
maxAge <- 75 #this is the maximum age among BC entries in Claires dataset
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

## Max Age 80 --------------------------------------------------------
new_name <- 'maxAge80'

mod <- base_mod

#change values
maxAge <- 80 #this is the maximum age among BC entries in Claires dataset
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


