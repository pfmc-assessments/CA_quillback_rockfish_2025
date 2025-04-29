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
# Biology sensitivities ----
####------------------------------------------------#


## Fix biological parameters --------------------------------------------------------

new_name <- 'fix_fecundity_EJest'

mod <- base_mod

#change fixed values


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