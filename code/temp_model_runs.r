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

sens_dir <- here('models', '_sensitivities')
#Enter in base model from which to base sensitivities
base_mod_name <- '5_1_3_preStarBase' #<---------------UPDATE WHEN CHANGE
base_mod <- SS_read(here('models', base_mod_name))

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










xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))

