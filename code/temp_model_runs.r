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
source(here('code/selexComp.R'))
source(here('code/model_runs_growth_comparison.R'))

sens_dir <- here('models', '_sensitivities')
#Enter in base model from which to base sensitivities
base_mod_name <- '5_1_3_preStarBase' #<---------------UPDATE WHEN CHANGE
base_mod <- SS_read(here('models', base_mod_name))

######-
## Request 14 Add CCFRP to the CCFRP fleet and Abrams to the growth fleet

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


pp <- SS_output(here('models', '_sensitivities', new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

######-
## Request 14 reweight CCFRP to CCFRP fleet and Abrams to growth---------------------------------------------

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

pp$sigma_R_info #alternate 0.84

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

