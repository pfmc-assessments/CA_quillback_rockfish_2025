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

new_name <- "mhm_test"

mod <- base_mod

#turn off commercial ages
aa <- mod$dat$agecomp
aa <- aa %>%
    mutate(year = -abs(year)) #case_when(fleet == 1  ~ -abs(year), 
                   #          TRUE ~ year)) #%>%
  #  mutate(year = case_when(fleet == 3 & year %in% c(-2010,-2011) ~ abs(year), 
   #                         TRUE ~ year))
mod$dat$agecomp <- aa

mod$ctl$Growth_Age_for_L1 <- 0

mod$ctl$MG_parms$INIT[2] <-  3.99192977
mod$ctl$MG_parms$INIT[3] <- 41.17135518	
mod$ctl$MG_parms$INIT[4] <-  0.17840051	
mod$ctl$MG_parms$INIT[5] <-  0.20157461
mod$ctl$MG_parms$INIT[6] <-  0.06413399

mod$ctl$MG_parms$PRIOR[2:6] <- mod$ctl$MG_parms$INIT[2:6]
#negative phase 
mod$ctl$MG_parms$PHASE[2:6] <- -9
##
#Output files and run
##

SS_write(mod,
         dir = here(sens_dir, new_name),
         overwrite = TRUE)

r4ss::run(dir = here(sens_dir, new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here(sens_dir, new_name))
SS_plots(pp, plot = c(1:26))
plot_sel_all(pp)

