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
mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"] <- -abs(mod$dat$agecomp[mod$dat$agecomp$fleet == 3, "year"])

aa <- mod$dat$agecomp
aa <- aa %>%
    mutate(year = case_when(fleet = 3  ~ -abs(year), 
                             TRUE ~ year)) %>%
    mutate(year = case_when(fleet =3 & year = 2010 ~ abs(year), 
                            TRUE ~ year))
View(aa)
& year %in% c(2010,2011) 

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




