##########################################################################################
#
# Model runs for 2025 California Quillback Rockfish 
#   By: Brian Langseth, Melissa Monk, Julia Coates
#
##########################################################################################
#Alternative to devtools if it doesn't work on your machine
#pak::pkg_install("pfmc-assessments/PEPtools")
#pak::pkg_install("r4ss/r4ss") #Version 1.50.0

#Load packages
library(r4ss)
library(PEPtools)
library(here)
library(dplyr)
library(tictoc)


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
               use_ss_new = TRUE,
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

new_name <- "0_0_2_2025setup"
old_name <- "0_0_1_2021base"


##
#Copy inputs
##

#Use ss_new inputs so have commonality with other SS3 models
copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               use_ss_new = TRUE,
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

#Model runs



####------------------------------------------------#
## 0_1_0_updateBio  ----
####------------------------------------------------#

#Update biological relationships

new_name <- "0_1_0_updateBio"
old_name <- "0_0_2_2025setup"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               use_ss_new = TRUE,
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
  c(0.01, 0.15, m_init, round(log(m_init), 2), m_se, m_prior, 2)


### Update growth prior ----

mod$ctl$Growth_Age_for_L1 <- 0

#Growth curve from all individuals. Follows from quillback_growth.R
vb_k <- 0.178
vb_linf <- 40.997
vb_l0 <- 3.922
vb_cv0 <- 0.226
vb_cv1 <- 0.065
vb_prior <- 0

mod$ctl$MG_parms['L_at_Amin_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0, 10, vb_l0, vb_l0, 0, vb_prior, -9)
mod$ctl$MG_parms['L_at_Amax_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(35, 50, vb_linf, vb_linf, 0, vb_prior, -9)
mod$ctl$MG_parms['VonBert_K_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.03, 0.3, vb_k, vb_k, 0, vb_prior, -9)
mod$ctl$MG_parms['CV_young_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.5, vb_cv0, vb_cv0, 0, vb_prior, -9)
mod$ctl$MG_parms['CV_old_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0.01, 0.5, vb_cv1, vb_cv1, 0, vb_prior, -9)


### Update LW relationship ----

lw_a <- 1.599251e-5
lw_b <- 3.076563
lw_prior <- 0

mod$ctl$MG_parms['Wtlen_1_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(0, 0.1, lw_a, lw_a, 0, vb_prior, -9)
mod$ctl$MG_parms['Wtlen_2_Fem_GP_1', c('LO', 'HI', 'INIT', 'PRIOR', 'PR_SD', 'PR_type', 'PHASE')] <-
  c(2, 4, lw_b, lw_b, 0, vb_prior, -9)


### Update maturity ----

#With updated data as of Feb 28, 2025
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

eggs_b <- 3.702
eggs_a <- round((0.00007809 * 10^eggs_b)/1000000, 10)
fec_prior <- 0 #turning off

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



r##########################################################################################-
#                     ---- 2025 exploration runs ----
##########################################################################################-


