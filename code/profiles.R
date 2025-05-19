##########################################################################################-
#
# This script runs the required proviles for 2025 California Quillback Rockfish 
#   By: Brian Langseth, Melissa Monk, Julia Coates
#
#
##########################################################################################-

# Based on the TOR, required profiles include M, h, and R0
# This script is set up to run all profiles, jitters, and retros together, but also
# has scripts to run each profile individually

#pak::pkg_install("pfmc-assessments/nwfscDiag")
#devtools::load_all("U:/Other github repos/nwfscDiag")
library(nwfscDiag)
library(r4ss)
library(here)
library(tictoc)

base_model <- '5_1_3_preStarBase'

directory <-  here::here("models")
exe_loc <- here::here(directory, 'ss3_win.exe')


## Combination of profiles, jitters, and retros --------------------------------------------------

# Need to run on a model with .exe included in the folder (for jitters)
# Current version of SS3 automatically resets another parameter to phase=1 
# (which for us is CCFRP Q). 
# I tested results when both CCFRP Q and R0 are phase = 1 and very similar. 

file.copy(from = file.path(here('models'), "ss3_win.exe"),
          to = file.path(here('models', base_model), "ss3_win.exe"), overwrite = TRUE)

profile.settings <- nwfscDiag::get_settings_profile(
  parameters =  c("NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)", "SR_sigmaR"),
  low =  c(0.02, 0.40, -0.5, 0.4),
  high = c(0.1, 0.95,  0.5, 1),
  step_size = c(0.005, 0.05, 0.1, 0.1),
  param_space = c('real', 'real', 'relative', 'real'))

#FROM CANARY RUNS: "Should do usepar because stabilizes. Testing this showed instability with some runs when not using it
#No effect with globalpar so using it for consistency"
#Here not doing this
settings <- nwfscDiag::get_settings(
  mydir = directory,
  settings = list(
    base_name = base_model,
    run = c("profile", "jitter", "retro"),
    profile_details = profile.settings,
    exe = exe_loc,
    extras = '-nohess',
    globalpar = FALSE,
    usepar = FALSE,
    init_values_src = 0,
    Njitter = 100, 
    jitter_fraction = 0.5, #default is 0.05
    show_in_console = FALSE,
    retro_yrs = -1:-15))

# set up parallel stuff - runs about 4x faster
future::plan(future::multisession(workers = parallelly::availableCores(omit = 1)))

tictoc::tic()
set.seed(12345)
run_diagnostics(mydir = here('models'), model_settings = settings)
tictoc::toc()

# back to sequential processing
future::plan(future::sequential)


## Individual profiles

# Individual M profile --------------------------------------------------------

#Note that names of parmaeters need to be from ss_new files
profile.settings <- nwfscDiag::get_settings_profile(
  parameters = 'NatM_uniform_Fem_GP_1', 
  low = .02, 
  high = 0.1,
  step_size = 0.005,
  param_space = 'real') 

settings <- nwfscDiag::get_settings(
  mydir = directory,
  settings = list(
    base_name = base_model,
    run = "profile",
    profile_details = profile.settings,
    exe = exe_loc,
    extras = '-nohess',
    usepar = FALSE,
    init_values_src = 0))

# set up parallel stuff - runs about 4x faster
future::plan(future::multisession(workers = parallelly::availableCores(omit = 1)))

tictoc::tic()
run_diagnostics(mydir = here('models'), model_settings = settings)
tictoc::toc()

# back to sequential processing
future::plan(future::sequential)


# Individual Steepness profile -------------------------------------------------------

profile.settings <- nwfscDiag::get_settings_profile(
  parameters = 'SR_BH_steep', 
  low = 0.5, 
  high = 0.95,
  step_size = 0.05,
  param_space = 'real',
  use_prior_like = 1) 

settings <- nwfscDiag::get_settings(
  mydir = directory,
  settings = list(
    base_name = base_model,
    run = "profile",
    profile_details = profile.settings,
    exe = exe_loc,
    extras = '-nohess',
    usepar = FALSE,
    init_values_src = 0))

# set up parallel stuff
future::plan(future::multisession(workers = parallelly::availableCores(omit = 1)))

tictoc::tic()
run_diagnostics(mydir = here('models'), model_settings = settings)
tictoc::toc()

# back to sequential processing
future::plan(future::sequential)


# Individual R0 profile --------------------------------------------------------------

profile.settings <- nwfscDiag::get_settings_profile(
  parameters = 'SR_LN(R0)', 
  low = -0.5, 
  high = 0.5,
  step_size = 0.1,
  param_space = 'relative',
  use_prior_like = 1) 

settings <- nwfscDiag::get_settings(
  mydir = directory,
  settings = list(
    base_name = base_model,
    run = "profile",
    profile_details = profile.settings,
    exe = exe_loc,
    extras = '-nohess',
    usepar = FALSE,
    init_values_src = 0))

# set up parallel stuff
future::plan(future::multisession(workers = parallelly::availableCores(omit = 1)))

tictoc::tic()
run_diagnostics(mydir = here('models'), model_settings = settings)
tictoc::toc()

# back to sequential processing
future::plan(future::sequential)


# Individual sigmaR profile -------------------------------------------------------

profile.settings <- nwfscDiag::get_settings_profile(
  parameters = 'SR_sigmaR', 
  low = 0.4, 
  high = 1,
  step_size = 0.1,
  param_space = 'real',
  use_prior_like = 1) 

settings <- nwfscDiag::get_settings(
  mydir = directory,
  settings = list(
    base_name = base_model,
    run = "profile",
    profile_details = profile.settings,
    exe = exe_loc,
    extras = '-nohess',
    usepar = FALSE,
    init_values_src = 0))

# set up parallel stuff
future::plan(future::multisession(workers = parallelly::availableCores(omit = 1)))

tictoc::tic()
run_diagnostics(mydir = here('models'), model_settings = settings)
tictoc::toc()

# back to sequential processing
future::plan(future::sequential)
 



## Individual other diagnostics

# Individual Jitter ------------------------------------------------------------------

#For running the jitter, I need to copy the exe within the model file
file.copy(from = file.path(here('models'), "ss3_win.exe"),
          to = file.path(here('models', base_model), "ss3_win.exe"), overwrite = TRUE)

settings <- nwfscDiag::get_settings(
  mydir = directory,
  settings = list(
    base_name = base_model,
    run = 'jitter',
    Njitter = 100,
    jitter_fraction = 0.5, #default is 0.05
    exe = exe_loc,
    extras = '-nohess'))

# set up parallel stuff
future::plan(future::multisession(workers = parallelly::availableCores(omit = 1)))

tictoc::tic()
set.seed(12345)
run_diagnostics(mydir = here('models'), model_settings = settings)
tictoc::toc()

# back to sequential processing
future::plan(future::sequential)


# # If need to rerun best jitter
# new_name <- paste0(base_model, '_best_jitter')
# r4ss::copy_SS_inputs(dir.old = here('models', base_model),
#                      dir.new = here('models', new_name))
# file.copy(from = here('models', paste0(base_model, '_jitter_0.05'), 'ss.par_43.sso'),
#           to = here('models', new_name, 'ss.par'))
# mod <- SS_read(here('models', new_name))
# mod$start$init_values_src <- 1
# SS_write(mod)
# 
# pp <- SS_output(here('models', new_name))
# SS_plots(pp)

# Retro (15 yr) ------------------------------------------------------------------

settings <- nwfscDiag::get_settings(
  mydir = directory,
  settings = list(
    base_name = base_model,
    exe = exe_loc,
    run = "retro",
    retro_yrs = -1:-15))

# set up parallel stuff
future::plan(future::multisession(workers = parallelly::availableCores(omit = 1)))

tictoc::tic()
run_diagnostics(mydir = here('models'), model_settings = settings)
tictoc::toc()

# back to sequential processing
future::plan(future::sequential)

# MCMC --------------------------------------------------------------------
#need to change do_recdev to option 2 for use with MCMC
base_model_recdev2 <- "4_2_1a_propBase"
#need to do? 

run_mcmc_diagnostics(
    dir_wd = here('models',base_model_recdev2),
    model = "ss3_win",
    extension = ".exe",
    iter = 200,
    chains = 2,
    interactive = FALSE,
    verbose = FALSE
  )



# ============================================================================ #
# Profile over M while estimating h
#need to modify the base model to estimate h
#making a copy of the base model and changing the phase 
bivar_directory <- here('models', '_bivariate_profiles')
new.dir <- here('models', '_bivariate_profiles',glue::glue(base_model,'_est_h'))
copy_SS_inputs(dir.old = here('models', base_model), 
               dir.new = new.dir,
               overwrite = TRUE)

mod <- SS_read(new.dir)
#estimate h
mod$ctl$SR_parms['SR_BH_steep', c('PHASE')] <- 4

SS_write(mod,
         dir = new.dir,
         overwrite = TRUE)

r4ss::run(dir = new.dir, 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

#Note that names of parmaeters need to be from ss_new files
profile.settings <- nwfscDiag::get_settings_profile(
  parameters = 'NatM_uniform_Fem_GP_1', 
  low = -0.02, 
  high = 0.03,
  step_size = 0.005,
  param_space = 'relative') 

settings <- nwfscDiag::get_settings(
  mydir = bivar_directory,
  settings = list(
    base_name = glue::glue(base_model, 'est_h'),
    run = "profile",
    profile_details = profile.settings,
    exe = exe_loc,
    extras = '-nohess',
    usepar = FALSE,
    init_values_src = 0))

# set up parallel stuff - runs about 4x faster
future::plan(future::multisession(workers = parallelly::availableCores(omit = 1)))

tictoc::tic()
run_diagnostics(mydir = here('models','_bivariate_profiles'), model_settings = settings)
tictoc::toc()

# back to sequential processing
future::plan(future::sequential)


# ============================================================================ #
# Profile over h while estimating M
#use the same base model just change the parameters
copy_SS_inputs(dir.old = here('models', base_model), 
               dir.new = here('models', '_bivariate_profiles', base_model),
               overwrite = TRUE)
mod <- SS_read(here('models', '_bivariate_profiles', base_model))
#estimate M
mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('PHASE')] <- 4

SS_write(mod,
         dir = here('models', '_bivariate_profiles', base_model),
         overwrite = TRUE)

r4ss::run(dir = here('models', '_bivariate_profiles', base_model), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess', 
          show_in_console = TRUE, 
          skipfinished = FALSE)

#set up the profiles for steepness
profile.settings <- nwfscDiag::get_settings_profile(
  parameters = 'SR_BH_steep', 
  low = 0.5, 
  high = 0.95,
  step_size = 0.05,
  param_space = 'real') 

settings <- nwfscDiag::get_settings(
  mydir = bivar_directory,
  settings = list(
    base_name = base_model,
    run = "profile",
    profile_details = profile.settings,
    exe = exe_loc,
    extras = '-nohess',
    usepar = FALSE,
    init_values_src = 0))

# set up parallel stuff
future::plan(future::multisession(workers = parallelly::availableCores(omit = 1)))

tictoc::tic()
run_diagnostics(mydir = here('models','_bivariate_profiles'), model_settings = settings)
tictoc::toc()

# back to sequential processing
future::plan(future::sequential)

