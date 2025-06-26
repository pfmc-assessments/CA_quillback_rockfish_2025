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


###fix growth to internal and leave out ages
new_name <- 'fix_growth_external_no_ages'
old_name <- base_mod_name

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))

 #         K        Linf          L0         CV0         CV1
 #0.17840051 41.17135518  3.99192977  0.20157461  0.06413399
#This is L0 so change that in the model

mod$ctl$MG_parms$INIT[2] <-  3.99192977
mod$ctl$MG_parms$INIT[3] <- 41.17135518	
mod$ctl$MG_parms$INIT[4] <-  0.17840051	
mod$ctl$MG_parms$INIT[5] <-  0.20157461
mod$ctl$MG_parms$INIT[6] <-  0.06413399
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

xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', new_name)))))




##
#Make Changes
##

mod$ctl$blocks_per_pattern <- c(3, 2)
mod$ctl$Block_Design <- list(c(2003, 2013, 2014, 2022, 2023, 2024), #commercial fleet
                             c(2001, 2022, 2023, 2024)) #recreational fleet



### Add block indicators into selectivity table
# Block = number of block to use, Block_Fxn = 2 means replace parameters 
selex_new <- mod$ctl$size_selex_parms

selex_new[intersect(grep("Commercial", rownames(selex_new)), which(selex_new$PHASE > 0)), 
          c("Block")] <- 1
selex_new[intersect(grep("Commercial", rownames(selex_new)), which(selex_new$PHASE > 0)), 
          c("Block_Fxn")] <- 2
selex_new[intersect(grep("Recreational", rownames(selex_new)), which(selex_new$PHASE > 0)), 
          c("Block")] <- 2
selex_new[intersect(grep("Recreational", rownames(selex_new)), which(selex_new$PHASE > 0)), 
          c("Block_Fxn")] <- 2

mod$ctl$size_selex_parms <- selex_new


### Time varying selectivity table
selex_tv_pars <- dplyr::filter(selex_new, Block > 0) |>
  dplyr::select(LO, HI, INIT, PRIOR, PR_SD, PR_type, PHASE, Block) |>
  tidyr::uncount(mod$ctl$blocks_per_pattern[Block], .id = 'id', .remove = FALSE)

rownames(selex_tv_pars) <- rownames(selex_tv_pars) |>
  stringr::str_remove('\\.\\.\\.[:digit:]+') |>
  stringr::str_c('_BLK', selex_tv_pars$Block, 'repl_', mapply("[",mod$ctl$Block_Design[selex_tv_pars$Block], selex_tv_pars$id * 2 - 1))

mod$ctl$size_selex_parms_tv <- selex_tv_pars |>
  dplyr::select(-Block, -id)

#estimate param 4 in recreational time blocks

  mod$ctl$size_selex_parms_tv[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms_tv)),
                                     grep("P_4", rownames(mod$ctl$size_selex_parms_tv))), 
                           c("PHASE")] <- c(4)