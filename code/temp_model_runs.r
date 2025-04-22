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



####------------------------------------------------#
## 3_2_14_ROVandRecDomed----
####------------------------------------------------#



new_name <- "3_2_14_ROVandRecDomed"
old_name <- "3_2_1_issue63Changes"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))

##
#Make Changes
##
  mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
                                     grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                           c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, 4)


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
plot_sel_all(pp)


####------------------------------------------------#
## 3_2_15_CCFRPandRecDomed----
####------------------------------------------------#



new_name <- "3_2_15_CCFRPandRecDomed"
old_name <- "3_2_1_issue63Changes"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))

##
#Make Changes
##
  mod$ctl$size_selex_parms[intersect(grep("CCFRP", rownames(mod$ctl$size_selex_parms)),
                                     grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                           c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, 4)


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
plot_sel_all(pp)


####------------------------------------------------#
## 3_2_16_RecEarlyAsymtotic----
####------------------------------------------------#

#rec 1916-2000 asymptotic
#asymptotic last block


new_name <- "3_2_16_RecEarlyAsymtotic"
old_name <- "3_2_1_issue63Changes"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))



##
#Make Changes
##

#rec selectivity asymptotic in first and last blocks

#rec size selectivity asymptotic in each time block
mod$ctl$size_selex_parms[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms)),
                                     grep("4", rownames(mod$ctl$size_selex_parms))), 
                           c("LO", "HI", "INIT", "PHASE")] <- c(0, 10, 15, -4)

mod$ctl$size_selex_parms_tv[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms_tv)),
                                     grep("P_4", rownames(mod$ctl$size_selex_parms_tv))), 
                           c("PHASE")] <- c(4)



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
plot_sel_all(pp)


####------------------------------------------------#
## 3_2_17_SimplifyRecBlocks----
####------------------------------------------------#

#rec 1916-2000 asymptotic
#remove 2001 to 2016 blocks
#2017-2022 domed
 


new_name <- "3_2_17_SimplifyRecBlocksc"
old_name <- "3_2_1_issue63Changes"


##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models',new_name))



##
#Make Changes
##

#rec selectivity asymptotic in first and last blocks
mod$ctl$blocks_per_pattern <- c(2, 2)
mod$ctl$Block_Design <- list(c(2003, 2022, 2023, 2024), #commercial fleet
                             c(2017, 2022, 2023, 2024)) #recreational fleet
 
### Time varying selectivity table
selex_tv_pars <- dplyr::filter(selex_new, Block > 0) |>
  dplyr::select(LO, HI, INIT, PRIOR, PR_SD, PR_type, PHASE, Block) |>
  tidyr::uncount(mod$ctl$blocks_per_pattern[Block], .id = 'id', .remove = FALSE)

rownames(selex_tv_pars) <- rownames(selex_tv_pars) |>
  stringr::str_remove('\\.\\.\\.[:digit:]+') |>
  stringr::str_c('_BLK', selex_tv_pars$Block, 'repl_', mapply("[",mod$ctl$Block_Design[selex_tv_pars$Block], selex_tv_pars$id * 2 - 1))

mod$ctl$size_selex_parms_tv <- selex_tv_pars |>
  dplyr::select(-Block, -id)

#rec size selectivity asymptotic in first and last time blocks
mod$ctl$size_selex_parms[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms)),
                                     grep("4", rownames(mod$ctl$size_selex_parms))), 
                           c("LO", "HI", "INIT", "PHASE")] <- c(0, 20, 15, -4)

mod$ctl$size_selex_parms_tv[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms_tv)),
                                     grep("P_4", rownames(mod$ctl$size_selex_parms_tv))), 
                           c("PHASE")] <- c(4)



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
plot_sel_all(pp)

















#rec size selectivity
# mod$ctl$size_selex_parms[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms)),
#                                    grep("1", rownames(mod$ctl$size_selex_parms))), 
#                          c("LO", "HI", "INIT", "PHASE")] <- c(10, 50, 30, 4)

#  #mod$ctl$size_selex_parms[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms)),
#  #                                   grep("2", rownames(mod$ctl$size_selex_parms))), 
#  #                         c("LO", "HI", "INIT", "PRIOR", "PR_SD", "PHASE")] <- c(-7, 7, -1, -0.5, 2, -4)

#  mod$ctl$size_selex_parms[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms)),
#                                     grep("3", rownames(mod$ctl$size_selex_parms))), 
#                           c("LO", "HI", "INIT", "PHASE")] <- c(0, 10, 5, 4)

#  mod$ctl$size_selex_parms[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms)),
#                                     grep("4", rownames(mod$ctl$size_selex_parms))), 
#                           c("LO", "HI", "INIT", "PHASE")] <- c(0, 10, 5, 4)

#  mod$ctl$size_selex_parms[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms)),
#                                     grep("5", rownames(mod$ctl$size_selex_parms))), 
#                           c("LO", "HI", "INIT", "PHASE")] <- c(-20, 30, -20, 4)

#  mod$ctl$size_selex_parms[intersect(grep("Recreational", rownames(mod$ctl$size_selex_parms)),
#                                     grep("6", rownames(mod$ctl$size_selex_parms))), 
#                           c("LO", "HI", "INIT", "PHASE")] <- c(-10, 10, 5, 4)

#  mod$ctl$size_selex_parms[intersect(grep("Commercial", rownames(mod$ctl$size_selex_parms)),
#                                     grep("6", rownames(mod$ctl$size_selex_parms))), 
#                           c("LO", "HI", "INIT", "PHASE")] <- c(-10, 10, 5, 4)

#  mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
#                                     grep("6", rownames(mod$ctl$size_selex_parms))), 
#                           c("LO", "HI", "INIT", "PHASE")] <- c(-10, 10, 5, 4)

 # mod$ctl$size_selex_parms[intersect(grep("CCFRP", rownames(mod$ctl$size_selex_parms)),
 #                                    grep("P_4", rownames(mod$ctl$size_selex_parms))), 
 #                          c("LO", "HI", "INIT", "PHASE")] <- c(0, 10, 5, 4)

 # mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
 #                                    grep("P_4", rownames(mod$ctl$size_selex_parms))), 
 #                          c("LO", "HI", "INIT", "PHASE")] <- c(0, 10, 5, 4)



