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

  mod$ctl$size_selex_parms[intersect(grep("CCFRP", rownames(mod$ctl$size_selex_parms)),
                                     grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                           c("LO", "HI", "INIT", "PHASE")] <- c(0, 10, 5, 4)

  mod$ctl$size_selex_parms[intersect(grep("ROV", rownames(mod$ctl$size_selex_parms)),
                                     grep("P_4", rownames(mod$ctl$size_selex_parms))), 
                           c("LO", "HI", "INIT", "PHASE")] <- c(0, 10, 5, 4)
